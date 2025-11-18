{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, BangPatterns #-}


module Main where




import Base
import Config

import Binary.Disassemble

import OutputGeneration.Metrics
import OutputGeneration.CallGraph
import OutputGeneration.NASM.ModDataSection
import OutputGeneration.GlobalMemAnalysis
import OutputGeneration.NASM.Abstract_ASM
import OutputGeneration.PathGeneration
import OutputGeneration.ELLF


import Data.CFG
import Data.SValue
import Data.SPointer
import Data.GlobalMem
import Data.Indirection
import Data.X86.Register

import Binary.FunctionNames
import Data.X86.Opcode
import Data.X86.Instruction
import Data.JumpTarget
import Data.X86.Register


import WithAbstractPredicates.ControlFlow
import WithAbstractSymbolicValues.Class
import WithAbstractSymbolicValues.FInit
import WithAbstractSymbolicValues.GMem
import WithAbstractPredicates.ContextSensitiveAnalysis
import WithAbstractSymbolicValues.InstanceOfAbstractPredicates
import WithNoAbstraction.SymbolicExecution
import WithNoAbstraction.SymbolicExecutionPath
import WithNoAbstraction.Lifted
import WithAbstractSymbolicValues.SymbolicExecution

import Binary.Generic
import Binary.Read
import Data.L0
import Data.SymbolicExpression

import OutputGeneration.NASM.L0ToNASM
import OutputGeneration.NASM.NASM

import Algorithm.Graph


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.ByteString as BS (readFile,writeFile) 
import qualified Data.Serialize as Cereal hiding (get,put)
import Data.List 
import Data.Word
import Data.Maybe (catMaybes,fromJust)
import Data.Function ((&))
import qualified Data.ByteString.Lazy as B

import System.IO
import System.Process (callCommand)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Exit (die,exitSuccess)
import System.Time.Extra (showDuration)

import Time.System
import Data.Hourglass
import Data.Int (Int64)

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Extra

import System.Environment
import System.Console.GetOpt

import Debug.Trace

-- | The command-line arguments
data CommandLineArgs = CommandLineArgs
  { 
    args_config             :: FilePath,   -- ^ The .config file
    args_dirname            :: FilePath,   -- ^ The directory where results are stored
    args_filename           :: FilePath,   -- ^ The name of the binary
    args_inputtype          :: String,     -- ^ The input type (a binary or a .L0 file)
    args_generate_L0        :: Bool,       -- ^ Shall we generate a .L0 file?
    args_generate_NASM      :: Bool,       -- ^ Shall we generate NASM?
    args_generate_functions :: Bool,       -- ^ Shall we generate CGs?
    args_generate_metrics   :: Bool,       -- ^ Shall we generate a .metrics file?
    args_generate_callgraph :: Bool,       -- ^ Shall we generate an annotated call graph?
    args_generate_ellf      :: Bool,       -- ^ Shall we generate a lifted ELLF?
    args_help               :: Bool,       -- ^ Shall we output a help message?
    args_verbose            :: Bool        -- ^ Shall we produce verbose output?
  }
 deriving Show

-- | Default values for command-line arguments
defaultArgs = CommandLineArgs "" "" "" "" False False False False False False False False

-- | The command-line arguments and their types
args =
    [ Option ['h','?'] ["help"]       (NoArg  set_args_help)                        "Provide help message and exit (overrides all other options)."
    , Option ['c']     ["config"]     (ReqArg set_args_config      "CONFIGFILE")    "The config file with .dhall extension."
    , Option ['d']     ["dir"]        (ReqArg set_args_dirname     "DIRNAME")       "Directory to use."
    , Option ['n']     ["name"]       (ReqArg set_args_filename    "FILENAME")      "Name of the binary."
    , Option ['i']     ["input"]      (ReqArg set_args_inputtype   "INPUTTYPE")     "Either the string BINARY or the string L0."
    , Option ['v']     ["verbose"]    (NoArg  set_args_verbose)                     "If enabled, produce verbose output."
    , Option []        ["GL0"]        (NoArg  set_args_generate_L0)                 "Generate a .L0 file (only if INPUTTYPE==BINARY)."
    , Option []        ["Gcallgraph"] (NoArg  set_args_generate_callgraph)          "Generate an annotated call graph."
    , Option []        ["GNASM"]      (NoArg  set_args_generate_NASM)               "Generate NASM"
    , Option []        ["Gfuncs"]     (NoArg  set_args_generate_functions)          "Generate per function a control flow graph (CFG) and information."
    , Option []        ["Gmetrics"]   (NoArg  set_args_generate_metrics)            "Generate metrics in .metrics.txt file."
    , Option []        ["Gellf"]      (NoArg  set_args_generate_ellf)               "Generate a lifted representation of an ELLF in .S file."
    ]
 where
  set_args_config    str      args = args { args_config = str }
  set_args_dirname   str      args = args { args_dirname = str }
  set_args_filename  str      args = args { args_filename = str }
  set_args_inputtype str      args = args { args_inputtype = str }
  set_args_generate_L0        args = args { args_generate_L0 = True }
  set_args_generate_NASM      args = args { args_generate_NASM = True }
  set_args_generate_functions args = args { args_generate_functions = True }
  set_args_generate_metrics   args = args { args_generate_metrics = True }
  set_args_generate_callgraph args = args { args_generate_callgraph = True }
  set_args_generate_ellf      args = args { args_generate_ellf = True }
  set_args_help               args = args { args_help = True }
  set_args_verbose            args = args { args_verbose = True }

-- | Parse argv into a CommandLineArgs 
parseCommandLineArgs :: [String] -> IO CommandLineArgs
parseCommandLineArgs argv =
  case getOpt Permute args argv of
    (o,[],[])  -> inputChecking $ foldl (flip id) defaultArgs o
    (o,n,[])   -> err $ "ERROR: invalid argument(s) " ++ show n
    (_,_,errs) -> err $ "ERROR: " ++ concat errs 
 where
  -- check validity of command-line arguments
  inputChecking args = do
    -- when help is requested, output usage message and exit
    when (args_help args) $ putStrLn usageMsg >> exitSuccess
    -- check if arguments make sense 
    when (args_config args    == "") $ err "ERROR: No config file specified (missing -c)"
    when (args_dirname args   == "") $ err "ERROR: No dirname specified (missing -d)"
    when (args_filename args  == "") $ err "ERROR: No name of binary specified (missing -n)"
    when (args_inputtype args == "") $ err "ERROR: No input type specified (missing -i)"
    when (args_inputtype args `notElem` ["BINARY","L0", "ELLF"]) $ err $ "ERROR: input type is now set to \"" ++ show (args_inputtype args) ++ "\" but should be either the string BINARY or the string L0 or the string ELLF."
    when (args_generate_L0 args   && args_inputtype args == "L0") $ err "ERROR: Cannot generate as output an L0 when input is set to L0."
    when (args_generate_ellf args && args_inputtype args == "L0") $ err "ERROR: Cannot generate as output a lifted ELLF when input is set to L0."
    
    when (no_output args) $ err "ERROR: Enable at least one output-generation option from [--GL0, --Gfuncs, --Gmetrics, --Gcallgraph, --GNASM, --Gellf]"
    when (not $ validate_ellf args) $ err "ERROR: for reading and lifting an ELLF, set input type to ELLF and enable only the -G option -Gellf"
    
    return args

  no_output args = all (not . (&) args) [args_generate_L0, args_generate_functions, args_generate_metrics, args_generate_callgraph,args_generate_NASM,args_generate_ellf]
  validate_ellf args = if args_generate_ellf args || args_inputtype args == "ELLF" then args_generate_ellf args && args_inputtype args == "ELLF" && all (not . (&) args) [args_generate_L0, args_generate_functions, args_generate_metrics, args_generate_callgraph,args_generate_NASM] else True

-- | The full usage message
usageMsg = usageInfo usageMsgHeader args ++ "\n" ++ usageMsgFooter
 where
  -- header
  usageMsgHeader = intercalate "\n"
    [ "The FoxDec Decompiler"
    , "USAGE: ./run_foxdec -c CONFIGFILE -d DIRNAME -n FILENAME -i INPUTTYPE [OUTPUTTYPES ...]"
    , ""
    , "Provide the configuration file, the directory where the input is located, the name of the binary, and the type of desired input and output(s)."
    , "The input can be a binary, or if the binary has already been lifted, its L0 representation. stored in a .L0 file."
    , "The desired output-type(s) can be any of the -G flags (more than one can be enabled)"
    , ""
    ]
  -- footer
  usageMsgFooter = intercalate "\n"
    [ "Example usage:" 
    , "    ./run_foxdec " ++ exampleArgs
    ]
  exampleArgs = "-c ./config/config.dhall -d examples/wc_small/ -n wc -i BINARY --Gmetrics"

-- | Exit unsuccesfully with an error message
err msg = die $ msg ++ "\n\n" ++ usageMsg

-- | Main: run the command-line arguments parser, and start
main = getArgs >>= parseCommandLineArgs >>= start


-- | Start
-- 1.) obtain a context, either through analyzing a binary or by reading in a .L0 file
-- 2.) use context for generating output
start :: CommandLineArgs -> IO ()
start args = do
  let dirname  = if last (args_dirname args) == '/' then args_dirname args else args_dirname args ++ "/"
  let name     = args_filename args

  config <- parse_config $ args_config args
  -- 1.)
  (bin,l0') <- obtain_L0 config (args_inputtype args) (args_verbose args) dirname name

  l0 <- return l0'
        -- >>= try_resolve_indirections_underapproximatively bin config
        -- >>= lift_to_L0 config bin empty_finit . l0_indirections
        -- >>= try_resolve_indirections_underapproximatively bin config
  --get_function_signatures bin config l0

  -- 2.)
  when (args_generate_metrics args)   $ generate_metrics bin l0
  when (args_generate_L0 args)        $ serialize_l0 bin l0
  when (args_generate_callgraph args) $ generate_call_graph bin config l0
  when (args_generate_NASM args)      $ generate_NASM (bin,config,l0)
  when (args_generate_functions args) $ generate_per_function bin config l0
  when (args_generate_ellf args)      $ read_and_lift_ellf bin







get_function_signatures bin config l0 = do
  let l       = (bin,config,l0)
  let dirname = binary_dir_name bin
  let name    = binary_file_name bin
  let entries = IS.toList $ IS.fromList $ map fst $ filter (\(entry,name) -> entry `elem` (IM.keys $ l0_functions l0)) $ binary_get_exported_functions bin 
  --let entries = map (\entry -> (entry, function_name_of_entry bin $ fromIntegral entry)) $ IM.keys $ l0_functions l0
  let fname   = dirname ++ name ++ ".signs"
  let fname2  = dirname ++ name ++ ".signs.tsv"


  signs <- mapM (get_function_signature) entries
  writeFile fname  $ intercalate "\n" $ map fst signs
  writeFile fname2 $ intercalate "\n" $ map snd signs
  putStrLn $ "Exported function signatures to plain-text file: " ++ fname
 where
  get_function_signature entry = do
    putStrLn $ "Entry: " ++ showHex entry
    
    let l = (bin,config,l0)
    let paths = S.toList $ function_to_paths l entry

    --putStrLn $ "Number of paths: " ++ show (length paths)

    results <- mapM (\path -> runStateT (symb_exec_transiting_path l path) init_symstate) paths
    let inputs = S.unions $ map (\((sems,ras,inputs),symstate) -> inputs) results
    let input_stateparts = S.toList $ S.unions [S.unions $ S.map toInputRegs inputs, toInputMem inputs 8]
    let (systemV_ABI_params,systemV_ABI_count) = match_with_systemV_ABI input_stateparts
    let aliasses = map snd $ filter (\(a,f) -> a == entry) $ binary_get_exported_functions bin

    let str0  = "0x" ++ showHex entry ++ ": " ++ intercalate "," aliasses
    let str1  = "  Exploring " ++ show (length paths) ++ " paths"
    let str2  = "  Read state parts:              " ++ show input_stateparts
    let str3  = "  System V ABI read state parts: " ++ show systemV_ABI_params
    let str4  = "  System V ABI argcount:         " ++ show systemV_ABI_count
    let signs = intercalate "\n" [str0,str1,str2,str3,str4]

    let str0   = binary_file_name bin
    let str1   = intercalate "," aliasses
    let str2   = show systemV_ABI_count
    let signs2 = intercalate "\t" [str0,str1,str2]   
    
    --mapM_ (\(path,((sems,ras,inputs),symstate)) -> putStrLn $ show path ++ "\n" ++ show inputs ++ "\n\n") $ zip paths results
    return (signs,signs2)

  toInputRegs (SP_Reg r)
    | real_reg r `notElem` [Reg64 RSP,RegSeg FS] = S.singleton $ SP_Reg r
    | otherwise = S.empty
  toInputRegs (SP_Mem a si) = S.unions $ map (toInputRegs . SP_Reg) $ regs_of_expr a


  toInputMem inputs offset =
    case find (isAboveStackFrame offset) inputs of
      Nothing -> S.empty
      Just sp -> S.insert sp $ toInputMem inputs $ offset + fromIntegral (size_of_mem_statepart sp)

  isAboveStackFrame offset (SP_Mem (SE_Op Plus _ [SE_Var (SP_Reg (Reg64 RSP)), SE_Immediate imm]) _) = offset == imm
  isAboveStackFrame offset _ = False

  size_of_mem_statepart (SP_Mem a si) = si


match_with_systemV_ABI input_stateparts = 
  let params = reg_inputs ++ mem_inputs in
    (params, count params)
 where
  reg_inputs = takeWhile (\sp -> sp `elem` input_stateparts) systemV_ABI_regs
  mem_inputs 
    | SP_Reg (Reg64 R9) `elem` input_stateparts = filter isMem input_stateparts
    | otherwise = []

  systemV_ABI_regs = map SP_Reg $ map Reg64 [ RDI, RSI, RDX, RCX, R8, R9] ++ map Reg128 [0..7]
  isMem (SP_Mem _ _) = True
  isMem sp = False

  isReg (SP_Reg _) = True
  isReg _ = False

  count params
    | SP_Reg (Reg64 R9) `elem` params && all (\i -> SP_Reg (Reg128 i) `elem` params) [0..7] = Variadic
    | SP_Reg (Reg64 R9) `elem` params = Argcount $ 6 + length (filter isMem params)
    | otherwise = Argcount $ length (filter isReg params)


-- TODO: move to own file
try_resolve_indirections_underapproximatively bin config l0 = do
  let unresolved_indirections = zip [0 ..] $ IM.keys $ IM.filter isUnresolved $ l0_indirections l0
  foldM (try_resolve_underapproximatively (length unresolved_indirections)) l0 unresolved_indirections
 where
  isUnresolved inds = all ((==) Indirection_Unresolved) inds

  try_resolve_underapproximatively total_num l0 (curr,a) = do
    putStrLn $ "Address: " ++ showHex a ++ " (" ++ show (curr+1)++ "/" ++ show total_num ++ ")"
    let l = (bin,config,l0)
    let (entry,paths') = generate_transiting_paths_to_address l (Just 2) a
    let paths = S.toList paths'

    --putStrLn $ "Length paths: " ++ show (sum $ map transiting_path_length paths)
    symstates <- mapM (\path -> execStateT (symb_exec_transiting_path l $ transiting_path_take_last 50 path) init_symstate) paths
    trgts <- mapM (do_path l0 entry a) $ zip paths symstates

    case S.toList $ S.unions $ catMaybes trgts of
      [] -> return l0
      inds -> do
        let ind = S.fromList $ map Indirection_Resolved inds
        runReaderT (execStateT (add_newly_resolved_indirection ind a) l0) (bin,config)

  do_path l0 entry a (path,symstate) = do
    let l      = (bin,config,l0,fromIntegral entry)
    Just i    <- fetch_instruction bin $ fromIntegral a
    let sem    = instr_to_semantics l i
    let ras    = resolve_operands l sem symstate
    let [SE_StatePart op Nothing] = operands_of sem
    let trgts  = (ctry_jump_targets l i . SConcrete . neFromList) $ (ras M.! op)
    --putStrLn $ show path
    --putStrLn $ show sem
    --putStrLn $ show op ++ " == " ++ show (ras M.! op)
    --putStrLn $ show trgts
    return trgts




-- INPUT

-- | Obtain L0 ...
-- ... by reading in a .L0 file
obtain_L0 :: Config -> String -> Bool -> String -> String -> IO (Binary,L0 (Sstate SValue SPointer) (FInit SValue SPointer) SValue)
obtain_L0 config "L0" verbose dirname name = do
  let fname = dirname ++ name ++ ".L0"
  exists <- doesFileExist fname
  if exists then do
    rcontents <- BS.readFile (dirname ++ name ++ ".L0")
    bcontents <- read_binary dirname name
    case (Cereal.decode rcontents, bcontents) of
      (Left err,_)        -> die $ "Could not read L0 in file " ++ (dirname ++ name ++ ".L0") ++  "\n" ++ show err
      (_,Nothing)         -> die $ "Cannot read binary file: " ++ dirname ++ name
      (Right l0,Just bin) -> do
        putStrLn $ "Obtained L0 from file " ++ fname
        return (bin,l0)
  else
    die $ "File: " ++ show (dirname ++ name ++ ".L0") ++ " does not exist."
-- ... by doing binary lifting
obtain_L0 config "BINARY" verbose dirname name = do
  binary <- read_binary dirname name
  case binary of
    Nothing -> die $ "Cannot read binary file: " ++ dirname ++ name
    Just b  -> lift config b
 where
  lift !config !bin = do
    startTime <- timeCurrent
    when (startTime `deepseq` verbose) $ putStrLn $ binary_pp bin

    --putStrLn $ show $ fetch_instruction bin 0x2b40e

    l0 <- lift_to_L0 config bin empty_finit IM.empty
    putStrLn $ "Obtained L0 by lifting " ++ dirname ++ name
    endTime <- l0 `deepseq` timeCurrent
    let runningTime = fromIntegral $ timeDiff endTime startTime
    return (bin,l0 {l0_time = showDuration runningTime})
-- ... by reading ELLF metafata
obtain_L0 config "ELLF" verbose dirname name = do
  binary <- read_binary dirname name
  case binary of
    Nothing -> die $ "Cannot read binary file: " ++ dirname ++ name
    Just b  -> lift config b
 where
  lift !config !bin = do
    putStrLn $ binary_pp bin
    putStrLn $ "Obtained L0 by reading ELLF metadata " ++ dirname ++ name
    let empty_l0 = L0 IM.empty IM.empty empty_gmem_structure ""
    return (bin,empty_l0)




-- OUTPUT

-- | Write a context to a .L0 file
serialize_l0 bin l0 = do
  let dirname  = binary_dir_name bin
  let name     = binary_file_name bin
  let fname    = dirname ++ name ++ ".L0" 
  BS.writeFile fname $ Cereal.encode l0
  putStrLn $ "Generated L0: " ++ fname 


-- | Generate metrics
generate_metrics bin l0 = do
  let dirname    = binary_dir_name bin
  let name       = binary_file_name bin
  let fname      = dirname ++ name ++ ".metrics.txt" 

  let metrics = mk_metrics bin l0
  putStrLn $ metrics
  writeFile fname $ metrics
  putStrLn $ "Generated metrics in plain-text file: " ++ fname 

-- | Generate information per function
generate_per_function :: BinaryClass bin => bin -> Config -> L0 (Sstate SValue SPointer) (FInit SValue SPointer) SValue -> IO ()
generate_per_function bin config l0 = do
  let dirname     = binary_dir_name bin
  let name        = binary_file_name bin

  mapM_ (write_function dirname name) $ IM.assocs $ l0_functions l0
  putStrLn $ "Generated CFGs in: " ++ dirname ++ "functions/"


  --let regions = analyze_gmem (bin,config,l0,0::Word64) $ map (gmem . l0_lookup_join l0) $ S.toList $ l0_get_function_entries l0
  --putStrLn $ "Joined global memory:\n" ++ show_gmem joined_gmem joined_gmem_structure 
  --putStrLn $ "Regions:\n" ++ (intercalate "\n" (map show_region_info $ IM.assocs regions))
 where
  write_function dirname name (entry,(finit,Just r@(FResult cfg post join calls vcs pa))) = do
    let fdirname = dirname ++ "functions/0x" ++ showHex entry ++ "/"
    createDirectoryIfMissing False $ dirname ++ "functions/"
    createDirectoryIfMissing False $ fdirname      

    let fname  = fdirname ++ name ++ ".dot"
    writeFile fname $ cfg_to_dot bin r

    let fname2 = fdirname ++ name ++ ".txt"
    writeFile fname2 $ show_report entry finit r
  show_report entry finit (FResult cfg post join calls vcs pa) = intercalate "\n" 
    [ "Function: " ++ showHex entry
    , mk_function_boundary entry cfg
    , if show finit == "" then "" else "Precondition:\n" ++ pp_finitC finit ++ "\n"
    , "Postcondition: function " ++ show post
    , if S.null calls then "" else "Dangling function pointers: " ++ showHex_list (map fromIntegral $ S.toList $ calls) -- TODO rest
    ]
   --TODO move to own file

mk_function_boundary entry cfg =
  let addresses = concat $ IM.elems $ cfg_blocks cfg in
    intercalate "\n" $ map show_chunk $ mk_consecutive_chunks addresses
 where
  show_chunk [i]   = showHex i ++ " (single instruction)"
  show_chunk chunk = showHex (head chunk) ++ "-->" ++ showHex (last chunk)

  mk_consecutive_chunks :: [Int] -> [[Int]]
  mk_consecutive_chunks = split_consecutives . sort

  split_consecutives :: [Int] -> [[Int]]
  split_consecutives []         = []
  split_consecutives [i]        = [[i]]
  split_consecutives (i0:i1:is) = if i1 < i0 + 16 then add_to_hd i0 $ split_consecutives (i1:is) else [i0] : split_consecutives (i1:is)


  add_to_hd :: Int -> [[Int]] -> [[Int]]
  add_to_hd i []       = [[i]]
  add_to_hd i (is:iss) = (i:is) : iss

-- | Generate the call graph
generate_call_graph bin config l0 = do
  let dirname   = binary_dir_name bin
  let name      = binary_file_name bin
  let do_pdfs   = generate_pdfs config
  let fname     = dirname ++ name ++ ".callgraph.dot" 
  let pdfname   = dirname ++ name ++ ".callgraph.pdf" 
  let (g,fptrs) = mk_callgraph (bin,config,l0)
  let dot       = callgraph_to_dot (bin,config,l0) pp_finitC g fptrs

  writeFile fname dot
  if do_pdfs then do
    callCommand $ "dot -Tpdf " ++ fname ++ " -o " ++ pdfname
    putStrLn $ "Generated call graph, exported to files: " ++ fname ++ " and " ++ pdfname
  else do
    putStrLn $ "Generated call graph, exported to file: " ++ fname 

  --let g'     = graph_mk_subgraph (graph_traverse_upwards g 0x31be0) g
  --let fptrs' = graph_mk_subgraph (graph_traverse_upwards fptrs 0x31be0) fptrs
  --let dot    = callgraph_to_dot (bin,config,l0) pp_finitC g' fptrs'
  --putStrLn $ show g'
  --writeFile (dirname ++ name ++ ".callgraph.sub.dot") dot


-- | Generate NASM
generate_NASM :: Lifted -> IO ()
generate_NASM l@(bin,config,l0) = do
  let dirname  = binary_dir_name bin ++ "nasm/"
  let name     = binary_file_name bin
  let fname    = dirname ++ name ++ ".asm" 
  let fname1   = dirname ++ "__gmon_start__.c" 
  let fname2   = dirname ++ name ++ ".abstract.asm" 

  createDirectoryIfMissing False dirname      

  let nasm' = lift_L0_to_NASM l

  let nasm = nasm'--split_data_section (bin,config,l0,0::Word64) nasm' -- TODO


  let gmon = __gmon_start_implementation
  writeFile   fname  $ render_NASM l nasm
  writeFile   fname1 $ gmon
  --writeFile   fname2 $ ai_show_NASM l nasm


  putStrLn $ "Generated NASM, exported to files: " ++ fname

  





{--
  -- TODO MAKE C OPTION
  let dirname  = binary_dir_name ctxt ++ "C/"
  let name     = binary_file_name ctxt
  let fname1   = dirname ++ name ++ ".c" 
  let fname2   = dirname ++ name ++ ".h" 
  let (ts,ds)  = C.render_NASM ctxt nasm

  createDirectoryIfMissing False dirname      

  writeFile fname1 ts
  writeFile fname2 ds
  putStrLn $ "Generated NASM, exported to directory: " ++ dirname
--}


symbolically_execute_paths bin config l0 = do
  symb_exec_all_entries bin config l0




