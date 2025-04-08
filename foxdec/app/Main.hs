{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData, BangPatterns #-}


module Main where




import Base
import Config



import OutputGeneration.Metrics
import OutputGeneration.CallGraph
import OutputGeneration.NASM.ModDataSection
import OutputGeneration.GlobalMemAnalysis

import Data.CFG
import Data.SValue
import Data.SPointer
import Data.GlobalMem


import WithAbstractPredicates.ControlFlow
import WithAbstractSymbolicValues.Class
import WithAbstractSymbolicValues.FInit
import WithAbstractSymbolicValues.GMem
import WithAbstractPredicates.ContextSensitiveAnalysis
import WithAbstractSymbolicValues.InstanceOfAbstractPredicates
import WithNoAbstraction.SymbolicExecution
import WithAbstractSymbolicValues.SymbolicExecution

import Binary.Generic
import Binary.Read
import Data.L0
import Data.SymbolicExpression

import OutputGeneration.NASM.L0ToNASM
import OutputGeneration.NASM.NASM


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.ByteString as BS (readFile,writeFile) 
import qualified Data.Serialize as Cereal hiding (get,put)
import Data.List 
import Data.Word
import Data.Function ((&))
import qualified Data.ByteString.Lazy as B

import System.IO
import System.Process (callCommand)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Exit (die)
import System.Time.Extra (showDuration)

import Time.System
import Data.Hourglass
import Data.Int (Int64)

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Strict

import System.Environment
import System.Console.GetOpt




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
    args_help               :: Bool,       -- ^ Shall we output a help message?
    args_verbose            :: Bool        -- ^ Shall we produce verbose output?
  }
 deriving Show

-- | Default values for command-line arguments
defaultArgs = CommandLineArgs "" "" "" "" False False False False False False False

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
    when (args_help args) $ err ""
    -- check if arguments make sense 
    when (args_config args    == "") $ err "ERROR: No config file specified (missing -c)"
    when (args_dirname args   == "") $ err "ERROR: No dirname specified (missing -d)"
    when (args_filename args  == "") $ err "ERROR: No name of binary specified (missing -n)"
    when (args_inputtype args == "") $ err "ERROR: No input type specified (missing -i)"
    when (args_inputtype args `notElem` ["BINARY","L0"]) $ err $ "ERROR: input type is now set to \"" ++ show (args_inputtype args) ++ "\" but should be either the string BINARY or the string L0."
    when (args_generate_L0 args && args_inputtype args == "L0") $ err "ERROR: Cannot generate as output an L0 when input is set to L0."
    when (no_output args) $ err "ERROR: Enable at least one output-generation option from [--GL0, --Gfuncs, --Gmetrics, --Gcallgraph, --GNASM]"
    return args

  no_output args = all (not . (&) args) [args_generate_L0, args_generate_functions, args_generate_metrics, args_generate_callgraph,args_generate_NASM]

-- | The full usage message
usageMsg = usageInfo usageMsgHeader args ++ "\n" ++ usageMsgFooter
 where
  -- header
  usageMsgHeader = intercalate "\n"
    [ "The FoxDec Decompiler"
    , "USAGE: foxdec-exe -c CONFIGFILE -d DIRNAME -n FILENAME -i INPUTTYPE [OUTPUTTYPES ...]"
    , ""
    , "Provide the configuration file, the directory where the input is located, the name of the binary, and the type of desired input and output(s)."
    , "The input can be a binary, or if the binary has already been lifted, its L0 representation. stored in a .L0 file."
    , "The desired output-type(s) can be any of the -G flags (more than one can be enabled)"
    , ""
    ]
  -- footer
  usageMsgFooter = intercalate "\n"
    [ "Example usage:" 
    , "    foxdec-exe " ++ exampleArgs
    , ""
    , "When using stack, provide command-line arguments after -- :"
    , "    stack exec foxdec-exe -- " ++ exampleArgs
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
  (bin,l0) <- obtain_L0 config (args_inputtype args) (args_verbose args) dirname name
  -- 2.)
  when (args_generate_metrics args)   $ generate_metrics bin l0
  when (args_generate_L0 args)        $ serialize_l0 bin l0
  when (args_generate_callgraph args) $ generate_call_graph bin config l0
  when (args_generate_NASM args)      $ generate_NASM (bin,config,l0)
  when (args_generate_functions args) $ generate_per_function bin config l0



type Lifted = Lifting Binary (Sstate SValue SPointer) (FInit SValue SPointer) SValue



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
    --die $ "END"

    l0 <- lift_to_L0 config bin empty_finit
    putStrLn $ "Obtained L0 by lifting " ++ dirname ++ name
    endTime <- l0 `deepseq` timeCurrent
    let runningTime = fromIntegral $ timeDiff endTime startTime
    return (bin,l0 {l0_time = showDuration runningTime})





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

  createDirectoryIfMissing False dirname      

  let nasm' = lift_L0_to_NASM l

  let nasm = nasm'--split_data_section (bin,config,l0,0::Word64) nasm' -- TODO


  let gmon = __gmon_start_implementation
  writeFile   fname  $ render_NASM l nasm
  writeFile   fname1 $ gmon


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


generate_reconstruction :: Context -> IO ()
generate_reconstruction ctxt = do
  let name     = binary_file_name ctxt

  reconstruct ctxt 
--}




