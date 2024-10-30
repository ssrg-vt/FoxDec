{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}


module Main where




import Base
import Config

import Analysis.Context

import Generic.Binary

import OutputGeneration.Metrics
import OutputGeneration.CallGraph
import qualified OutputGeneration.JSON as JSON


import Algorithm.L0_Lifting

import NASM.L0ToNASM
import qualified NASM.NASMToC as C


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.ByteString as BS (readFile,writeFile) 
import qualified Data.Serialize as Cereal hiding (get,put)
import Data.List (intercalate)
import Data.Function ((&))
import Data.IORef

import System.IO
import System.Process (callCommand)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Exit (die)

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
    args_generate_json      :: Bool,       -- ^ Shall we generate a .json file?
    args_verbose_json       :: Bool,       -- ^ Must the .json include all the invariants?
    args_generate_metrics   :: Bool,       -- ^ Shall we generate a .metrics file?
    args_generate_callgraph :: Bool,       -- ^ Shall we generate an annotated call graph?
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
    , Option []        ["Gjson"]      (NoArg  set_args_generate_json)               "Generate a .json file (result is pretty-printed as well in .json.txt file)."
    , Option []        ["vjson"]      (NoArg  set_args_verbose_json)                "Make the JSON verbose by outputting all invariants (only when Gjson is enabled). Warning: this may be a large output."
    , Option []        ["Gmetrics"]   (NoArg  set_args_generate_metrics)            "Generate metrics in .metrics.txt and .metrics.json files."
    ]
 where
  set_args_config    str      args = args { args_config = str }
  set_args_dirname   str      args = args { args_dirname = str }
  set_args_filename  str      args = args { args_filename = str }
  set_args_inputtype str      args = args { args_inputtype = str }
  set_args_generate_L0        args = args { args_generate_L0 = True }
  set_args_generate_NASM      args = args { args_generate_NASM = True }
  set_args_generate_json      args = args { args_generate_json = True }
  set_args_verbose_json       args = args { args_verbose_json = True }
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
    when (not (args_generate_json args) && args_verbose_json args) $ err "ERROR: Cannot generate verbose JSON without enabling -Gjson"
    when (args_generate_L0 args && args_inputtype args == "L0") $ err "ERROR: Cannot generate as output an L0 when input is set to L0."
    when (no_output args) $ err "ERROR: Enable at least one output-generation option from [--GL0, --Gjson, --Gmetrics, --Gcallgraph, --GNASM]"
    return args

  no_output args = all (not . (&) args) [args_generate_L0, args_generate_json, args_generate_metrics, args_generate_callgraph,args_generate_NASM]

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

  -- 1.)
  ctxt <- obtain_context (args_inputtype args) (args_config args) (args_verbose args) dirname name
  -- 2.)
  when (args_generate_metrics args)   $ generate_metrics ctxt
  when (args_generate_callgraph args) $ generate_call_graph ctxt
  when (args_generate_NASM args)      $ generate_NASM ctxt
  when (args_generate_json    args)   $ generate_json ctxt $ args_verbose_json args
  when (args_generate_L0 args)        $ serialize_context ctxt




-- INPUT

-- | Obtain a context ...
-- ... by reading in a .L0 file
obtain_context "L0" configname verbose dirname name = do
  let fname = dirname ++ name ++ ".L0"
  exists <- doesFileExist fname
  if exists then do
    ctxt <- ctxt_read_L0 dirname name
    putStrLn $ "Obtained L0 from file " ++ fname
    return ctxt
  else
    die $ "File: " ++ show (dirname ++ name ++ ".L0") ++ " does not exist."
-- ... by doing binary lifting
obtain_context "BINARY" configname verbose dirname name = do
  config <- parse_config configname
  binary <- read_binary dirname name
  case binary of
    Nothing -> die $ "Cannot read binary file: " ++ dirname ++ name
    Just b  -> lift config b
 where
  lift config b = do
    startTime <- timeCurrent
    when (startTime `deepseq` verbose) $ putStrLn $ binary_pp b
    ioref <- newIORef IM.empty
    ctxt <- execStateT lift_to_L0 $! init_context b ioref config verbose dirname name
    putStrLn $ "Obtained L0 by lifting " ++ dirname ++ name
    endTime <- ctxt_ctxt_ ctxt `deepseq` timeCurrent

    let runningTime = fromIntegral $ timeDiff endTime startTime
    return $ set_ctxt_runningtime runningTime ctxt





-- OUTPUT

-- | Write a context to a .L0 file
serialize_context :: Context -> IO ()
serialize_context ctxt = do
  let dirname  = ctxt_dirname ctxt
  let name     = ctxt_name ctxt
  let fname    = dirname ++ name ++ ".L0" 
  BS.writeFile fname $ Cereal.encode $ purge_context ctxt
  putStrLn $ "Generated L0: " ++ fname 


-- | Generate metrics
generate_metrics :: Context -> IO ()
generate_metrics ctxt = do
  let dirname    = ctxt_dirname ctxt
  let name       = ctxt_name ctxt
  let fname      = dirname ++ name ++ ".metrics.txt" 
  let fname_json = dirname ++ name ++ ".metrics.json" 

  let (json,txt) = mk_metrics ctxt
  writeFile fname $ txt
  writeFile fname_json $ json
  putStrLn $ "Generated metrics: " ++ fname_json
  putStrLn $ "Generated metrics in readable plain-text: " ++ fname 

-- | Generate JSON
generate_json :: Context -> Bool -> IO ()
generate_json ctxt verbose = do
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let fname_plain = dirname ++ name ++ ".json.txt" 
  let fname_json  = dirname ++ name ++ ".json" 
  JSON.generate_json ctxt fname_plain fname_json verbose
  putStrLn $ "Generated JSON: " ++ fname_json
  putStrLn $ "Generated JSON in readable plain-text: " ++ fname_plain

-- | Generate the call graph
generate_call_graph :: Context -> IO ()
generate_call_graph ctxt = do
  let dirname  = ctxt_dirname ctxt
  let name     = ctxt_name ctxt
  let do_pdfs  = ctxt_generate_pdfs ctxt
  let fname    = dirname ++ name ++ ".calls.dot" 
  let pdfname  = dirname ++ name ++ ".calls.pdf" 
  let g        = mk_callgraph ctxt

  writeFile fname g

  if do_pdfs then do
    callCommand $ "dot -Tpdf " ++ fname ++ " -o " ++ pdfname
    putStrLn $ "Generated call graph, exported to files: " ++ fname ++ " and " ++ pdfname
  else do
    putStrLn $ "Generated call graph, exported to file: " ++ fname 

-- | Generate NASM
generate_NASM :: Context -> IO ()
generate_NASM ctxt = do
  let dirname  = ctxt_dirname ctxt ++ "nasm/"
  let name     = ctxt_name ctxt
  let fname    = dirname ++ name ++ ".asm" 
  let fname1   = dirname ++ "__gmon_start__.c" 

  createDirectoryIfMissing False dirname      

  let nasm = lift_L0_to_NASM ctxt
  let gmon = __gmon_start_implementation
  writeFile fname $ render_NASM ctxt nasm
  writeFile fname1 gmon
  putStrLn $ "Generated NASM, exported to file: " ++ fname 

  BS.writeFile (fname++".serialized") $ Cereal.encode $ nasm
  


{--
  -- TODO MAKE C OPTION
  let dirname  = ctxt_dirname ctxt ++ "C/"
  let name     = ctxt_name ctxt
  let fname1   = dirname ++ name ++ ".c" 
  let fname2   = dirname ++ name ++ ".h" 
  let (ts,ds)  = C.render_NASM ctxt nasm

  createDirectoryIfMissing False dirname      

  writeFile fname1 ts
  writeFile fname2 ds
  putStrLn $ "Generated NASM, exported to directory: " ++ dirname
--}


generate_reconstruction :: Context -> IO ()
generate_reconstruction ctxt = do
  let name     = ctxt_name ctxt

  reconstruct ctxt 
