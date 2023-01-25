{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, DeriveGeneric, StandaloneDeriving, StrictData #-}


module JSON where


import Base

import Data.SymbolicExpression
import Data.SPointer

import Generic.SymbolicConstituents
import Generic.Binary

import Instantiation.SymbolicPropagation

import Analysis.Pointers 
import Analysis.ControlFlow
import qualified Analysis.Context as C


import qualified X86.Instruction as X86
import qualified X86.Operand
import X86.Opcode
import X86.Prefix
import X86.Register
import Data.JSON_Taxonomy

import Generic.HasSize (HasSize(sizeof))

import Generic.Instruction (GenericInstruction(..))
import qualified Generic.Operand


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable
import Data.Word
import Data.IORef


import System.Exit (die)
import Control.Monad.State.Strict
import Data.Functor.Identity
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)

import qualified Data.ByteString as BS  
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as BSC (putStrLn) 
import qualified Data.Serialize as Cereal hiding (get,put)

import Data.Aeson
import GHC.Generics
import Debug.Trace






-- | Main function
-- Read in first command-line argument, check whether it is a .report file.
-- Read the file, and use the "VerificationReportInterface" to get an overview of the instructions.
main = do
  args <- getArgs
  if length args /= 4 || args!!2 `notElem` ["Metrics", "FullHoareGraph"] || args!!3 `notElem` ["Pretty", "JSON"]  then
    putStrLn $ usageMsg
  else do
    let pp = args!!3 == "Pretty"
    let dirname = if last (args!!0)  == '/' then args!!0 else args!!0 ++ "/"
    exists <- doesFileExist $ dirname ++ args!!1 ++ ".report"
    if exists then do
      ctxt <- C.ctxt_read_report dirname (args !! 1)
      if args!!2 == "Metrics" then
        ctxt_generate_metrics ctxt pp
      else
        ctxt_generate_json ctxt pp
    else
      putStrLn $ "File: " ++ show (dirname ++ args!!1 ++ ".report") ++ " does not exist."


usageMsg = intercalate "\n" [
  "Usage:",
  "",
  "  foxdec-json-exe DIRNAME NAME TOPIC TYPE",
  "",
  "Here NAME refers to the NAME used when running foxdec-exe and DIRNAME contains a file NAME.report.",
  "",
  "TOPIC is either:",
  "  Metrics         =  generate metrics",
  "  FullHoareGraph  =  generate the entire Hoare Graph",
  "",
  "TYPE is either:",
  "  Pretty    =  generate humanly readable output",
  "  JSON      =  generate JSON"
 ]




metrics = M.fromList [
  ("#instructions",         "number of covered instructions"),
  ("avgInstructionSize",    "average instruction size"),
  ("#expectedInstructions", "expected number of instructions"),
  ("%instructionCoverage",  "estimate of percentage of covered instructions"),
  ("#memWrites",            "total number of instructions writing to memory"),
  ("pointerDesignations",   "(C+LGH, C+O, NC+LH, NC+O, U)"),
  ("specifityMetric",       "weighted mean of pointer designations")
 ]

metrics_metric_size = maximum $ map length $ M.keys metrics
metrics_descr_size = maximum $ map length $ M.elems metrics


round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2



intDiv :: (Integral a,Integral b) => a -> b -> Double
x `intDiv` y = fromIntegral x / fromIntegral y

data Metric = Metric {
  metrics_name  :: String, -- ^ Name of the metric
  metrics_desc  :: String, -- ^ Description of the metric
  metrics_value :: String  -- ^ Serialized value of the metric
 }



ctxt_generate_metrics ctxt pp = do
  let instrs     = S.toList $ ctxt_get_instructions ctxt

  let avg_size = average $ map sizeof instrs
  let text_section_size = binary_text_section_size $ C.ctxt_binary ctxt
  let num_expected_intrs = floor (fromIntegral text_section_size / avg_size)
  let num_intrs = length instrs
  let instructionCoverage = round2dp (fromIntegral num_intrs / fromIntegral num_expected_intrs * 100)
  let mem_ops = ctxt_resolve_mem_operands ctxt
  let pointerDesignations = mk_metric_pointerDesignations ctxt mem_ops 
  let pointerDesignationsPercentages = designations_to_percentages pointerDesignations
  let memWrites = sum $ M.elems pointerDesignations
  let specifityMetric = specifityMetricOf pointerDesignationsPercentages


  let metrics = map mk_metric [
                 ("#instructions", show num_intrs),
                 ("avgInstructionSize", show avg_size),
                 ("#expectedInstructions", show num_expected_intrs),
                 ("%instructionCoverage", show instructionCoverage),
                 ("#memWrites", show memWrites),
                 ("pointerDesignations", show pointerDesignationsPercentages),
                 ("specifityMetric", show specifityMetric)
                ]


  mapM_ (putStrLn . show_metric) metrics
 where
  mk_metric (name,value) = Metric name (metrics M.! name) value


  show_metric (Metric name desc value) =
    let str0 = name ++ replicate (metrics_metric_size - length name) ' '
        str1 = str0 ++ " (" ++ desc ++ ")" in
      str1 ++ replicate (metrics_descr_size - length desc) ' ' ++ " = " ++ value

  designations_to_percentages :: M.Map String Int -> M.Map String Double
  designations_to_percentages m = M.map (\v -> mk_percentage v $ sum $ M.elems m) m
  
  mk_percentage x y = round2dp (x `intDiv` y * 100)


mk_metric_pointerDesignations :: C.Context -> [(Word64,Word64, [Maybe SPointer])] -> M.Map String Int
mk_metric_pointerDesignations ctxt = foldr (M.adjust ((+) 1)) init_m . concatMap get_specifity_per_instruction
 where
  init_m = M.fromList [("C+LGH",0), ("C+O", 0), ("NC+LH",0), ("NC+O",0), ("U",0)]

  get_specifity_per_instruction (entry,a,es) = map (get_domains entry) es

  get_domains entry Nothing = "Nothing"
  get_domains entry (Just e) = 
    let fctxt = C.mk_fcontext ctxt (fromIntegral entry) in
       get_pointer_specifity_cpointer fctxt e


get_pointer_domain_cpointer fctxt v = exprs_to_domain $ spointer_to_exprs fctxt v
 where
  exprs_to_domain es
    | S.null es = "U"
    | all (not . contains_bot) es = "C"
    | all is_base $ S.map (get_pointer_domain fctxt) es = "B"
    | otherwise = "S"
  is_base (Just (C.Domain_Bases _)) = True
  is_base _ = False



-- "C+LGH" = Concrete and local,global,heap
-- "C+O"   = Concrete and otherwise
-- "NC+LH" = Not concrete, and local or heap
-- "NC+O"  = Not concrete, but sources are known
-- "U"     = Unresolved
get_pointer_specifity_cpointer fctxt  v = exprs_to_specificity $ spointer_to_exprs fctxt v
 where
  exprs_to_specificity es
    | S.null es                                          = "U"
    | all (not . contains_bot) es                        = concrete es
    | all (expr_is_highly_likely_local_pointer fctxt) es = "NC+LH"
    | all (expr_is_highly_likely_heap_pointer fctxt) es  = "NC+LH"
    | otherwise                                          = "NC+O"
  concrete es
    | all (expr_is_highly_likely_local_pointer fctxt) es = "C+LGH"
    | all (expr_is_global_immediate $ C.f_ctxt fctxt) es = "C+LGH"
    | all (expr_is_highly_likely_heap_pointer fctxt) es  = "C+LGH"
    | otherwise                                          = "C+O"


specifityMetricOf :: M.Map String Double -> Double
specifityMetricOf m = m M.! "C+LGH" + m M.! "C+O" + m M.! "NC+LH" + 0.3*(m M.! "NC+O")












-- | First retrieve all instruction addresses, and for each address get the instruction.
ctxt_generate_json :: C.Context -> Bool -> IO ()
ctxt_generate_json ctxt pp = do
  let entries = ctxt_get_function_entries ctxt


  let instrs     = S.toList $ ctxt_get_instructions ctxt
  let addresses =  S.toList $ ctxt_get_instruction_addresses ctxt
  control_flow  <- mapM (ctxt_get_controlflow ctxt) addresses
  let boundaries = map  (ctxt_mk_function_boundary ctxt) $ S.toList entries
  let summaries  = map  (ctxt_get_function_summary ctxt) $ S.toList entries
  let invs       = map  (ctxt_get_inv              ctxt) $ addresses
  let mem_ops    = ctxt_resolve_mem_operands ctxt


  if pp then do
    putStrLn $ "------------"
    putStrLn $ "INSTRUCTIONS"
    putStrLn $ "------------"
    putStrLn $ intercalate "\n" $ map (C.pp_instruction ctxt) instrs
    putStrLn $ "\n\n\n"
    putStrLn $ "------------"
    putStrLn $ "CONTROL FLOW"
    putStrLn $ "------------"
    putStrLn $ pp_control_flow control_flow
    putStrLn $ "\n\n\n"
    putStrLn $ "-------------------"
    putStrLn $ "FUNCTION BOUNDARIES"
    putStrLn $ "-------------------"
    putStrLn $ pp_boundaries boundaries
    putStrLn $ "\n\n\n"
    putStrLn $ "------------------"
    putStrLn $ "FUNCTION SUMMARIES"
    putStrLn $ "------------------"
    putStrLn $ pp_summaries summaries
    putStrLn $ "\n\n\n"
    putStrLn $ "----------"
    putStrLn $ "INVARIANTS"
    putStrLn $ "----------"
    putStrLn $ pp_invs invs
    putStrLn $ "\n\n\n"
    putStrLn $ "---------------"
    putStrLn $ "POINTER DOMAINS"
    putStrLn $ "---------------"
    putStrLn $ pp_mem_ops ctxt mem_ops
  else do
    let json_instructions = map mk_json_instruction instrs
    let json_summaries = map (\(a,(finit,post)) -> (a,FunctionSummary (M.mapKeys SP_Reg finit) $ mk_json_post post)) summaries
    let json_invs = map (\(a,Just invs) -> (a,map (\(entry,inv) -> (entry,mk_json_predicate inv)) invs)) $ filter (((/=) Nothing) . snd) invs
    let json = JSON json_instructions control_flow boundaries json_summaries json_invs mem_ops
    BSC.putStrLn $ toStrict $ encode json








      


pp_invs = intercalate "\n\n" . map pp
 where
  pp (a,Nothing)   = "Address " ++ showHex a ++ ": no invariant."
  pp (a,Just invs) = "Address " ++ showHex a ++ ":\n" ++ intercalate "\n\n\n" (map pp_inv invs)
  pp_inv (entry,inv) = "Entry: " ++ showHex entry ++ ":\n" ++ show inv

pp_control_flow :: [(Word64,[Word64])] -> String
pp_control_flow = intercalate "\n" . map pp
 where
  pp (a,as) = showHex a ++ " --> " ++ showHex_list as

pp_boundaries :: [(Word64,String)] -> String
pp_boundaries = intercalate "\n" . map pp
 where
 pp (a,s) = showHex a ++ ": " ++ map repl s 
 repl '\n' = ';'
 repl c    = c

pp_summaries = intercalate "\n\n" . map pp
 where
  pp (a,(finit,post)) = "Entry: " ++ showHex a ++ "\n" ++ pp_finit finit ++ "\n" ++ pp_post post

  pp_post (Just C.UnknownRetBehavior) = "Unknown return behavior"
  pp_post (Just C.Terminating)        = "Terminal"
  pp_post (Just (C.ReturningWith q))  = "Returns with postcondition:\n" ++ show q 

  pp_finit finit = if M.null finit then "No precondition" else "Precondition:\n" ++ show finit


pp_mem_ops ctxt = intercalate "\n" . map pp
 where
  pp (entry,a,es) = "Address " ++ showHex a ++ ", entry: " ++ showHex entry ++ ": [" ++ intercalate "," (map pp_expr_option es) ++ "] " ++ concatMap (pp_dom entry) es

  pp_expr_option Nothing = "_"
  pp_expr_option (Just e) = show e

  pp_dom entry Nothing = "_"
  pp_dom entry (Just e) =
    let fctxt = C.mk_fcontext ctxt (fromIntegral entry) in
      get_pointer_domain_cpointer fctxt e





ctxt_get_instructions ctxt =
  let entries = ctxt_get_function_entries ctxt
      cfgs    = map (\entry -> IM.lookup entry (C.ctxt_cfgs ctxt)) $ S.toList entries
      instrs  = map (\(Just cfg) -> concat $ IM.elems $ C.cfg_instrs cfg) cfgs in
    if length entries /= IM.size (C.ctxt_cfgs ctxt) then
      error $ show (length entries, length cfgs)
    else
      S.unions $ map S.fromList instrs

ctxt_disassemble_address :: C.Context -> Word64 -> IO (Instruction,String)
ctxt_disassemble_address ctxt a = do
  let entries = ctxt_get_function_entries ctxt
      cfgs    = map (\entry -> (entry,IM.lookup entry (C.ctxt_cfgs ctxt))) $ S.toList entries
      instrs  = map (\(entry,Just cfg) -> (entry,concat $ IM.elems $ C.cfg_instrs cfg)) cfgs


  i <- ctxt_get_instruction ctxt $ fromIntegral a
  return $ (mk_json_instruction i, C.pp_instruction ctxt i)


ctxt_get_function_summary ctxt entry =
  let finit = C.ctxt_finits ctxt IM.! entry
      post  = IM.lookup entry $ C.ctxt_calls ctxt in
    (fromIntegral entry,(finit,post))


-- | Retrieve instruction for a given instruction address, both as datastructure and pretty-printed
ctxt_get_instruction :: C.Context -> Int -> IO X86.Instruction
ctxt_get_instruction ctxt a = do
  i <- C.fetch_instruction ctxt $ fromIntegral a
  case i of
    Nothing -> die $ "Could not disassemble instruction at address: " ++ showHex a
    Just i  -> return i


-- | Retrieve all instruction addresses.
ctxt_get_instruction_addresses :: C.Context -> S.Set Word64
ctxt_get_instruction_addresses ctxt =
  S.map fromIntegral $ S.unions $ map cfg_to_addresses $ IM.elems $ C.ctxt_cfgs ctxt
 where
  cfg_to_addresses g = S.fromList $ concat $ IM.elems $ C.cfg_blocks g

-- | Given an address @a@, retrieve the set of next addresses.
ctxt_get_controlflow :: C.Context -> Word64 -> IO (Word64,[Word64])
ctxt_get_controlflow ctxt a = do
  let entries = ctxt_get_function_entries ctxt
  posts   <- mapM get_post_per_entry $ S.toList entries
  return $ (fromIntegral a,map fromIntegral $ IS.toList $ IS.unions posts)
 where
  get_post_per_entry entry = do
    post <- stepA ctxt entry $ fromIntegral a
    case post of
      Left _     -> return $ IS.empty
      Right nxts -> return $ IS.fromList $ map fst nxts


-- | Returns a set of funtion entries.
ctxt_get_function_entries :: C.Context -> S.Set Int
ctxt_get_function_entries = S.fromList . IM.keys . C.ctxt_calls

ctxt_mk_function_boundary ctxt entry =
  let cfg = C.ctxt_cfgs ctxt IM.! entry
      addresses = concat $ IM.elems $ C.cfg_blocks cfg in
    (fromIntegral entry, intercalate "\n" $ map show_chunk $ mk_consecutive_chunks addresses)
 where
  show_chunk [i]   = showHex i ++ " (single instruction)"
  show_chunk chunk = showHex (head chunk) ++ "-->" ++ showHex (last chunk)

mk_consecutive_chunks :: [Int] -> [[Int]]
mk_consecutive_chunks = split_consecutives . sort
 where
  split_consecutives :: [Int] -> [[Int]]
  split_consecutives []         = []
  split_consecutives [i]        = [[i]]
  split_consecutives (i0:i1:is) = if i1 < i0 + 16 then add_to_hd i0 $ split_consecutives (i1:is) else [i0] : split_consecutives (i1:is)


  add_to_hd :: Int -> [[Int]] -> [[Int]]
  add_to_hd i []       = [[i]]
  add_to_hd i (is:iss) = (i:is) : iss



-- | Given an address @a@, retrieve all invariants (the address may occur in multiple functions).
ctxt_get_inv :: C.Context -> Word64 -> (Word64,Maybe [(Word64,C.Predicate)])
ctxt_get_inv ctxt a = do
  let entries = ctxt_get_function_entries ctxt
      invs    = map get_invariant_per_entry $ S.toList entries in
    if invs == [] || all ((==) Nothing) invs then do
      (a,Nothing)
    else
      (a,Just $ catMaybes invs)
 where
  get_invariant_per_entry entry =
    let fctxt = C.mk_fcontext ctxt entry in
     case get_invariant fctxt $ fromIntegral a of
       Nothing -> Nothing
       Just inv -> Just (fromIntegral entry,inv)


ctxt_resolve_mem_operands :: C.Context -> [(Word64,Word64, [Maybe SPointer])]
ctxt_resolve_mem_operands ctxt = 
  let entries = ctxt_get_function_entries ctxt
      cfgs    = map (\entry -> (entry,IM.lookup entry (C.ctxt_cfgs ctxt))) $ S.toList entries
      instrs  = map (\(entry,Just cfg) -> (entry,concat $ IM.elems $ C.cfg_instrs cfg)) cfgs in
    concatMap resolve_mem_operands instrs
 where
  resolve_mem_operands (entry,instrs) = map (resolve_mem_operand entry) instrs

  resolve_mem_operand entry i = (fromIntegral entry, X86.addressof i, map (resolve entry i) $ get_operands i)

  resolve entry i (Generic.Operand.Memory a si) = 
    let fctxt   = C.mk_fcontext ctxt entry
        Just p  = get_invariant fctxt (fromIntegral $ X86.addressof i)
        ptr     = evalState (sset_rip fctxt i >> sresolve_address fctxt a) (p,S.empty) in
        -- domain  = get_pointer_domain fctxt ptr in
      Just ptr
  resolve entry i _ = Nothing

  get_operands i =
    case dest i of
      Nothing  -> srcs i
      Just dst -> dst : srcs i










instance ToJSON Register  where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON Address where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON Operand where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON Opcode where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON Prefix where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON Instruction where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON BotSrc where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON PointerBase where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON BotTyp where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON Operator where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSONKey StatePart
instance ToJSON StatePart where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON SimpleExpr where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON Postcondition where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON FunctionSummary  where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON C.PointerDomain  where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON SPointer where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON JSON where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }




