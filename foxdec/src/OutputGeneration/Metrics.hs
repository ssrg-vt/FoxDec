{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, DeriveGeneric, StandaloneDeriving, StrictData #-}


module OutputGeneration.Metrics (
  num_of_instructions,
  num_of_unres_inds_in_cfg,
  num_of_blocks,
  num_of_edges, 
  mk_metrics
 ) where

import Base

import Data.SValue
import Data.SymbolicExpression

import Analysis.Context
import Analysis.Pointers 
import Analysis.ControlFlow

import Parser.ParserPinLog

import OutputGeneration.Retrieval

import Generic.HasSize 
import Generic.Binary
import Generic.SymbolicConstituents
import Generic.Instruction

import Instantiation.SymbolicPropagation (get_invariant)

import X86.Opcode


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)

import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable
import Data.Word
import Data.Int (Int64)

import System.IO.Unsafe (unsafePerformIO)

import GHC.Generics
import Debug.Trace
import Numeric


metrics = M.fromList [
  ("#instructions",          "number of covered instructions"),
  ("avgInstructionSize",     "average instruction size"),
  ("#expectedInstructions",  "expected number of instructions"),
  ("%instructionCoverage",   "estimate of percentage of covered instructions"),
  ("#memWrites",             "total number of instructions writing to memory"),
  ("pointerDesignations",    intercalate "," $ M.keys init_pointerDesignations),
  ("pointerDesignations2",   ""),
  ("%resolvedMemWrites",     "percentage of pointers that are assigned a non-trivial domain"),

  ("#functions",             "total number of functions"),
  ("#functions_verified",    "total number of verified functions"),
  ("#functions_unresolved",  "total number of functions with unresolved indirections"),
  ("#functions_verif_error", "total number of functions with verification error"),

  ("#resolved_jumps",        "total number of resolved jumps"),
  ("#resolved_calls",        "total number of resolved calls"),
  ("#unresolved_jumps",      "total number of unresolved jumps"),
  ("#unresolved_calls",      "total number of unresolved calls"),

  ("runningTime",            "Running time in HH:MM:SS")
 ]


-- | Returns all generated metrics in JSON and pretty-printed
mk_metrics :: Context -> (String,String)
mk_metrics ctxt =
  let instrs                          = S.toList $ ctxt_get_instructions ctxt
      avg_size                        = average $ map sizeof instrs
      text_section_size               = binary_text_section_size $ ctxt_binary ctxt
      num_expected_intrs              = floor (fromIntegral text_section_size / avg_size)
      num_intrs                       = length instrs
      instructionCoverage             = round2dp (fromIntegral num_intrs / fromIntegral num_expected_intrs * 100)
      mem_ops                         = ctxt_resolve_mem_operands ctxt
      pointerDesignations             = mk_metric_pointerDesignations ctxt mem_ops 
      pointerDesignationsPercentages  = designations_to_percentages pointerDesignations
      memWrites                       = sum $ M.elems pointerDesignations
      resolvedMemWrites               = percentageResolvedMemWrites pointerDesignationsPercentages

      num_functions                   = IM.size $ ctxt_results ctxt
      num_verif_success               = num_of_verif_success ctxt
      num_verif_unresolved            = num_of_verif_unresolved ctxt
      num_verif_error                 = num_of_verif_error ctxt

      resolved_jumps                  = num_of_resolved_indirection_jumps ctxt
      resolved_calls                  = num_of_resolved_indirection_calls ctxt
      unresolved_jumps                = num_of_unres_inds ctxt isJump
      unresolved_calls                = num_of_unres_inds ctxt isCall

      runningTimeRepr                 = show_runningtime $ ctxt_runningtime ctxt

      metrics = map mk_metric [
                 ("#instructions",            show num_intrs),
                 ("avgInstructionSize",       show avg_size),
                 ("#expectedInstructions",    show num_expected_intrs),
                 ("%instructionCoverage",     show instructionCoverage),
                 ("#memWrites",               show memWrites),
                 ("pointerDesignations",      showPointerDesignationsPercentages $ M.toList pointerDesignationsPercentages),
                 ("pointerDesignations2",     show $ M.toList pointerDesignationsPercentages),
                 ("%resolvedMemWrites",       show resolvedMemWrites),
                 ("#functions",               show num_functions),
                 ("#functions_verified",      show num_verif_success),
                 ("#functions_unresolved",    show num_verif_unresolved),
                 ("#functions_verif_error",   show num_verif_error),
                 ("#resolved_jumps",          show resolved_jumps),
                 ("#resolved_calls",          show resolved_calls),
                 ("#unresolved_jumps",        show unresolved_jumps),
                 ("#unresolved_calls",        show unresolved_calls),
                 ("runningTime",              runningTimeRepr)
                ] in
  (unpack $ encode metrics,intercalate "\n" $ map show_metric metrics)
 where
  mk_metric (name,value) = Metric name (metrics M.! name) value


  show_metric (Metric name desc value) =
    let str0 = name ++ replicate (metrics_metric_size - length name + 3) ' '
        str1 = str0 ++ " (" ++ desc ++ ")" in
      str1 ++ replicate (metrics_descr_size - length desc) ' ' ++ " = " ++ value

  designations_to_percentages :: M.Map String Int -> M.Map String Double
  designations_to_percentages m = M.map (\v -> mk_percentage v $ sum $ M.elems m) m
  
  mk_percentage x y = round2dp (x `intDiv` y * 100)


-- | Show time in HH:MM:SS
show_runningtime :: Int64 -> String
show_runningtime secs = show hours ++ ":" ++ show minutes ++ ":" ++ show seconds
 where
  (hours,rem_hour)  = secs `divMod` 3600
  (minutes,seconds) = rem_hour `divMod` 60

-- | Number of functions successfully verified
num_of_verif_success = IM.size . IM.filter isVerificationSuccess . ctxt_results
 where
  isVerificationSuccess VerificationSuccess               = True
  isVerificationSuccess VerificationSuccesWithAssumptions = True
  isVerificationSuccess _                                 = False

-- | Number of functions with unresolved indirections
num_of_verif_unresolved = IM.size . IM.filter ((==) VerificationUnresolvedIndirection) . ctxt_results

-- | Number of functions with a verification error
num_of_verif_error = IM.size . IM.filter isVerificationError . ctxt_results
 where
  isVerificationError (VerificationError _) = True
  isVerificationError _                     = False

-- | Number of unresolved indirections
num_of_unres_inds ctxt chkKind = sum (map (num_of_unres_inds_in_cfg ctxt chkKind) $ IM.elems $ ctxt_cfgs ctxt)

-- | Number of unresolved indirections in the given CFG
num_of_unres_inds_in_cfg ctxt chkKind g = 
  let blocks  = IM.keys $ cfg_blocks g in
    length (filter (\b -> node_info_of ctxt g b == UnresolvedIndirection && ends_in_kind b) blocks)
 where
  ends_in_kind b = chkKind $ opcode $ last (fetch_block g b)

-- | Number of resolved indirections
num_of_resolved_indirection_calls ctxt = IM.size $ IM.filterWithKey (indirectionIsCall ctxt) $ ctxt_inds ctxt
 where
  indirectionIsCall ctxt a _ =
    case unsafePerformIO $ fetch_instruction ctxt $ fromIntegral a of -- Should be safe as result is immutable.
      Nothing -> False
      Just i  -> isCall $ opcode i
num_of_resolved_indirection_jumps ctxt = IM.size $ IM.filterWithKey (indirectionIsJump ctxt) $ ctxt_inds ctxt
 where
  indirectionIsJump ctxt a _ =
    case unsafePerformIO $ fetch_instruction ctxt $ fromIntegral a of -- Should be safe as result is immutable.
      Nothing -> False
      Just i  -> not $ isCall $ opcode i


-- | Number of instructions in CFG
num_of_instructions g = sum (map length $ IM.elems $ cfg_blocks g)

-- | Number of basic blocks in CFG
num_of_blocks g = IM.size $ cfg_blocks g

-- | Number of edges in CFG
num_of_edges g = sum (map IS.size $ IM.elems $ cfg_edges g)

{--
mk_comparison_to_pinlog :: Context -> [(Word64,Word64, [Maybe SimpleExpr])] -> PinLog -> [String]
mk_comparison_to_pinlog ctxt pointerDesignations (PinLog name base sections log) =
  concatMap mk_comparison_per_instruction pointerDesignations
 where
  mk_comparison_per_instruction (entry,a,es) = concatMap (mk_comparison_per_mem_access a) es

  mk_comparison_per_mem_access a Nothing  = []
  mk_comparison_per_mem_access a (Just e) = 
    case M.lookup a log of
      Nothing -> ["No ground truth"]
      Just observations -> compare a e observations

   compare rip a observations
     | expr_is_highly_likely_local_pointer ctxt a = must_be_local rip observations
     | expr_is_global_immediate ctxt a            = must_be_global rip observations
     | 

--}
powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

showFullPrecision :: Double -> String
showFullPrecision x = map substDot $ showFFloat Nothing x ""
 where
  substDot '.' = ','
  substDot c   = c

showPointerDesignationsPercentages :: [(String,Double)] -> String
showPointerDesignationsPercentages = intercalate " " . map (showFullPrecision . snd)

init_pointerDesignations = M.fromList $ zip keys $ repeat 0
 where
  lhg  = filter ((/=) "") $ powerList "LGH" 
  keys = [x:y | x <- "CBS", y <- lhg] ++ ["U"] 


mk_metric_pointerDesignations :: Context -> [(Word64,Word64, [Maybe SimpleExpr])] -> M.Map String Int
mk_metric_pointerDesignations ctxt = foldr (M.adjust ((+) 1)) init_pointerDesignations . concatMap get_specifity_per_instruction
 where
  get_specifity_per_instruction (entry,a,es) = map (get_domains entry) $ concatMap unfold es

  unfold Nothing  = [Nothing]
  unfold (Just e) = map Just $ unfold_non_determinism ctxt e

  get_domains entry Nothing = "Nothing"
  get_domains entry (Just e) = 
    let fctxt = mk_fcontext ctxt (fromIntegral entry) in
       get_pointer_specifity fctxt e

-- "C"     = Concrete
-- "B"     = Bases
-- "S"     = Sources
-- "U"     = Unknown
get_pointer_specifity :: FContext -> SimpleExpr -> String
get_pointer_specifity fctxt e
  | not (contains_bot e) = "C" ++ get_pointer_domains e
  | otherwise = 
    case get_pointer_domain fctxt e of
      Just (Domain_Bases _)   -> "B" ++ get_pointer_domains e
      Just (Domain_Sources _) -> "S" ++ get_pointer_domains e
      Nothing                 -> "U"
 where
  get_pointer_domains e = concat [get_local e, get_global e, get_heap e]

  get_local e
    | any is_local_base $ get_pointer_base_set fctxt e = "L"
    | any is_local_source $ srcs_of_expr fctxt e = "L"
    | otherwise = ""
   
  is_local_base (StackPointer _) = True
  is_local_base _                = False

  is_local_source (Src_StackPointer _) = True
  is_local_source _                    = False 


  get_global e
    | any is_global_base $ get_pointer_base_set fctxt e = "G"
    | any is_global_source $ srcs_of_expr fctxt e = "G"
    | otherwise = ""

  is_global_base (GlobalAddress _) = True
  is_global_base _                = False

  is_global_source (Src_ImmediateAddress a) = expr_is_global_immediate (f_ctxt fctxt) (SE_Immediate a)
  is_global_source _                        = False 

  get_heap e
    | all_and_not_empty is_local_base $ get_pointer_base_set fctxt e = ""
    | all_and_not_empty is_local_source $ srcs_of_expr fctxt e = ""
    | all_and_not_empty is_global_base $ get_pointer_base_set fctxt e = ""
    | all_and_not_empty is_global_source $ srcs_of_expr fctxt e = ""
    | otherwise = "H"

  all_and_not_empty p s = not (S.null s) && all p s



percentageResolvedMemWrites :: M.Map String Double -> Double
percentageResolvedMemWrites m = 100 - m M.! "U"





metrics_metric_size = maximum $ map length $ M.keys metrics
metrics_descr_size  = maximum $ map length $ M.elems metrics


round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2



intDiv :: (Integral a,Integral b) => a -> b -> Double
x `intDiv` y = fromIntegral x / fromIntegral y

data Metric = Metric {
  metrics_name  :: String, -- ^ Name of the metric
  metrics_desc  :: String, -- ^ Description of the metric
  metrics_value :: String  -- ^ Serialized value of the metric
 }
 deriving Generic

instance ToJSON Metric where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
