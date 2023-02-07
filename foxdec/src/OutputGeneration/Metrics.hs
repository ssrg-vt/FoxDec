{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, DeriveGeneric, StandaloneDeriving, StrictData #-}


module OutputGeneration.Metrics where

import Base

import Data.SPointer
import Data.SymbolicExpression

import Analysis.Context
import Analysis.Pointers 
import Analysis.ControlFlow


import OutputGeneration.Retrieval

import Generic.HasSize 
import Generic.Binary
import Generic.SymbolicConstituents
import Generic.Instruction

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


metrics = M.fromList [
  ("#instructions",          "number of covered instructions"),
  ("avgInstructionSize",     "average instruction size"),
  ("#expectedInstructions",  "expected number of instructions"),
  ("%instructionCoverage",   "estimate of percentage of covered instructions"),
  ("#memWrites",             "total number of instructions writing to memory"),
  ("pointerDesignations",    "C+LGH, C+O, NC+LH, NC+O, U"),
  ("specifityMetric",        "weighted mean of pointer designations"),

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
      specifityMetric                 = specifityMetricOf pointerDesignationsPercentages

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
                 ("pointerDesignations",      show $ M.toList pointerDesignationsPercentages),
                 ("specifityMetric",          show specifityMetric),
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


mk_metric_pointerDesignations :: Context -> [(Word64,Word64, [Maybe SPointer])] -> M.Map String Int
mk_metric_pointerDesignations ctxt = foldr (M.adjust ((+) 1)) init_m . concatMap get_specifity_per_instruction
 where
  init_m = M.fromList [("C+LGH",0), ("C+O", 0), ("NC+LH",0), ("NC+O",0), ("U",0)]

  get_specifity_per_instruction (entry,a,es) = map (get_domains entry) es

  get_domains entry Nothing = "Nothing"
  get_domains entry (Just e) = 
    let fctxt = mk_fcontext ctxt (fromIntegral entry) in
       get_pointer_specifity_cpointer fctxt e



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
    | all (expr_is_global_immediate $ f_ctxt fctxt) es   = "C+LGH"
    | all (expr_is_highly_likely_heap_pointer fctxt) es  = "C+LGH"
    | otherwise                                          = "C+O"


specifityMetricOf :: M.Map String Double -> Double
specifityMetricOf m = m M.! "C+LGH" + m M.! "C+O" + m M.! "NC+LH" + 0.3*(m M.! "NC+O")



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
