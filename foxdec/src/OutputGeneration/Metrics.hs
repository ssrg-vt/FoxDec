{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, DeriveGeneric, StandaloneDeriving, StrictData #-}


module OutputGeneration.Metrics where 

import Base

import Data.SValue
import Data.L0
import Data.CFG
import Data.Indirection
import Data.VerificationCondition
import Data.SymbolicExpression
import Data.X86.Instruction


import Binary.Generic
import WithNoAbstraction.Pointers

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







mk_metrics bin l0 =
  let (rest,unresolved) = IM.partition ((/=) (S.singleton Indirection_Unresolved)) $ l0_indirections l0
      (partially_resolved,resolved) = IM.partition (S.member Indirection_Unresolved) $ rest
      instrs = l0_get_all_instructions l0
      num_intrs = length instrs
      avg_size = average $ map inSize instrs
      num_expected_intrs = floor (fromIntegral (binary_text_section_size bin) / avg_size)
      pars = IM.unions $ l0_get_pars l0
      pointerDesignations = mk_metric_pointerDesignations bin pars in
   intercalate "\n" [
       "-------------"
     , "INDIRECTIONS:"
     , "-------------"
     , "#indirections:                   " ++ show (IM.size $ l0_indirections l0) 
     , "  (of which resolved):           " ++ show (IM.size resolved) 
     , "  (of which partially resolved): " ++ show (IM.size partially_resolved)
     , "  (of which unresolved):         " ++ show (IM.size unresolved)
     ,show_indirections $ IM.filter ((/=) (S.singleton Indirection_Unresolved)) $ l0_indirections l0 

     , "\n"
     , "-------------"
     , "INSTRUCTIONS:"
     , "-------------"
     , "#instructions:                      " ++ show num_intrs
     , "average instruction size:           " ++ show avg_size
     , "estimate of expected #instructions: " ++ show num_expected_intrs
     , "coverage:                           " ++ show (round2dp (fromIntegral num_intrs / fromIntegral num_expected_intrs * 100))
     , "#instructions with memory access:   " ++ show (IM.size pars)
     , "  designations:                     " ++ show (M.assocs $ designations_to_percentages pointerDesignations)

     , "\n"
     , "----------"
     , "FUNCTIONS:"
     , "----------"
     , "#lifted functions:                              " ++ show (IM.size $ l0_functions l0)
     , "  (of which ending in unresolved indirections): " ++ show (IM.size $ IM.filter (is_unresolved . get_post) $ l0_functions l0)
     , "  (of which verification error):                " ++ show (IM.size $ IM.filter (is_error . get_post) $ l0_functions l0)

     , "\n"
     , "----------"
     , "OTHER:"
     , "----------"     
     , "verificaton time: " ++ l0_time l0
     , "\n"
   ]
  --putStrLn $ show $ IM.filter is_error $ IM.map get_post $ l0_functions l0
  --putStrLn $ show $ IM.map get_cfg $ IM.filter (is_error . get_post) $ l0_functions l0
 where
  get_post (finit,Just (FResult _ post _ _ _ _)) = post
  get_cfg (finit,Just (FResult cfg post _ _ _ _)) = cfg


  is_error (VerificationError _) = True
  is_error _ = False

  is_unresolved (HasUnresolvedIndirections _) = True
  is_unresolved _ = False

  l0_get_all_instructions = S.toList . S.fromList . concat . concatMap (IM.elems . cfg_instrs) . IM.elems . l0_get_cfgs

  

mk_metric_pointerDesignations bin = foldr (M.adjust ((+) 1)) init_m . concatMap get_specifity_per_instruction . IM.assocs
 where
  init_m = M.fromList [("U",0), ("CL",0),("CG",0),("CH",0),("CLG",0),("CLH",0),("CGH",0),("CLHG",0), ("AL",0),("AG",0),("AH",0),("ALG",0),("ALH",0),("AGH",0),("ALHG",0)]

  get_specifity_per_instruction (entry,PointerAnalysisResult ws rs) = map (get_domains entry) ws -- (w:rs) IF ALSO PRODUCING DATA OVER READS

  get_domains entry Nothing  = "Nothing"
  get_domains entry (Just e) = get_pointer_specifity_cpointer bin e


-- "C"     = Concrete
-- "C+U"   = Concrete plus unknown offset
-- "U"     = Unknown
-- "A"     = Addends
get_pointer_specifity_cpointer bin Top = "U"
get_pointer_specifity_cpointer bin (SAddends es)  = "A" ++ get_types bin es
get_pointer_specifity_cpointer bin (SConcrete es) = "C" ++ get_types bin es

get_types bin es = local ++ global ++ heap
 where
  local
    | any (expr_is_maybe_local_pointer bin) es = "L"
    | otherwise = ""
  global
    | any (expr_is_maybe_global_pointer bin) es = "G"
    | otherwise = ""
  heap 
    | any (\e -> not (expr_is_maybe_global_pointer bin e) && not (expr_is_maybe_local_pointer bin e)) es = "H"
    | otherwise = ""


designations_to_percentages :: M.Map String Int -> M.Map String Double
designations_to_percentages m = M.map (\v -> mk_percentage v $ sum $ M.elems m) m
 where
  mk_percentage x y = round2dp (x `intDiv` y * 100)

intDiv :: (Integral a,Integral b) => a -> b -> Double
x `intDiv` y = fromIntegral x / fromIntegral y


{--

metrics = M.fromList [
  ("#instructions",          "number of covered instructions"),
  ("avgInstructionSize",     "average instruction size"),
  ("#expectedInstructions",  "expected number of instructions"),
  ("%instructionCoverage",   "estimate of percentage of covered instructions"),
  ("#memWrites",             "total number of instructions writing to memory"),
  ("pointerDesignations",    "A, C, C+U, U"),
  ("%resolvedMemWrites",     "percentage of pointers that are assigned a non-trivial domain"),
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
      specifityMetric                 = specifityMetricOf pointerDesignationsPercentages
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
                 ("pointerDesignations",      show $ M.toList pointerDesignationsPercentages),
                 ("%resolvedMemWrites",       show resolvedMemWrites),
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
    case unsafePerformIO $ Analysis.Context.fetch_instruction ctxt $ fromIntegral a of -- Should be safe as result is immutable.
      Nothing -> False
      Just i  -> isCall $ opcode i
num_of_resolved_indirection_jumps ctxt = IM.size $ IM.filterWithKey (indirectionIsJump ctxt) $ ctxt_inds ctxt
 where
  indirectionIsJump ctxt a _ =
    case unsafePerformIO $ Analysis.Context.fetch_instruction ctxt $ fromIntegral a of -- Should be safe as result is immutable.
      Nothing -> False
      Just i  -> not $ isCall $ opcode i


-- | Number of instructions in CFG
num_of_instructions g = sum (map length $ IM.elems $ cfg_blocks g)

-- | Number of basic blocks in CFG
num_of_blocks g = IM.size $ cfg_blocks g

-- | Number of edges in CFG
num_of_edges g = sum (map IS.size $ IM.elems $ cfg_edges g)



percentageResolvedMemWrites :: M.Map String Double -> Double
percentageResolvedMemWrites m = 100 - m M.! "U"


specifityMetricOf :: M.Map String Double -> Double
specifityMetricOf m = m M.! "C" + 0.8*(m M.! "A") + 0.6*(m M.! "A")



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

--}
