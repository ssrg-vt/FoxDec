{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, DeriveGeneric, StandaloneDeriving, StrictData #-}


module OutputGeneration.JSON where



import Base

import Data.SValue
import Data.SymbolicExpression
import Data.JSON_Taxonomy

import Analysis.Context as C
import Analysis.Pointers 
import Analysis.ControlFlow


import OutputGeneration.Retrieval

import Generic.HasSize 
import Generic.Binary
import Generic.SymbolicConstituents
import Generic.Instruction
import Generic.Operand

import X86.Opcode
import X86.Prefix
import X86.Register

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable
import Data.Word

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)
import qualified Data.ByteString as BS  
import Data.ByteString.Lazy (writeFile)
import qualified Data.Serialize as Cereal hiding (get,put)

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

import Data.Aeson
import GHC.Generics
import Debug.Trace











generate_json :: Context -> String -> String -> Bool -> IO ()
generate_json ctxt fname_plain fname_json verbose = do
  let entries = ctxt_get_function_entries ctxt

  let instrs     = S.toList $ ctxt_get_instructions ctxt
  let addresses =  S.toList $ ctxt_get_instruction_addresses ctxt
  control_flow  <- mapM (ctxt_get_controlflow ctxt) addresses
  let boundaries = map  (ctxt_mk_function_boundary ctxt) $ S.toList entries
  let summaries  = map  (ctxt_get_function_summary ctxt) $ S.toList entries
  let mem_ops    = ctxt_resolve_mem_operands ctxt
  let invs       = if verbose then map (ctxt_get_inv ctxt) $ addresses else []

  let pp_text = toByteString $ mconcat $ map fromString $ pp_json ctxt verbose instrs control_flow boundaries summaries mem_ops invs
  BS.writeFile fname_plain pp_text


  let json_instructions = map mk_json_instruction instrs
  let json_summaries = map (\(a,(finit,post)) -> (a,FunctionSummary (show finit) $ mk_json_post post)) summaries
  let json_invs = if verbose then map (\(a,Just invs) -> (a,map (\(entry,inv) -> (entry,mk_json_predicate inv)) invs)) $ filter (((/=) Nothing) . snd) invs else []
  let json = JSON json_instructions control_flow boundaries json_summaries json_invs mem_ops
  Data.ByteString.Lazy.writeFile fname_json $ encode json




pp_json ctxt verbose instrs control_flow boundaries summaries mem_ops invs = map ((++) "\n") $
    [ "------------"
    , "INSTRUCTIONS"
    , "------------"]
    ++ map (pp_instruction ctxt) instrs ++
    [ "\n\n"
    , "------------"
    , "CONTROL FLOW"
    , "------------"]
    ++ pp_control_flow control_flow ++
    [ "\n\n"
    , "-------------------"
    , "FUNCTION BOUNDARIES"
    , "-------------------" ]
    ++ pp_boundaries boundaries ++
    [ "\n\n"
    , "------------------"
    , "FUNCTION SUMMARIES"
    , "------------------" ]
    ++ pp_summaries summaries ++
    [ "\n\n"
    , "---------------"
    , "POINTER DOMAINS"
    , "---------------" ]
    ++ pp_mem_ops ctxt mem_ops ++ 
    [ "\n\n" ]
  ++
    if verbose then 
      [ "----------"
      , "INVARIANTS"
      , "----------"]
      ++ pp_invs invs ++
      [ "\n\n"]
    else []

      


pp_invs = map pp
 where
  pp (a,Nothing)   = "Address " ++ showHex a ++ ": no invariant.\n"
  pp (a,Just invs) = "Address " ++ showHex a ++ ":\n" ++ intercalate "\n\n\n" (map pp_inv invs) ++ "\n"
  pp_inv (entry,inv) = "Entry: " ++ showHex entry ++ ":\n" ++ show inv ++ "\n"

pp_control_flow :: [(Word64,[Word64])] -> [String]
pp_control_flow = map pp
 where
  pp (a,as) = showHex a ++ " --> " ++ showHex_list as

pp_boundaries :: [(Word64,String)] -> [String]
pp_boundaries = map pp
 where
 pp (a,s) = showHex a ++ ": " ++ map repl s 
 repl '\n' = ';'
 repl c    = c

pp_summaries = map pp
 where
  pp (a,(finit,post)) = "Entry: " ++ showHex a ++ "\n" ++ pp_finit finit ++ "\n" ++ pp_post post

  pp_post (Just C.UnknownRetBehavior) = "Unknown return behavior" ++ "\n"
  pp_post (Just C.Terminating)        = "Terminal" ++ "\n"
  pp_post (Just (C.ReturningWith q))  = "Returns with postcondition:\n" ++ show q 

  pp_finit finit = if finit == init_finit then "No precondition" else "Precondition:\n" ++ show finit


pp_mem_ops ctxt = map pp
 where
  pp (entry,a,es) = "Address " ++ showHex a ++ ", entry: " ++ showHex entry ++ ": [" ++ intercalate "," (map pp_expr_option es) ++ "] " ++ concatMap (pp_dom entry) es

  pp_expr_option Nothing = "_"
  pp_expr_option (Just e) = show e

  pp_dom entry Nothing = "_"
  pp_dom entry (Just e) =
    let fctxt = mk_fcontext ctxt (fromIntegral entry) in
      get_pointer_domain_cpointer fctxt e

get_pointer_domain_cpointer fctxt v = exprs_to_domain $ svalue_to_exprs fctxt v
 where
  exprs_to_domain es
    | S.null es = "U"
    | all (not . contains_bot) es = "C"
    | all is_base $ S.map (get_pointer_domain fctxt) es = "B"
    | otherwise = "S"
  is_base (Just (C.Domain_Bases _)) = True
  is_base _ = False







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
instance ToJSON PointerDomain  where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON PtrOffset where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON PtrBase where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON SAddend where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON PtrValue where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON SValue where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }
instance ToJSON JSON where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }





