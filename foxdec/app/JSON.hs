{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, DeriveGeneric, StandaloneDeriving #-}


module JSON where


import Base
import Data.SimplePred
import Data.Binary
import Data.Pointers (mk_fcontext,get_pointer_domain)
import qualified Analysis.Context as C
import qualified X86.Instruction as X86
import qualified X86.Operand
import X86.Opcode
import X86.Prefix
import X86.Register
import Data.JSON_Taxonomy
import Pass.CFG_Gen
import Analysis.Propagation (supremum)
import Analysis.SymbolicExecution (get_invariant)
import Data.MachineState (resolve_address,write_reg)
import Data.CallGraph (pp_dom)

import Typeclasses.HasSize (HasSize(sizeof))
import Typeclasses.HasAddress (addressof)

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
instance ToJSON JSON where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }






-- | Main function
-- Read in first command-line argument, check whether it is a .report file.
-- Read the file, and use the "VerificationReportInterface" to get an overview of the instructions.
main = do
  args <- getArgs
  if length args /= 3 || args!!2 `notElem` ["True", "False"]  then
    putStrLn $ "Usage:\n\n  foxdec-json-exe DIRNAME NAME PP\n\nHere NAME refers to the NAME used when running foxdec-exe. PP is either True or False to indicate pretty-printing or JSON.\nRun this program from the same directory foxdec-exe was run."
  else do
    let pp = args!!2 == "True"
    let dirname = if last (args!!0)  == '/' then args!!0 else args!!0 ++ "/"
    exists <- doesFileExist $ dirname ++ args!!1 ++ ".report"
    if exists then do
      ctxt <- ctxt_read_report dirname (args !! 1)
      ctxt_generate_json ctxt pp
    else
      putStrLn $ "File: " ++ show (dirname ++ args!!1 ++ ".report") ++ " does not exist."


-- | First retrieve all instruction addresses, and for each address get the instruction.
ctxt_generate_json :: C.Context -> Bool -> IO ()
ctxt_generate_json ctxt pp = do
  let entries = ctxt_get_function_entries ctxt


  let instrs     = S.toList $ ctxt_get_instructions ctxt
  let addresses = [] -- S.toList $ ctxt_get_instruction_addresses ctxt
  control_flow  <- mapM (ctxt_get_controlflow ctxt) addresses
  let boundaries = [] --map  (ctxt_mk_function_boundary ctxt) $ S.toList entries
  let summaries  = [] --map  (ctxt_get_function_summary ctxt) $ S.toList entries
  let invs       = [] -- map  (ctxt_get_inv              ctxt) $ addresses
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
{--
    putStrLn $ "----------"
    putStrLn $ "INVARIANTS"
    putStrLn $ "----------"
    putStrLn $ pp_invs invs
    putStrLn $ "\n\n\n"
--}
    putStrLn $ "---------------"
    putStrLn $ "POINTER DOMAINS"
    putStrLn $ "---------------"
    putStrLn $ pp_mem_ops ctxt mem_ops
    putStrLn $ summarize_mem_ops ctxt mem_ops
  else do
    let json_instructions = map mk_json_instruction instrs
    let json_summaries = map (\(a,(finit,post)) -> (a,FunctionSummary finit $ mk_json_post post)) summaries
    let json_invs = map (\(a,Just invs) -> (a,map (\(entry,inv) -> (entry,mk_json_predicate inv)) invs)) $ filter (((/=) Nothing) . snd) invs
    let json = JSON json_instructions control_flow boundaries json_summaries json_invs mem_ops
    BSC.putStrLn $ toStrict $ encode json



summarize_mem_ops ctxt mem_ops = "(C,B,S,U) = " ++ (show $ foldr plus (0,0,0,0) (concatMap get_domains_per_instruction mem_ops))
 where

  get_domains_per_instruction (entry,a,es) = concatMap (get_domains entry) es

  get_domains entry Nothing = [(0,0,0,0)]
  get_domains entry (Just e) =
    if is_C e then 
      [(1,0,0,0)]
    else 
      let fctxt = mk_fcontext ctxt (fromIntegral entry) in
        case get_pointer_domain fctxt e of
          C.Domain_Bases _ -> [(0,1,0,0)]
          C.Domain_Sources srcs -> if S.null srcs then [(0,0,0,1)] else [(0,0,1,0)]

  plus (c,b,s,u) (c',b',s',u') = (c+c',b+b',s+s',u+u')

  is_C (Bottom (FromNonDeterminism es)) = all (not . contains_bot) es
  is_C e = not (contains_bot e)



pp_mem_ops ctxt = intercalate "\n" . map pp
 where
  pp (entry,a,es) = "Address " ++ showHex a ++ ", entry: " ++ showHex entry ++ ": [" ++ intercalate "," (map pp_expr_option es) ++ "] " ++ concatMap (pp_dom entry) es

  pp_expr_option Nothing = "_"
  pp_expr_option (Just e) = show e

  pp_dom entry Nothing = "_"
  pp_dom entry (Just e) =
    if not (contains_bot e) then 
      "C"
    else 
      let fctxt = mk_fcontext ctxt (fromIntegral entry) in
        case get_pointer_domain fctxt e of
          C.Domain_Bases _ -> "B"
          C.Domain_Sources srcs -> if S.null srcs then "U" else "S"


pp_invs = intercalate "\n\n" . map pp
 where
  pp (a,Nothing)   = "Address " ++ showHex a ++ ": no invariant."
  pp (a,Just invs) = "Address " ++ showHex a ++ ":\n" ++ intercalate "\n\n\n" (map pp_inv invs)
  pp_inv (entry,inv) = "Entry: " ++ showHex entry ++ ":\n" ++ pp_pred inv

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
  pp_post (Just (C.ReturningWith q))  = "Returns with postcondition:\n" ++ pp_pred q 

  pp_finit finit = if M.null finit then "No precondition" else "Precondition:\n" ++ (pp_pred $ Predicate finit None)


ctxt_get_instructions ctxt =
  let entries = ctxt_get_function_entries ctxt
      cfgs    = map (\entry -> IM.lookup entry (C.ctxt_cfgs ctxt)) $ S.toList entries
      instrs  = map (\(Just cfg) -> concat $ IM.elems $ C.cfg_instrs cfg) cfgs in
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





-- | Read in the .report file from a file with the given file name.
--   May produce an error if no report can be read from the file.
--   Returns the verification report stored in the .report file.
ctxt_read_report :: 
   String        -- ^ The directory name
   -> String     -- ^ The file name of the binary
   -> IO C.Context
ctxt_read_report dirname name = do
  rcontents <- BS.readFile (dirname ++ name ++ ".report")
  ioref     <- newIORef IM.empty
  bcontents <- C.read_binary dirname name
  case (Cereal.decode rcontents, bcontents) of
    (Left err,_)           -> error $ "Could not read verification report in file " ++ (dirname ++ name ++ ".report") ++  "\n" ++ show err
    (_,Nothing)            -> error $ "Cannot read binary file: " ++ dirname ++ name
    (Right ctxt_,Just bin) -> return $ C.Context bin ioref ctxt_





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
-- Produce the supremum of all found invariants.
ctxt_get_inv :: C.Context -> Word64 -> (Word64,Maybe [(Word64,Pred)])
ctxt_get_inv ctxt a = do
  let entries = ctxt_get_function_entries ctxt
      invs    = map get_invariant_per_entry $ S.toList entries in
    if invs == [] || all ((==) Nothing) invs then do
      (a,Nothing)
    else
      (a,Just $ catMaybes invs)
 where
  get_invariant_per_entry entry =
    let fctxt = mk_fcontext ctxt entry in
     case get_invariant fctxt $ fromIntegral a of
       Nothing -> Nothing
       Just inv -> Just (fromIntegral entry,inv)


ctxt_resolve_mem_operands ctxt = 
  let entries = ctxt_get_function_entries ctxt
      cfgs    = map (\entry -> (entry,IM.lookup entry (C.ctxt_cfgs ctxt))) $ S.toList entries
      instrs  = map (\(entry,Just cfg) -> (entry,concat $ IM.elems $ C.cfg_instrs cfg)) cfgs in
    concatMap resolve_mem_operands instrs
 where
  resolve_mem_operands (entry,instrs) = map (resolve_mem_operand entry) instrs

  resolve_mem_operand entry i = (fromIntegral entry, addressof i, map (resolve entry i) $ get_operands i)

  resolve entry i (Generic.Operand.Memory a si) = 
    let fctxt   = mk_fcontext ctxt entry
        Just p  = get_invariant fctxt (fromIntegral $ addressof i)
        ptr     = evalState (writeRIP fctxt i >> resolve_address fctxt a) (p,S.empty)
        domain  = get_pointer_domain fctxt ptr in
      Just ptr
  resolve entry i _ = Nothing

get_operands i =
  case dest i of
    Nothing  -> srcs i
    Just dst -> dst : srcs i


writeRIP ctxt i = write_reg ctxt (addressof i) RIP (SE_Immediate $ addressof i + (fromIntegral $ sizeof i))

