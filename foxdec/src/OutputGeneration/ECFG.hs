{-# LANGUAGE PartialTypeSignatures, StrictData, BangPatterns #-}

{-|
Module      : ECFG 
Description : Contains functions pertaining to exceptional control flow graph generation.
-}

module OutputGeneration.ECFG where

import Base

import Algorithm.Graph

import Binary.FunctionNames
import Data.Symbol
import Data.X86.Opcode
import Data.X86.Instruction
import Data.X86.Register
import WithAbstractPredicates.ControlFlow
import WithNoAbstraction.SymbolicExecutionPath
import WithNoAbstraction.Lifted
import WithAbstractPredicates.GenerateCFG

import OutputGeneration.CallGraph
import Data.L0
import Data.JumpTarget
import Data.Indirection
import Data.CFG
import Data.CFI
import Binary.Generic 
import Data.SymbolicExpression 

import Conventions


import Control.Monad.Extra
import Control.Monad.State.Strict


import qualified Data.IntMap as IM hiding (insertWith)
import qualified Data.IntMap.Strict as IM (insertWith)
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Set.Extra as S (unzip)
import qualified Data.Map as M
import Data.Either (fromRight,fromLeft,partitionEithers)
import Data.Maybe (fromJust,fromMaybe,isNothing,mapMaybe)
import Data.List
import Data.List.Split (chunksOf)
import Data.Word (Word64)
import Control.Monad ((>=>))
import Debug.Trace
import System.Demangle.Pure


import System.IO.Unsafe


-- DATA STRUCTURES
data Exception = Exception {
    exception_type :: String -- ^ The type, e.g., "std::runtime_error"
  , exception_base :: Maybe String -- ^ Optionally, the base class, e.g., "std:exception"
  , exception_destructor :: Maybe String -- ^ Optionally, the name of the destructor function passed to __cxa_throw
  }
  deriving (Ord,Eq)

data ECFG_ControlFlow = 
    Terminate -- ^ The program has terminated (e.g., exit() or _assert_fail())
  | Resume -- ^ __Unwind_Resume
  | Raise -- ^ _Unwind_RaiseException
  | CatchBegin -- ^ "__cxa_begin_catch
  | CatchEnd -- ^ __cxa_end_catch
  | Rethrow Word64 -- ^ __cxa_rethrow  
  | Throw Word64 Exception -- ^ __cxa_throw
  deriving (Eq)

show_exception_as_arg (Exception typ base destr) = (if destr == Nothing then "" else "*") ++ "(" ++ typ ++ show_base base ++ ")"
 where
  show_base Nothing = ""
  show_base (Just base) = " :: " ++ base

instance Show ECFG_ControlFlow where
  show Terminate = "Terminate"
  show Resume = "Resume"
  show Raise = "Raise"
  show CatchBegin = "CatchBegin"
  show CatchEnd = "CatchEnd"
  show (Rethrow a) = "Rethrow"
  show (Throw a e) = "Throw" ++ show_exception_as_arg e

data ECFG_Vertex_Info =
    ECFG_Start                      -- ^ Initial vertex
  | ECFG_LandingPad String Bool Int -- ^ A landing pad with color (bool==True indicates cleanup only)
  | ECFG_CF ECFG_ControlFlow        -- ^ A call to a control flow function (throw, catchBegin, etc.)
  | ECFG_Return (Maybe Word64)      -- ^ Return (this can happen through a RET, or to a JMP instead of CALL to a function)
  | ECFG_Vertex_Region ECFG_Region  -- ^ A region
  deriving (Eq,Show)

-- A region (from a callsite in the GCC except table) is defined by start- and end-addresses in the .text section.
-- Throws within those bounds will unwind to the landing pad.
-- The action is used to derive which exception type we are going to handle.
-- The color is for rendering the ECFG.
data ECFG_Region = ECFG_Region {
    ecfg_region_end :: Word64
  , ecfg_region_start :: Word64
  , ecfg_region_landingpad :: Int
  , ecfg_region_action :: Word64
  , ecfg_region_color :: String
  , ecfg_region_index_of_table :: Int
  }
  deriving (Eq,Show)

-- A vertex consists of its address, the adress at the end of the current block, and information.
-- The second is only needed for rendering, i.e., has no semantical meaning.
data ECFG_Vertex = ECFG_Vertex Word64 Word64 [ECFG_Vertex_Info] (S.Set (Word64,Word64))
  deriving (Eq)

instance Show ECFG_Vertex where
  show (ECFG_Vertex start end info calls) = "Block 0x" ++ showHex start

-- An edge goes from blockID to blockID with optionally a label (for filtering exception types).
data ECFG_Edge = ECFG_Edge {
    ecfg_edge_start :: Int
  , ecfg_edge_label :: Maybe String
  , ecfg_edge_end   :: Int
  }
  deriving (Ord,Eq,Show)

-- Each function has its own ECFG
data ECFG = ECFG {
    ecfg_vertices :: IM.IntMap ECFG_Vertex -- ^ A mapping from blockIDs to vertices
  , ecfg_edges :: [ECFG_Edge] -- ^ The edges (using blockIDs)
  , ecfg_entry :: Word64 -- ^ The entry address of the function
  , ecfg_regions :: [ECFG_Region] -- ^ The regions
  }
  deriving (Eq,Show)




-- SYMBOLIC EXECUTION
data EStatus = Normal | Terminated | Returned | Unwinding Exception | Landed Exception | Handling Exception | WeirdError String
  deriving (Ord,Eq)

instance Show EStatus where
  show Normal = "Normal"
  show Terminated = "Terminated"
  show Returned = "Returned"
  show (Unwinding e) = "Unwinding" ++ show_exception_as_arg e
  show (Landed e) = "Landed" ++ show_exception_as_arg e
  show (Handling e) = "Handling" ++ show_exception_as_arg e
  show (WeirdError msg) = "Error " ++ show msg

data ECFG_SymState = ECFG_SymState {
    ss_estatus :: [EStatus]
  , ss_rip :: Word64
  }
  deriving (Ord,Eq)


instance Show ECFG_SymState where
  show (ECFG_SymState es a) = "0x" ++ showHex a ++ ": " ++ show es


emitLog   msg = id -- trace msg

ecfg_vertex_to_address (ECFG_Vertex a _ _ _) = a

ecfg_blockID_to_address (ECFG vertices _ _ _) blockID = ecfg_vertex_to_address $ vertices IM.! blockID

init_ecfg_symstate ecfg blockID = ECFG_SymState [Normal] $ ecfg_blockID_to_address ecfg blockID


sym_exec_cfg_all :: Lifted -> IO ()
sym_exec_cfg_all l@(bin,config,l0) = do
  let fs = get_call_graph_sources l
  ssf <- execStateT (mapM_ run_entry $ IS.toList fs) init_StateSoFar
  putStrLn $ ssf_pp l ssf
 where
  run_entry = sym_exec_ecfg_entry l True . fromIntegral


sym_exec_ecfg_entry :: Lifted -> Bool -> Word64 -> StateT StateSoFar IO (S.Set EStatus)
sym_exec_ecfg_entry l@(bin,config,l0) isFirst entry = do
  posts <- gets ssf_function_posts
  is_explored <- gets (IS.member (fromIntegral entry) . ssf_currently_explored_functions)
  case (is_explored, IM.lookup (fromIntegral entry) posts) of
    (True,_) -> do
      let msg = "Entry 0x" ++ showHex entry ++ ": overapproximation due to (mutual) recursion."
      let cfg  = cfg_split_jumps $ l0_get_cfgs l0 IM.! fromIntegral entry
      let ecfg = ecgf_unfold_jumps_to_function_entries l $ cfg_to_ecfg l entry cfg
      let q    = overapproximate_post l entry ecfg
      return $ emitLog msg q 
    (_,Just posts) -> do
      let msg = "Entry 0x" ++ showHex entry ++ " already visited."
      return $ emitLog msg posts
    (_,Nothing) -> do
      putIfFirst $ "-----------------------------"
      putIfFirst $ "Entry: " ++ address_to_label bin entry
      let msg  = "Entry 0x" ++ showHex entry ++ " to be explored."
      let cfg  = emitLog msg $ cfg_split_jumps $ l0_get_cfgs l0 IM.! fromIntegral entry
      let ecfg = ecgf_unfold_jumps_to_function_entries l $ cfg_to_ecfg l entry cfg
      pres <- gets ssf_block_pres

      modify $ ssf_set_explored entry True
      modify $ ssf_set_preconditions IM.empty
      sym_exec_ecfg l ecfg
      modify $ ssf_set_preconditions pres
      modify $ ssf_set_explored entry False

      ssf <- get
      posts <- gets $ ssf_get_posts entry
      putIfFirst $ "Post: " ++ (show $ S.toList posts)
      putIfFirst $ "-----------------------------"

      let msg = "Entry 0x" ++ showHex entry ++ " exploration done: " ++ show (S.toList posts)
      return $ emitLog msg posts
 where
  putIfFirst 
    | isFirst   = liftIO . putStrLn
    | otherwise = \_ -> return ()



overapproximate_post l@(bin,config,l0) entry ecfg = S.unions [ use_l0_post, use_ecfg ]
 where
  use_l0_post =
    let (finit,result) = l0_functions l0 IM.! (fromIntegral entry) in
      case result_post <$> result of
        Just Terminates      -> S.singleton Terminated
        Just (ReturnsWith _) -> S.singleton Returned
        _                    -> S.empty

  use_ecfg = IM.foldr ecfg_vertex_to_post S.empty $ ecfg_vertices ecfg

  ecfg_vertex_to_post (ECFG_Vertex _ _ info _) q = foldr ecfg_vertex_info_to_post q info

  ecfg_vertex_info_to_post (ECFG_CF Terminate)     = S.insert $ Terminated
  ecfg_vertex_info_to_post (ECFG_CF (Throw a e))   = S.insert $ Unwinding e
  ecfg_vertex_info_to_post (ECFG_Return _)         = S.insert $ Returned
  ecfg_vertex_info_to_post _                       = id




post_to_estatus (ECFG_SymState (e@(Returned)    :_) _) = e
post_to_estatus (ECFG_SymState (e@(Unwinding  _):_) _) = e
post_to_estatus (ECFG_SymState (e@(Terminated  ):_) _) = e
post_to_estatus (ECFG_SymState (e@(WeirdError _):_) _) = e
post_to_estatus s = error $ "Post = " ++ show s

data StateSoFar = StateSoFar {
    ssf_function_posts :: IM.IntMap (S.Set EStatus)   -- ^ Currently known postconditions for functions
  , ssf_block_pres :: IM.IntMap (S.Set ECFG_SymState) -- ^ Preconditions for blocks current function
  , ssf_currently_explored_functions :: IS.IntSet     -- ^ Which functions are currently being explored?
  }

init_StateSoFar = StateSoFar IM.empty IM.empty IS.empty

ssf_pp l@(bin,config,l0) (StateSoFar fp bp fs) = intercalate "\n" $ ["SUMMARY:"] ++ mk_errors ++ [mk_summary] -- (intercalate "\n" $ map show_fp_entry $ IM.assocs fp)
 where
  num_of_instrs = sum $ map (sum . map length . IM.elems . cfg_instrs) $ IM.elems $ l0_get_cfgs l0

  count_posts p = show $ IM.size $ IM.filter p fp

  num_return_only = IM.size $ IM.filter (\es -> S.size es == 1) fp

  mk_summary = intercalate ", " $ 
    [ binary_file_name bin
    , show $ num_of_instrs
    , count_posts (\_ -> True)
    , count_posts (\es -> all (\e -> isReturn e || isTerminated e) es && any isReturn es)
    , count_posts (\es -> all isTerminated es)
    , count_posts (\es -> all isUnwinding es)
    , count_posts (\es -> any isReturn es && any isUnwinding es)
    , count_posts (\es -> any isError es) ]

  isReturn     p = p == Returned
  isTerminated p = p == Terminated
  isUnwinding  (Unwinding _)  = True
  isUnwinding  _              = False
  isError  (WeirdError _) = True
  isError  _              = False

  mk_errors = map show_error $ S.toList $ S.unions $ map get_errors $ IM.elems fp

  get_errors ps = S.filter isError ps
  show_error (WeirdError msg) = "ERROR: " ++ msg
    

ssf_set_preconditions pres (StateSoFar fp bp fs) = StateSoFar fp pres fs

ssf_add_preconditon blockID s (StateSoFar fp bp fs) = StateSoFar fp (IM.insertWith S.union blockID (S.singleton s) bp) fs

ssf_add_function_post entry p (StateSoFar fp bp fs) = StateSoFar (IM.insertWith S.union (fromIntegral entry) p fp) bp fs

ssf_get_posts entry (StateSoFar fp bp fs) = IM.lookup (fromIntegral entry) fp `orElse` S.empty

ssf_set_explored entry True  (StateSoFar fp bp fs) = StateSoFar fp bp $ IS.insert (fromIntegral entry) fs
ssf_set_explored entry False (StateSoFar fp bp fs) = StateSoFar fp bp $ IS.delete (fromIntegral entry) fs

sym_exec_ecfg l ecfg = sym_exec_ecfg' l ecfg $ S.singleton (0,init_ecfg_symstate ecfg 0)

safeLookup m k = 
  case IM.lookup k m of
    Just v  -> v
    Nothing -> error $ "Cannot find key " ++ show k ++ " in map " ++ show m

sym_exec_ecfg' l ecfg@(ECFG vertices edges entry regions) bag =
  case S.minView bag of
    Nothing          -> return ()
    Just ((blockID,s),bag) -> do
      curr_pres <- gets ssf_block_pres
      let curr_ss = IM.lookup blockID curr_pres `orElse` S.empty
      if s `S.member` curr_ss then
        sym_exec_ecfg' l ecfg bag
      else do
        let v                          = vertices `safeLookup` blockID
        let out_edges                  = S.fromList $ filter (is_edge_from blockID) edges
        -- Execute the vertex
        ss'                           <- sym_exec_ecfg_vertex l ecfg entry blockID v s
        -- Unwind states that can be unwinded
        let (done_or_unwinding,normal) = S.partition isDoneOrUnwinding ss'
        let (done,bag1)                = S.unzip $ S.map (sym_exec_unwind ecfg v) done_or_unwinding
        -- For other states, go the next vertex
        let filtered_out_edges         = S.unions $ S.map (applyFilter out_edges) normal
        let bag0                       = S.map (\(s',edge) -> sym_exec_ecfg_edge ecfg edge s') filtered_out_edges
        -- Add the currently known precondition for the current block
        modify $ ssf_add_preconditon blockID s
        -- Add new postconditions
        modify $ ssf_add_function_post entry $ S.map post_to_estatus $ S.unions done
        sym_exec_ecfg' l ecfg $ S.unions [bag,bag0,S.unions bag1]
 where
  is_edge_from blockID (ECFG_Edge blockID0 _ _) = blockID == blockID0

  isUnwinding (ECFG_SymState (Unwinding _:_) _) = True
  isUnwinding _ = False

  isDoneOrUnwinding (ECFG_SymState (Unwinding  _:_) _) = True
  isDoneOrUnwinding (ECFG_SymState (Returned    :_) _) = True
  isDoneOrUnwinding (ECFG_SymState (Terminated  :_) _) = True
  isDoneOrUnwinding (ECFG_SymState (WeirdError _:_) _) = True
  isDoneOrUnwinding _                                  = False


-- TODO
applyFilter edges s
  | S.null labeled         = withState edges
  | not $ S.null unlabeled = error $ "Do not know how to give semantics to edges: " ++ show edges
  | otherwise =
    case ss_estatus s of
      (Landed e:_)   -> try_label (exception_type e) `orTry` (try_label =<< exception_base e) `orTry` try_label "(...)" `orTry` try_label "(none)" `orElse` fail
      (Handling e:_) -> try_label "(...)" `orTry` try_label "(none)" `orElse` fail
      _              -> error $ "Applying filter in a state that is not Landed: " ++ show s
 where
  fail = error $ "Outgoing edges cannot be applied: " ++ show edges ++ " in state " ++ show s

  withState = S.map (\edge -> (s,edge))

  (unlabeled,labeled) = S.partition (\edge -> ecfg_edge_label edge == Nothing) edges

  try_label l =
    let filtered = S.filter (\edge -> ecfg_edge_label edge == Just l) labeled in
      if S.null filtered then
        Nothing
      else
        emitLog ("FILTERING EXCEPTION " ++ l) $ Just $ withState filtered


-- Symbolic execution of a single vertex
-- 1.) Normal execution assumes that no called function (other than __cxa_throw) throws. Based on the info of the vertex, the estatus may change. 
-- 2.) Exceptional execution checks if there is any throwing function called. If so, set the estatus to Unwinding(...) and the rip to the address of the calling instruction. 
sym_exec_ecfg_vertex l@(_,_,l0) ecfg entry blockID v@(ECFG_Vertex _ _ info calls) s = do
  normal      <- sym_exec_normal
  exceptional <- sym_exec_exceptional
  return $ S.union normal exceptional
 where
  sym_exec_exceptional = do -- S.fromList $ map (\(a,_) -> s { ss_estatus = Unwinding "(...)" : ss_estatus s, ss_rip = a } ) get_calls
    posts  <- mapM get_posts_for_call $ S.toList calls 
    throws <- mapM mk_throw $ S.toList $ S.unions posts
    return $ S.unions throws

  mk_throw (a0,a1,Unwinding e) = do
    let msg = "CALL TO 0x" ++ showHex a1 ++ " AT 0x" ++ showHex a0 ++ " THROWS " ++ exception_type e
    emitLog msg $ return $ S.singleton $ s { ss_estatus = Unwinding e : ss_estatus s, ss_rip = a0 }
  mk_throw _                = return S.empty

  get_posts_for_call (a0,a1) = do
    posts <- sym_exec_ecfg_entry l False a1
    return $ S.map (\posts -> (a0,a1,posts)) posts





  sym_exec_normal = S.singleton <$> foldM go s info
  go s (ECFG_CF f) = return $ sym_exec_call f s
  go s (ECFG_Return Nothing)
    | head (ss_estatus s) == Normal = return $ s { ss_estatus = Returned : tail (ss_estatus s) }
    | otherwise                     = error $ "Entry: 0x" ++ showHex entry ++ "\nReturning while status is not Normal at vertex " ++ show v


  go s _ = return s

  sym_exec_call Terminate    (ECFG_SymState _               rip) = ECFG_SymState [Terminated]     rip
  sym_exec_call Resume       (ECFG_SymState (Landed   e:es) rip) = emitLog ("RESUME: " ++ exception_type e) ECFG_SymState (Unwinding e:es) rip
  sym_exec_call CatchBegin   (ECFG_SymState (Landed   e:es) rip) = ECFG_SymState (Handling e:es)  rip
  sym_exec_call CatchEnd     (ECFG_SymState es              rip) = ECFG_SymState (popHandling es) rip
  sym_exec_call (Rethrow a)  (ECFG_SymState (Handling e:es) rip) = emitLog ("RETHROWING AT 0x" ++ showHex a ++ ": " ++ exception_type e) $ ECFG_SymState (Unwinding e:Handling e:es) a
  sym_exec_call (Throw a e)  (ECFG_SymState es              rip) = emitLog ("THROWING AT 0x" ++ showHex a ++ ": " ++ exception_type e)   $ ECFG_SymState (Unwinding e:es) a
  sym_exec_call call         (ECFG_SymState _               rip) = ECFG_SymState [WeirdError $ "Entry 0x" ++ showHex entry ++ ": cannot execute " ++ show call ++ " in state " ++ show s ] rip

  popHandling (Handling e:es) = es
  popHandling (e:es) = e : popHandling es



sym_exec_ecfg_edge :: ECFG -> ECFG_Edge -> ECFG_SymState -> (Int, ECFG_SymState)
sym_exec_ecfg_edge ecfg _ s@(ECFG_SymState (Terminated :_)  _) = error $ "Should not run in terminated state."
sym_exec_ecfg_edge ecfg _ s@(ECFG_SymState (Unwinding e:_) _)  = error $ "Should not run in unwinding state." 
sym_exec_ecfg_edge ecfg _ s@(ECFG_SymState (Returned   :_) _)  = error $ "Should not run in returning state." 
sym_exec_ecfg_edge ecfg (ECFG_Edge blocKID label blockID') s  = (blockID',s{ss_rip = ecfg_blockID_to_address ecfg blockID'})


-- TODO recursive
sym_exec_unwind :: ECFG -> ECFG_Vertex -> ECFG_SymState -> (S.Set ECFG_SymState, S.Set (Int,ECFG_SymState))
sym_exec_unwind ecfg v@(ECFG_Vertex _ _ _ _) s@(ECFG_SymState (Unwinding e:es) _) = 
  case find is_encompassing_region $ ecfg_regions ecfg of
    Nothing -> 
      case es of
        (Landed e0:_) -> let msg = "Entry 0x" ++ showHex (ecfg_entry ecfg) ++ ": Uncaught exception " ++ exception_type e ++ " between landing but before handling exception " ++ exception_type e0 ++ " at " ++ show v in
                           emitLog msg $ (S.singleton $ s {ss_estatus = [WeirdError msg]} , S.empty)
        _             -> emitLog ("NO LANDING PAD " ++ show v) $ (S.singleton s, S.empty)
    Just r  -> emitLog ("LANDING FROM " ++ show v ++ " TO 0x" ++ showHex (ecfg_region_landingpad r)) $ (S.empty, S.singleton $ (get_blockID_for_address ecfg (fromIntegral $ ecfg_region_landingpad r), s {ss_rip = fromIntegral (ecfg_region_landingpad r), ss_estatus = Landed e:es }))
 where
   is_encompassing_region (ECFG_Region end start lp action color indx) = start <= ss_rip s && ss_rip s < end
sym_exec_unwind _ _ s = (S.singleton s, S.empty)


get_blockID_for_address ecfg a =
  case find (\(_,ECFG_Vertex block_start _ _ _) -> block_start == a) $ IM.assocs $ ecfg_vertices ecfg of
    Just (blockID,_) -> blockID
    Nothing -> error $ show ecfg ++ "\nUnknown address 0x" ++ showHex a











-- Rendering an ECFG to dot
render_ecfg_to_dot (ECFG vertices edges entry regions) = 
    intercalate "\n" $ [dot_start , dot_nodes vertices, dot_edges edges, dot_end]
 where
  dot_start = intercalate "\n" 
    [ "digraph ECFG {"
    , "  rankdir=LR;  // left-to-right layout (optional)"
    , "  node ["
    , "  shape=plaintext,"
    , "  fontname=\"Helvetica\""
    , "  ];"
    ]

  dot_end = "}"

  -- Edges
  dot_edges = intercalate "\n" . map mk_edge

  mk_edge (ECFG_Edge blockID label blockID') = "  " ++ show blockID ++ " -> " ++ show blockID' ++ mk_label label ++ ";"

  mk_label Nothing   = "" 
  mk_label (Just l) = "[label=\"" ++ l ++ "\"]"

  -- Nodes
  dot_nodes = intercalate "\n" . map mk_node . IM.assocs

  mk_node (blockID, ECFG_Vertex block_start block_end info calls) = 
    let label_elts = mapMaybe mk_init info ++ mapMaybe mk_landing_pad info ++ mapMaybe (mk_region_color block_start block_end) info ++ mapMaybe mk_call info ++ mapMaybe mk_end info
        label      = mk_HTML_label $ [("0x" ++ showHex block_start,"")] ++ filter (\(txt,color) -> txt /= "") label_elts in
      "  " ++ show blockID ++ " [label=" ++ label ++ "];"

  mk_HTML_label strs = "< <TABLE BORDER=\"1\" CELLBORDER=\"1\" CELLSPACING=\"0\">" ++ concatMap mk_HTML_cell strs ++ "</TABLE> >"

  mk_HTML_cell (str,"")    = "<TR><TD>" ++ str ++ "</TD></TR>"
  mk_HTML_cell (str,color) = "<TR><TD bgcolor=\"" ++ color ++ "\"><FONT COLOR=\"" ++ hex_color_of_text color ++ "\">" ++ str ++ "</FONT></TD></TR>"

  -- Start
  mk_init (ECFG_Start) = Just ("<B>START</B>", "")
  mk_init _            = Nothing 

  -- Landing pad
  mk_landing_pad (ECFG_LandingPad color True indx)  = Just ("CLEANUP-ONLY<BR/>LANDING PAD" ++ mk_table_indx indx, color)
  mk_landing_pad (ECFG_LandingPad color False indx) = Just ("LANDING PAD" ++ mk_table_indx indx, color)
  mk_landing_pad _                                  = Nothing

  mk_table_indx 0    = ""
  mk_table_indx indx = "<sup>" ++ show indx ++ "</sup>"

  -- Region
  mk_region_color block_start block_end (ECFG_Vertex_Region (ECFG_Region end start lp action color indx)) =
    let s = mk_region_color_start start block_start
        e = mk_region_color_end end block_end
        t = mk_table_indx indx
        a = if s /= "" && e == "" then " &#8677; " else if s == "" || e == "" then "" else if s /= "" || e /= "" then " &rarr; " else " &#8614;; " in
      Just ("REGION" ++ t ++ s ++ a ++ e, color)
  mk_region_color block_start block_end _ = Nothing

  mk_region_color_start start block_start
    | block_start < start = " 0x" ++ showHex start
    | otherwise           = ""

  mk_region_color_end end block_end
    | end < block_end = " 0x" ++ showHex end
    | otherwise       = ""

  -- Calls
  mk_call (ECFG_CF f) = Just (mk_dot_safe $ show f,"")
  mk_call _           = Nothing



  -- End
  mk_end (ECFG_Return Nothing)  = Just ("Return", "")
  mk_end (ECFG_Return (Just a)) = Just ("Return by 0x" ++ showHex a, "")
  mk_end _                      = Nothing









-- Creating an ECFG from a CFG
cfg_to_ecfg :: Lifted -> Word64 -> CFG -> ECFG
cfg_to_ecfg l@(bin,config,l0) entry cfg =
  let cfg' = cfg_compress l entry cfg in
    ECFG (mk_vertices cfg') (mk_edges cfg') entry $ concatMap mk_regions all_relevant_tables
 where
  -- Vertices 
  mk_vertices cfg' = IM.mapWithKey (mk_vertex cfg') $ cfg_blocks cfg'

  mk_vertex cfg' blockID instrs = 
    let block_start = blockID_to_address cfg blockID
        block_end   = blockID_to_end_address blockID
        t           = find_gcc_except_table_for_blockID blockID
        info        = mk_init blockID ++ mk_landing_pad t blockID ++ mk_region_colors t blockID instrs ++ mk_relevant_calls blockID ++ mk_end blockID 
        calls       = mk_calls blockID cfg' in
      ECFG_Vertex block_start block_end info calls

  -- Start
  mk_init 0 = [ECFG_Start]
  mk_init _ = []

  -- Landing pad
  mk_landing_pad Nothing blockID = []
  mk_landing_pad (Just t) blockID =
    case find (\(ECFG_Region end start lp action color indx) -> lp == blockID_to_address cfg blockID) $ mk_regions t of
      Nothing -> []
      Just (ECFG_Region end start lp 0      color indx) -> [ECFG_LandingPad color True indx]
      Just (ECFG_Region end start lp action color indx) -> [ECFG_LandingPad color False indx]

  -- Region
  mk_region_colors Nothing  blockID instrs = []
  mk_region_colors (Just t) blockID instrs = map ECFG_Vertex_Region $ nub $ concatMap (get_regions_for_address t) $ blockID_to_addresses cfg blockID 

  -- Calls
  mk_relevant_calls blockID = concatMap (annotate_relevant_call blockID) $ get_external_calls l cfg blockID

  annotate_relevant_call blockID (a,"_Unwind_Resume")         = [ECFG_CF Resume]
  annotate_relevant_call blockID (a,"_Unwind_RaiseException") = [ECFG_CF Raise]
  annotate_relevant_call blockID (a,"__cxa_begin_catch")      = [ECFG_CF CatchBegin]
  annotate_relevant_call blockID (a,"__cxa_end_catch")        = [ECFG_CF CatchEnd]
  annotate_relevant_call blockID (a,"__cxa_rethrow")          = [ECFG_CF $ Rethrow a]
  annotate_relevant_call blockID (a,"__cxa_throw")            =
    let a0        = blockID_to_address cfg blockID
        as        = IS.singleton $ blockID_to_last_address blockID
        l         = (bin,config,l0,fromIntegral entry) 
        symstates = unsafePerformIO $ evalStateT (symbolically_execute_until l a0 as init_symstate) 0
        tinfos    = S.toList $ S.map (try_get_type_info . evalState (sread_reg (Reg64 RSI))) symstates
        rdxs      = S.toList $ S.map (try_get_label . evalState (sread_reg (Reg64 RDX))) symstates in
      if length tinfos /= 1 || length rdxs /= 1 then
        error $ intercalate "," $ map show $ tinfos
      else let e_type  = strip_typeinfo_for $ fst $ head tinfos
               e_base  = strip_typeinfo_for <$> (snd $ head tinfos)
               e_destr = whenNotNull $ head rdxs
               e       = Exception e_type e_base e_destr in
        [ECFG_CF $ Throw a e]
  annotate_relevant_call _ (a,f)
    -- https://gcc.gnu.org/onlinedocs/libstdc++/libstdc++-api-4.5/a00874.html
    | "std::__throw_" `isPrefixOf` (demangle f `orElse` f) =
      let e = Exception ("std::" ++ (takeWhile ((/=) '(') $ drop 13 $ demangle f `orElse` f)) (Just "std::exception") Nothing in
        [ECFG_CF $ Throw a e]
    | is_exiting_function_call f = [ECFG_CF Terminate]
    | otherwise  = []

  -- e is a symbolic expression that should just be an immediate pointer into the data section
  -- We expect the following data:
  -- e:
  -- 	vtable for ... + 0x10
  -- e+8:
  -- 	ptr to string with name of 
  -- e+16:
  -- 	typeinfo for base class
  try_get_type_info e =
    let label = try_get_label e
        base  = try_get_pointee $ simp $ SE_Op Plus 64 [e, SE_Immediate 16] in
      --if not $ "typeinfo for " `isPrefixOf` label then
      --  error $ "RSI does not contain address of typeinfo object when throwing. It is: " ++ show label ++ " storing " ++ show (try_get_pointee e)
      if "typeinfo for " `isPrefixOf` base then
        (label,Just base)
      else
        (label,Nothing)

  try_get_label (SE_Immediate imm)                    = address_to_label bin imm
  try_get_label (SE_Var (SP_Mem (SE_Immediate a) si)) = address_to_pointee bin $ Absolute a
  try_get_label e                                     = "0x" ++ show e -- bit ugly...

  try_get_pointee (SE_Immediate imm)                  = address_to_pointee bin $ Absolute imm
  try_get_pointee e                                   = "*" ++ show e

  whenNotNull "0x0" = Nothing
  whenNotNull str   = Just str

  -- Internal calls
  -- Retrieve all calls in all blocks from the current one up to (but excluding) the frontier
  mk_calls blockID cfg' =
    let frontier = post cfg' blockID
        blockIDs = graph_traverse_downwards cfg blockID frontier in
      S.fromList $ mapMaybe functionMayThrow $ concatMap (get_calls_from_blockID l cfg) $ IS.toList blockIDs

  functionMayThrow (i,ImmediateAddress a)
    | not (isJump (inOperation i) && jump_is_actually_a_call l i) = Just (inAddress i,a)
    | otherwise = Nothing
  functionMayThrow _  = Nothing

  -- End
  mk_end blockID
    | IS.null (intgraph_post cfg blockID) = 
      let i = last $ cfg_instrs cfg IM.! blockID in
        if isRet $ inOperation i then
          [ECFG_Return Nothing]
        else if isJump (inOperation i) && jump_is_actually_a_call l i then
          case jump_target_for_instruction bin i of 
            ImmediateAddress a -> [ECFG_Return $ Just a]
            _                  -> [ECFG_Return Nothing]
        else 
          []
    | otherwise = []


  -- Edges
  mk_edges = concatMap mk_edge . IM.assocs . cfg_edges

  mk_edge (blockID,blockIDs) =
    case find_gcc_except_table_for_blockID blockID of
      Nothing -> map (ECFG_Edge blockID Nothing) $ IS.toList blockIDs
      Just t  -> case IS.toList $ blockID_to_landing_pads t blockID of
                  []  -> map (ECFG_Edge blockID Nothing) $ IS.toList blockIDs
                  [a] -> case find (\(ECFG_Region end start lp action color indx) -> lp == a) $ mk_regions t of
                           Nothing -> map (ECFG_Edge blockID Nothing) $ IS.toList blockIDs
                           Just (ECFG_Region end start lp 0         color indx) -> map (ECFG_Edge blockID Nothing) $ IS.toList blockIDs
                           Just (ECFG_Region end start lp cs_action color indx) -> if cs_action > 0 then mk_labeled_edges (obtain_typeinfo t (fromIntegral cs_action) blockID blockIDs) (blockID,blockIDs) else error $ "Negative call-site action"

  mk_labeled_edges info (blockID,blockIDs) = map (mk_labeled_edge info blockID) $ IS.toList blockIDs

  mk_labeled_edge info blockID blockID' = 
    case find (\(_,type_info,rips) -> S.size rips == 1 && blockID_to_address cfg blockID' `S.member` rips) info of
      Nothing -> ECFG_Edge blockID (Just "(none)") blockID'
      Just (_,type_info,_) -> ECFG_Edge blockID (Just $ if type_info == "*NULL" then "(...)" else type_info) blockID'



  blockID_to_last_address blockID = fromIntegral $ inAddress $ last $ cfg_instrs cfg IM.! blockID
  blockID_to_end_address blockID = 
    let i = last $ cfg_instrs cfg IM.! blockID in
      inAddress i + fromIntegral (inSize i)


  !all_relevant_tables = zip [0..] $ get_relevant_tables bin cfg

  blockID_to_landing_pads (id,t) blockID = 
    let landing_pads = get_landing_pads_from_gcc_except_table t
        block_as     = IS.fromList $ blockID_to_addresses cfg blockID in
      IS.intersection block_as landing_pads

  find_gcc_except_table_for_blockID blockID = 
    case filter (\(id,t) -> table_has_overlapping_region_or_landing_pad_for_block cfg t blockID) all_relevant_tables of
      []  -> Nothing
      [t] -> Just t
      ts   -> error $ "BlockID " ++ show blockID ++ showHex_list (blockID_to_addresses cfg blockID) ++ " has multiple gcc_except_tables:\n" ++ show ts


  get_regions_for_address t a = filter (is_region_for a) $ mk_regions t

  is_region_for a (ECFG_Region end start lp action color indx) = a >= start &&  a < end

  mk_regions (id,t) = 
    let regions             = get_callsite_regions_from_gcc_except_table t
        regions_with_lp     = filter (\(end,start,lp,action) -> lp /= fromIntegral entry) $ regions
        landing_pads        = map (\(end,start,lp,action) -> lp) regions_with_lp
        landing_pads_colors = IM.fromList $ map (\(indx,lp) -> (lp, hex_colors !! (indx `mod` length hex_colors))) $ zip [0..] landing_pads in
      map (mk_callsite_region id landing_pads_colors) regions_with_lp

  mk_callsite_region id landing_pads_colors (end,start,lp,action)  = ECFG_Region end start lp action (landing_pads_colors IM.! lp) id

  obtain_typeinfo (id,t) cs_action blockID blockIDs =
    let a  = blockID_to_address cfg blockID
        as = IS.map (blockID_to_address cfg) blockIDs
        l  = (bin,config,l0,fromIntegral entry) 
        indexed_types_infos = gcc_except_table_action_indx_to_type_info t cs_action in
      map (get_trgt_from_indexed_type l a as) $ indexed_types_infos

  get_trgt_from_indexed_type l a as (rdx,type_info) =
    let symstates = unsafePerformIO $ evalStateT (symbolically_execute_until l a as (init_sym_state_with (Reg64 RDX) $ fromIntegral rdx)) 0
        rips      = S.map read_RIP symstates in
      (a,strip_typeinfo_for $ address_to_pointee bin type_info,rips)


  strip_typeinfo_for str
    | "typeinfo for " `isPrefixOf` str = drop 13 str
    | otherwise = str

address_to_pointee bin (Absolute 0) = "*NULL"
address_to_pointee bin (Absolute a) =
  case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
    Just sym@(PointerToExternalFunction f)    -> demangle f `orElse` f
    Just sym@(PointerToObject f _ 0 _)        -> demangle f `orElse` f
    Just sym@(Relocated_ResolvedObject f _ 0) -> demangle f `orElse` f
    _ -> case find (\(Relocation a0 a1) -> a0 == a) $ binary_get_relocations bin of
           Just (Relocation a0 a1) -> address_to_label bin a1
           _ -> "&0x" ++ showHex a

address_to_label bin a = 
 case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
   Just sym@(PointerToObject _ _ _ (Just l)) -> demangle l `orElse` l
   Just sym@(AddressOfLabel l _)             -> demangle l `orElse` l
   Just sym@(AddressOfObject l _)            -> demangle l `orElse` l
   _                                         -> "0x" ++ showHex a
      

ecgf_unfold_jumps_to_function_entries :: Lifted -> ECFG -> ECFG
ecgf_unfold_jumps_to_function_entries l@(bin,config,l0) = repeatUtilFixpoint unfold
 where
  unfold ecfg = 
    let jmps = mapMaybe get_jump_into_function $ IM.assocs $ ecfg_vertices ecfg in
      foldr unfold_vertex ecfg jmps

  get_jump_into_function v@(blockID,ECFG_Vertex _ _ info _) 
    | any is_jump_into_function info = Just v
    | otherwise = Nothing

  is_jump_into_function (ECFG_Return (Just a)) = True
  is_jump_into_function _ = False

  unfold_vertex (blockID,ECFG_Vertex _ _ info _) ecfg =
    let [ECFG_Return (Just a)] = filter is_jump_into_function info
        (maxBlockID,_)         = IM.findMax $ ecfg_vertices ecfg
        cfg_child              = cfg_split_jumps $ l0_get_cfgs l0 IM.! fromIntegral a
        ecfg_child             = increaseBlockIDs (maxBlockID+1) $ cfg_to_ecfg l a cfg_child
        ecfg_parent            = ecfg { ecfg_vertices = IM.adjust removeReturn blockID (ecfg_vertices ecfg), ecfg_edges = (ECFG_Edge blockID Nothing (maxBlockID+1)) : ecfg_edges ecfg }
        ecfg_merge             = ECFG (IM.union (ecfg_vertices ecfg_parent) (ecfg_vertices ecfg_child))
                                      (ecfg_edges ecfg_parent ++ ecfg_edges ecfg_child)
                                      (ecfg_entry ecfg_parent)
                                      (ecfg_regions ecfg_parent ++ ecfg_regions ecfg_child) in
      ecfg_merge

  removeReturn (ECFG_Vertex block_start block_end info calls) = ECFG_Vertex block_start block_end (filter (not . is_jump_into_function) info) calls
        
  increaseBlockIDs increment ecfg = ecfg { ecfg_vertices = IM.mapKeys ((+) increment) (ecfg_vertices ecfg), ecfg_edges = map (increaseBlockIDs_edge increment) (ecfg_edges ecfg) }

  increaseBlockIDs_edge increment (ECFG_Edge start label end) = ECFG_Edge (start+increment) label (end+increment)






blockID_to_address cfg blockID = fromIntegral $ inAddress $ head $ cfg_instrs cfg `safeLookup` blockID
blockID_to_addresses cfg blockID = map (fromIntegral . inAddress) $ cfg_instrs cfg IM.! blockID



get_relevant_tables bin cfg = filter (table_has_overlapping_region_or_landing_pad cfg) all_tables
 where
  all_tables = IM.elems $ cfi_gcc_except_tables $ binary_get_cfi bin


table_has_overlapping_region_or_landing_pad cfg t = 
  let regions = get_callsite_regions_from_gcc_except_table t
      as      = IM.keysSet $ cfg_addr_to_blockID cfg in -- all instruction addresses
   any (\(end,start,lp,_) -> lp `IS.member` as || (not $ IS.disjoint as $ IS.fromRange (start,end-1))) regions


table_has_overlapping_region_or_landing_pad_for_block cfg t blockID = 
  let regions = get_callsite_regions_from_gcc_except_table t
      as      = IS.fromList $ cfg_blocks cfg IM.! blockID in -- all instruction addresses
   any (\(end,start,lp,_) -> lp `IS.member` as || (not $ IS.disjoint as $ IS.fromRange (start,end-1))) regions





-- Compresses a CFG, i.e., removes node that are irrelevant.
-- Removing a node means connecting all parents to all children
-- A node is relevant if it either:
-- 1.) is a start or an end node
-- 2.) contains calls to, e.g., __cxa_throw
-- 3.) has CFI directives
-- 4.) is the beginning or end of a call-site region 
cfg_compress :: BinaryClass bin => Lifting bin pred finit v -> Word64 -> CFG -> CFG
cfg_compress l@(bin,_,_) entry cfg0 = foldr maybe_remove_node cfg0 $ IM.keys $ cfg_edges cfg0
 where
  maybe_remove_node blockID cfg
    | IS.null (intgraph_pre  cfg blockID)                        = cfg
    | IS.null (intgraph_post cfg blockID)                        = cfg
    | relevant_calls l cfg blockID /= []                         = cfg
    | block_has_cfi_directive cfg blockID                        = cfg 
    | block_contains_region_start_or_end cfg all_regions blockID = cfg
    -- TODO or end of basic block has cfi_directive end is not start of another?
    | otherwise                                                  = remove_node blockID cfg


  relevant_calls l cfg blockID = filter is_ecfg_relevant $ get_external_calls l cfg blockID

  block_has_cfi_directive cfg blockID = any address_has_cfi_directive $ map (fromIntegral . inAddress) $ cfg_instrs cfg IM.! blockID


  all_regions = concatMap get_callsite_regions_from_gcc_except_table $ cfi_gcc_except_tables $ binary_get_cfi bin


  address_has_cfi_directive a = IM.lookup a (cfi_directives $ binary_get_cfi bin) /= Nothing


  instruction_overlaps_region regions i = any (region_contains_address $ inAddress i) regions
  region_contains_address a (end,start,_,_) = start <= a && a < end

  block_contains_region_start_or_end cfg regions blockID =
    let instrs            = cfg_instrs cfg IM.! blockID
        last_i            = last instrs
        block_end_address = fromIntegral (inAddress last_i) + fromIntegral (inSize last_i)
        block_addresses   = block_end_address : (map (fromIntegral . inAddress) $ drop 1 instrs) in
      any (\(end,start,_,_) -> start `elem` (map inAddress instrs) || end `elem` block_addresses) regions

  remove_node blockID cfg = 
    let parents  = IS.delete blockID $ intgraph_pre cfg blockID
        children = IS.delete blockID $ intgraph_post cfg blockID
        prod     = filter (\(x,y) -> x /= y) $ [(x,y) | x <- IS.toList parents, y <- IS.toList children]
        cfg0     = delete_node blockID cfg in
      foldr add_new_edge cfg0 prod

  add_new_edge (parent,child) cfg  = cfg { cfg_edges = IM.insertWith IS.union parent (IS.singleton child) (cfg_edges cfg) }

  delete_node blockID cfg = cfg { cfg_edges = IM.map (IS.delete blockID) $ IM.delete blockID $ cfg_edges cfg, cfg_blocks = IM.delete blockID $ cfg_blocks cfg }






is_ecfg_relevant (a,f) = (f `notElem` ["__cxa_finalize", "__cxa_allocate_exception", "__cxa_free_exception"] && "__cxa_" `isPrefixOf` f) || f `elem` ["_Unwind_Resume", "_Unwind_RaiseException", "_Unwind_ForcedUnwind"] || "std::__throw_" `isPrefixOf` (demangle f `orElse` f)

get_external_calls l cfg blockID = concatMap get_external_call $ get_calls_from_blockID l cfg blockID
 where
  get_external_call (i,External f) = [(inAddress i,f)]
  get_external_call _ = []

get_calls_from_blockID l cfg blockID = get_call_target (cfg_instrs cfg IM.! blockID)
 where
  get_call_target instrs
    | is_call $ last instrs = zip (repeat (last instrs)) (get_known_jump_targets l $ last instrs)
    | otherwise             = []

  is_call i = isCall (inOperation i) || (isJump (inOperation i) && jump_is_actually_a_call l i)


