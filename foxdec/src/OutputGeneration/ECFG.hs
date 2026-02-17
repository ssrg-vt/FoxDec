{-# LANGUAGE PartialTypeSignatures, StrictData #-}

{-|
Module      : ControlFlow
Description : Contains functions pertaining to control flow graph generation.
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
    exception_type :: String
  , exception_destructor :: Maybe String
  }
  deriving (Ord,Eq)

instance Show Exception where
  show (Exception typ Nothing)      = typ 
  show (Exception typ (Just destr)) = typ ++ "," ++ destr

data ECFG_Function = Terminate | Resume | Raise | CatchBegin | CatchEnd | Rethrow Word64 | Throw Word64 Exception

show_exception_as_arg (Exception typ Nothing)  = "(" ++ typ ++ ")"
show_exception_as_arg (Exception typ (Just _)) = "*(" ++ typ ++ ")"

instance Show ECFG_Function where
  show Terminate = "Terminate"
  show Resume = "Resume"
  show Raise = "Raise"
  show CatchBegin = "CatchBegin"
  show CatchEnd = "CatchEnd"
  show (Rethrow a) = "Rethrow"
  show (Throw a e) = "Throw" ++ show_exception_as_arg e

data ECFG_Vertex_Info =
    ECFG_Start                     -- ^ Initial vertex
  | ECFG_LandingPad String Bool    -- ^ A landing pad with color (bool==True indicates cleanup only)
  | ECFG_Call ECFG_Function        -- ^ A call to a relevant function (throw, catchBegin, etc.)
  | ECFG_Return                    -- ^ Return
  | ECFG_Vertex_Region ECFG_Region -- ^ A region
  deriving (Show)


data ECFG_Region = ECFG_Region {
    ecfg_region_end :: Word64
  , ecfg_region_start :: Word64
  , ecfg_region_landingpad :: Int
  , ecfg_region_action :: Word64
  , ecfg_region_color :: String
  }
  deriving (Eq,Show)

data ECFG_Vertex = ECFG_Vertex Word64 Word64 [ECFG_Vertex_Info]

instance Show ECFG_Vertex where
  show (ECFG_Vertex start end info) = "Block 0x" ++ showHex start

data ECFG_Edge = ECFG_Edge {
    ecfg_edge_start :: Int
  , ecfg_edge_label :: Maybe String
  , ecfg_edge_end   :: Int
  }
  deriving (Ord,Eq,Show)

data ECFG = ECFG {
    ecfg_vertices :: IM.IntMap ECFG_Vertex
  , ecfg_edges :: [ECFG_Edge]
  , ecfg_entry :: Word64
  , ecfg_regions :: [ECFG_Region]
  }




-- SYMBOLIC EXECUTION
type CallStack = [Word64]

data EStatus = Normal | Terminated | Returned | Unwinding Exception | Landed Exception | Handling Exception
  deriving (Ord,Eq)

instance Show EStatus where
  show Normal = "Normal"
  show Terminated = "Terminated"
  show Returned = "Returned"
  show (Unwinding e) = "Unwinding" ++ show_exception_as_arg e
  show (Landed e) = "Landed" ++ show_exception_as_arg e
  show (Handling e) = "Handling" ++ show_exception_as_arg e

data ECFG_SymState = ECFG_SymState {
    ss_callstack :: CallStack
  , ss_estatus :: [EStatus]
  , ss_rip :: Word64
  }
  deriving (Ord,Eq)


instance Show ECFG_SymState where
  show (ECFG_SymState _ es a) = "0x" ++ showHex a ++ ": " ++ show es


emitLog msg = id -- trace msg

ecfg_vertex_to_address (ECFG_Vertex a _ _) = a

ecfg_blockID_to_address (ECFG vertices _ _ _) blockID = ecfg_vertex_to_address $ vertices IM.! blockID

init_ecfg_symstate ecfg blockID = ECFG_SymState [] [Normal] $ ecfg_blockID_to_address ecfg blockID


sym_exec_cfg_all :: Lifted -> IO ()
sym_exec_cfg_all l = do
  let fs = get_call_graph_sources l
  mapM_ run_entry $ IS.toList fs
 where
  run_entry entry = runStateT (sym_exec_ecfg_entry l True (fromIntegral entry)) init_StateSoFar


sym_exec_ecfg_entry :: Lifted -> Bool -> Word64 -> StateT StateSoFar IO ()
sym_exec_ecfg_entry l@(bin,config,l0) isFirst entry = do
  posts <- gets ssf_function_posts
  case IM.lookup (fromIntegral entry) posts of
    Just posts -> do
      let msg = "Entry 0x" ++ showHex entry ++ " already visited."
      return $ emitLog msg ()
    Nothing    -> do
      putIfFirst $ "-----------------------------"
      putIfFirst $ "Entry: 0x" ++ showHex entry
      let msg  = "Entry 0x" ++ showHex entry ++ " to be explored."
      let cfg  = emitLog msg $ l0_get_cfgs l0 IM.! fromIntegral entry
      let ecfg = cfg_to_ecfg l entry cfg
      let q    = overapproximate_post l entry ecfg
      modify $ ssf_add_function_post entry q
      modify $ ssf_clear_preconditions
      sym_exec_ecfg l ecfg
      posts <- gets $ ssf_get_posts entry
      let msg = "Entry 0x" ++ showHex entry ++ " exploration done: " ++ show (S.toList posts)
      ssf <- emitLog msg get
      putIfFirst $ ssf_pp ssf
      putIfFirst $ "-----------------------------"
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

  ecfg_vertex_to_post (ECFG_Vertex _ _ info) q = foldr ecfg_vertex_info_to_post q info

  ecfg_vertex_info_to_post (ECFG_Call Terminate)   = S.insert $ Terminated
  ecfg_vertex_info_to_post (ECFG_Call (Throw a e)) = S.insert $ Unwinding e
  ecfg_vertex_info_to_post (ECFG_Return)           = S.insert $ Returned
  ecfg_vertex_info_to_post _                       = id




post_to_estatus (ECFG_SymState _ (e@(Returned)   :_) _) = e
post_to_estatus (ECFG_SymState _ (e@(Unwinding _):_) _) = e
post_to_estatus (ECFG_SymState _ (e@(Terminated ):_) _) = e
post_to_estatus s = error $ "Post = " ++ show s

data StateSoFar = StateSoFar {
    ssf_function_posts :: IM.IntMap (S.Set EStatus)   -- ^ Currently known postconditions for functions
  , ssf_block_pres :: IM.IntMap (S.Set ECFG_SymState) -- ^ Preconditions for blocks current function
  }

init_StateSoFar = StateSoFar IM.empty IM.empty

ssf_pp (StateSoFar fp bp) = "FUNCTION POSTCONDITIONS:\n" ++ (intercalate "\n" $ map show_fp_entry $ IM.assocs fp) 
 where
  show_fp_entry (entry,ess) = "0x" ++ showHex entry ++ ": " ++ intercalate ", " (map show $ S.toList ess)

ssf_clear_preconditions (StateSoFar fp bp) = StateSoFar fp IM.empty

ssf_add_preconditon blockID s (StateSoFar fp bp) = StateSoFar fp $ IM.insertWith S.union blockID (S.singleton s) bp

ssf_add_function_post entry p (StateSoFar fp bp) = StateSoFar (IM.insertWith S.union (fromIntegral entry) p fp) bp

ssf_get_posts entry (StateSoFar fp bp) = IM.lookup (fromIntegral entry) fp `orElse` S.empty


sym_exec_ecfg l ecfg = sym_exec_ecfg' l ecfg $ S.singleton (0,init_ecfg_symstate ecfg 0)

sym_exec_ecfg' l ecfg@(ECFG vertices edges entry regions) bag =
  case S.minView bag of
    Nothing          -> return ()
    Just ((blockID,s),bag) -> do
      curr_pres <- gets ssf_block_pres
      let curr_ss = IM.lookup blockID curr_pres `orElse` S.empty
      if s `S.member` curr_ss then
        sym_exec_ecfg' l ecfg bag
      else do
        let v                          = vertices IM.! blockID
        let out_edges                  = S.fromList $ filter (is_edge_from blockID) edges
        -- Execute the vertex
        ss'                           <- sym_exec_ecfg_vertex l ecfg entry blockID v (fromIntSet $ S.map ecfg_edge_end out_edges) s
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

  isUnwinding (ECFG_SymState _ (Unwinding _:_) _) = True
  isUnwinding _ = False

  isDoneOrUnwinding (ECFG_SymState _ (Unwinding _:_) _) = True
  isDoneOrUnwinding (ECFG_SymState _ (Returned   :_) _) = True
  isDoneOrUnwinding (ECFG_SymState _ (Terminated :_) _) = True
  isDoneOrUnwinding _                                   = False


-- TODO
applyFilter edges s
  | S.null labeled         = withState edges
  | not $ S.null unlabeled = error $ "Do not know how to give semantics to edges: " ++ show edges
  | otherwise =
    case ss_estatus s of
      (Landed e:_)   -> try_label (exception_type e) `orTry` try_label "(...)" `orTry` try_label "(none)" `orElse` fail
      (Handling e:_) -> try_label "(...)"  `orElse` fail
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
sym_exec_ecfg_vertex l@(_,_,l0) ecfg entry blockID v@(ECFG_Vertex _ _ info) frontier s = do
  let normal   = sym_exec_normal
  exceptional <- sym_exec_exceptional
  return $ S.union sym_exec_normal exceptional
 where
  sym_exec_exceptional = do -- S.fromList $ map (\(a,_) -> s { ss_estatus = Unwinding "(...)" : ss_estatus s, ss_rip = a } ) get_calls
    posts  <- mapM get_posts_for_call get_calls
    throws <- mapM mk_throw $ S.toList $ S.unions posts
    return $ S.unions throws

  mk_throw (a0,a1,Unwinding e) = do
    let msg = "CALL TO 0x" ++ showHex a1 ++ " AT 0x" ++ showHex a0 ++ " THROWS " ++ exception_type e
    emitLog msg $ return $ S.singleton $ s { ss_estatus = Unwinding e : ss_estatus s, ss_rip = a0 }
  mk_throw _                = return S.empty


  get_posts_for_call (a0,ImmediateAddress a1) = do
    sym_exec_ecfg_entry l False a1
    posts <- gets $ ssf_get_posts a1
    return $ S.map (\posts -> (a0,a1,posts)) posts

    
    

  -- Retrieve all calls in all blocks from the current one up to (but excluding) the frontier
  get_calls = 
    let cfg      = l0_get_cfgs l0 IM.! fromIntegral (ecfg_entry ecfg)
        blockIDs = graph_traverse_downwards cfg blockID frontier in
      filter functionMayThrow $ concatMap (get_calls_from_blockID l cfg) $ IS.toList blockIDs

  functionMayThrow (_,ImmediateAddress a) = True
  functionMayThrow (_,External f)         = False -- not $ is_ecfg_relevant f
  functionMayThrow _                      = False


  sym_exec_normal = S.singleton $ foldr go s info
  go (ECFG_Call f) s = sym_exec_call f s
  go (ECFG_Return) s
    | head (ss_estatus s) == Normal = s { ss_estatus = Returned : tail (ss_estatus s) }
    | otherwise                     = error $ "Returning while status is not Normal"
  go _             s = s

  sym_exec_call Terminate    (ECFG_SymState cs _               rip) = ECFG_SymState cs [Terminated]     rip
  sym_exec_call Resume       (ECFG_SymState cs (Landed   e:es) rip) = ECFG_SymState cs (Unwinding e:es) rip
  sym_exec_call CatchBegin   (ECFG_SymState cs (Landed   e:es) rip) = ECFG_SymState cs (Handling e:es)  rip
  sym_exec_call CatchEnd     (ECFG_SymState cs es              rip) = ECFG_SymState cs (popHandling es) rip
  sym_exec_call (Rethrow a)  (ECFG_SymState cs (Handling e:es) rip) = emitLog ("RETHROWING AT 0x" ++ showHex a ++ ": " ++ show (exception_type e)) $ ECFG_SymState cs (Unwinding e:Handling e:es) a
  sym_exec_call (Throw a e)  (ECFG_SymState cs es              rip) = emitLog ("THROWING AT 0x" ++ showHex a ++ ": " ++ show (exception_type e))   $ ECFG_SymState cs (Unwinding e:es) a
  sym_exec_call _ _ = s

  popHandling (Handling e:es) = es
  popHandling (e:es) = e : popHandling es



sym_exec_ecfg_edge :: ECFG -> ECFG_Edge -> ECFG_SymState -> (Int, ECFG_SymState)
sym_exec_ecfg_edge ecfg _ s@(ECFG_SymState _ (Terminated :_)  _) = error $ "Should not run in terminated state."
sym_exec_ecfg_edge ecfg _ s@(ECFG_SymState _ (Unwinding e:_) _)  = error $ "Should not run in unwinding state." 
sym_exec_ecfg_edge ecfg _ s@(ECFG_SymState _ (Returned   :_) _)  = error $ "Should not run in returning state." 
sym_exec_ecfg_edge ecfg (ECFG_Edge blocKID label blockID') s  = (blockID',s{ss_rip = ecfg_blockID_to_address ecfg blockID'})


-- TODO recursive
sym_exec_unwind :: ECFG -> ECFG_Vertex -> ECFG_SymState -> (S.Set ECFG_SymState, S.Set (Int,ECFG_SymState))
sym_exec_unwind ecfg v@(ECFG_Vertex _ _ _) s@(ECFG_SymState _ (Unwinding e:es) _) = 
  case find is_encompassing_region $ ecfg_regions ecfg of
    Nothing -> 
      case es of
        (Landed _:_) -> error $ "Uncaught exception between landing but before handling exception at " ++ show v
        _            -> emitLog ("NO LANDING PAD " ++ show v) $ (S.singleton s, S.empty)
    Just r  -> emitLog ("LANDING FROM " ++ show v ++ " TO 0x" ++ showHex (ecfg_region_landingpad r)) $ (S.empty, S.singleton $ (get_blockID_for_address ecfg (fromIntegral $ ecfg_region_landingpad r), s {ss_rip = fromIntegral (ecfg_region_landingpad r), ss_estatus = Landed e:es }))
 where
   is_encompassing_region (ECFG_Region end start lp action color) = start <= ss_rip s && ss_rip s < end
sym_exec_unwind _ _ s = (S.singleton s, S.empty)


get_blockID_for_address ecfg a =
  case find (\(_,ECFG_Vertex block_start _ _) -> block_start == a) $ IM.assocs $ ecfg_vertices ecfg of
    Just (blockID,_) -> blockID












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

  mk_node (blockID, ECFG_Vertex block_start block_end info) = 
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
  mk_landing_pad (ECFG_LandingPad color True)  = Just ("CLEANUP-ONLY<BR/>LANDING PAD", color)
  mk_landing_pad (ECFG_LandingPad color False) = Just ("LANDING PAD", color)
  mk_landing_pad _                             = Nothing

  -- Region
  mk_region_color block_start block_end (ECFG_Vertex_Region (ECFG_Region end start lp action color)) =
    let s = mk_region_color_start start block_start
        e = mk_region_color_end end block_end
        a = if s /= "" && e == "" then " &#8677; " else if s == "" || e == "" then "" else if s /= "" || e /= "" then " &rarr; " else " &#8614;; " in
      Just ("REGION" ++ s ++ a ++ e, color)
  mk_region_color block_start block_end _ = Nothing

  mk_region_color_start start block_start
    | block_start < start = " 0x" ++ showHex start
    | otherwise           = ""

  mk_region_color_end end block_end
    | end < block_end = " 0x" ++ showHex end
    | otherwise       = ""

  -- Calls
  mk_call (ECFG_Call f) = Just (mk_dot_safe $ show f,"")
  mk_call _             = Nothing



  -- End
  mk_end ECFG_Return = Just ("Return", "")
  mk_end _           = Nothing







-- Creating an ECFG from a CFG
cfg_to_ecfg :: Lifted -> Word64 -> CFG -> ECFG
cfg_to_ecfg l@(bin,config,l0) entry cfg =
  let cfg' = cfg_compress l entry cfg in
    ECFG (mk_vertices cfg') (mk_edges cfg') entry mk_regions
 where
  -- Vertices 
  mk_vertices cfg = IM.mapWithKey mk_vertex $ cfg_blocks cfg

  mk_vertex blockID instrs = 
    let block_start = blockID_to_address cfg blockID
        block_end   = blockID_to_end_address blockID
        info        = mk_init blockID ++ mk_landing_pad blockID ++ mk_region_colors blockID instrs ++ mk_relevant_calls blockID ++ mk_end blockID in
      ECFG_Vertex block_start block_end info

  -- Start
  mk_init 0 = [ECFG_Start]
  mk_init _ = []

  -- Landing pad
  mk_landing_pad blockID =
    case block_has_address_from blockID landing_pads of
      []  -> []
      [a] -> case find (\(ECFG_Region end start lp action color) -> lp == a) $ mk_regions of
               Nothing -> []
               Just (ECFG_Region end start lp 0      color)      -> [ECFG_LandingPad color True]
               Just (ECFG_Region end start lp action color) -> [ECFG_LandingPad color False]

  -- Region
  mk_region_colors blockID instrs = map ECFG_Vertex_Region $ nub $ concatMap get_regions_for_address $ blockID_to_addresses cfg blockID 

  -- Calls
  mk_relevant_calls blockID = concatMap (annotate_relevant_call blockID) $ get_external_calls l cfg blockID

  annotate_relevant_call blockID (a,"_Unwind_Resume")         = [ECFG_Call Resume]
  annotate_relevant_call blockID (a,"_Unwind_RaiseException") = [ECFG_Call Raise]
  annotate_relevant_call blockID (a,"__cxa_begin_catch")      = [ECFG_Call CatchBegin]
  annotate_relevant_call blockID (a,"__cxa_end_catch")        = [ECFG_Call CatchEnd]
  annotate_relevant_call blockID (a,"__cxa_rethrow")          = [ECFG_Call $ Rethrow a]
  annotate_relevant_call blockID (a,"__cxa_throw")            =
    let a0        = blockID_to_address cfg blockID
        as        = IS.singleton $ blockID_to_last_address blockID
        l         = (bin,config,l0,fromIntegral entry) 
        symstates = unsafePerformIO $ symbolically_execute_until l 0 a0 as init_symstate
        rsis      = S.toList $ S.map (try_get_label . evalState (sread_reg (Reg64 RSI))) symstates
        rdxs      = S.toList $ S.map (try_get_label . evalState (sread_reg (Reg64 RDX))) symstates in
      if any ("0x" `isPrefixOf`) rsis || length rsis /= 1 || length rdxs /= 1 then
        error $ intercalate "," rsis
      else let e_type  = strip_typeinfo_for $ head rsis
               e_destr = whenNotNull $ head rdxs
               e       = Exception e_type e_destr in
        [ECFG_Call $ Throw a e]
  annotate_relevant_call _ (a,f)
    | "std::__throw_" `isPrefixOf` (demangle f `orElse` f) =
      let e = Exception ("std::" ++ (takeWhile ((/=) '(') $ drop 13 $ demangle f `orElse` f)) Nothing in
        [ECFG_Call $ Throw a e]
    | is_exiting_function_call f = [ECFG_Call Terminate]
    | otherwise  = []

  try_get_label (SE_Immediate imm)                    = address_to_label imm
  try_get_label (SE_Var (SP_Mem (SE_Immediate a) si)) = address_to_pointee (Absolute a)
  try_get_label e                                     = "0x" ++ show e -- bit ugly...

  whenNotNull "0x0" = Nothing
  whenNotNull str   = Just str

  -- End
  mk_end blockID
    | IS.null (intgraph_post cfg blockID) = 
      let i = last $ cfg_instrs cfg IM.! blockID in
        if isRet $ inOperation i then
          [ECFG_Return]
        else 
          []
    | otherwise = []


  -- Edges
  mk_edges = concatMap mk_edge . IM.assocs . cfg_edges

  mk_edge (blockID,blockIDs) =
    case block_has_address_from blockID landing_pads of
      []  -> map (ECFG_Edge blockID Nothing) $ IS.toList blockIDs
      [a] -> case find (\(ECFG_Region end start lp action color) -> lp == a) $ mk_regions of
               Nothing -> map (ECFG_Edge blockID Nothing) $ IS.toList blockIDs
               Just (ECFG_Region end start lp 0         color) -> map (ECFG_Edge blockID Nothing) $ IS.toList blockIDs
               Just (ECFG_Region end start lp cs_action color) -> if cs_action > 0 then mk_labeled_edges (obtain_typeinfo (fromIntegral cs_action) blockID blockIDs) (blockID,blockIDs) else error $ "Negative call-site action"

  mk_labeled_edges info (blockID,blockIDs) = map (mk_labeled_edge info blockID) $ IS.toList blockIDs

  mk_labeled_edge info blockID blockID' = 
    case find (\(_,type_info,rips) -> S.size rips == 1 && blockID_to_address cfg blockID' `S.member` rips) info of
      Nothing -> ECFG_Edge blockID (Just "(none)") blockID'
      Just (_,type_info,_) -> ECFG_Edge blockID (Just $ if type_info == "*NULL" then "(...)" else type_info) blockID'



  blockID_to_last_address blockID = fromIntegral $ inAddress $ last $ cfg_instrs cfg IM.! blockID
  blockID_to_end_address blockID = 
    let i = last $ cfg_instrs cfg IM.! blockID in
      inAddress i + fromIntegral (inSize i)

  blockID_to_addresses cfg blockID = map (fromIntegral . inAddress) $ cfg_instrs cfg IM.! blockID

  t             = find (\t -> function_entry t == entry) $ cfi_gcc_except_tables $ binary_get_cfi bin
  landing_pads  = maybe IS.empty get_landing_pads_from_gcc_except_table t
  region_starts = maybe IS.empty get_callsite_region_starts_from_gcc_except_table t
  region_ends   = maybe IS.empty get_callsite_region_ends_from_gcc_except_table t


  block_has_address_from blockID as = filter (\a -> a `IS.member` as) $ map (fromIntegral . inAddress) $ cfg_instrs cfg IM.! blockID


  get_regions_for_address a = filter (is_region_for a) mk_regions 

  is_region_for a (ECFG_Region end start lp action color) = a >= start &&  a < end

  mk_regions = 
    let regions             = (get_callsite_regions_from_gcc_except_table <$> t) `orElse` []
        regions_with_lp     = filter (\(end,start,lp,action) -> lp /= fromIntegral entry) $ regions
        landing_pads        = map (\(end,start,lp,action) -> lp) regions_with_lp
        landing_pads_colors = IM.fromList $ map (\(indx,lp) -> (lp, hex_colors !! indx)) $ zip [0..] landing_pads in
      map (mk_callsite_region landing_pads_colors) regions_with_lp

  mk_callsite_region landing_pads_colors (end,start,lp,action)  = ECFG_Region end start lp action (landing_pads_colors IM.! lp)





  get_all_action_records = maybe [] actions t

  find_action_at cs_action = find_action cs_action get_all_action_records

  find_action 1         (act:_) = act
  find_action cs_action (act:acts)
    | gcc_except_table_action_type_size act <= fromIntegral cs_action = find_action (cs_action - gcc_except_table_action_type_size act) acts
    | otherwise = error $ show (cs_action,act:acts) 

  traverse_actions act
    | gcc_except_table_action_type_next_action act == 0 = [act]
    | otherwise =
      case find (\act' -> gcc_except_table_action_type_address act' == gcc_except_table_action_type_next_address act) get_all_action_records of
        Just act' -> act : traverse_actions act'

  gcc_except_table_action_to_type_info act =
    let typs      = maybe [] type_infos t
        filter    = gcc_except_table_action_type_filter act
        typs_indx = (length typs - fromIntegral filter) in
       if filter == 0 then
         (gcc_except_table_action_type_filter act,Absolute 0)
       else if typs_indx < length typs then
         (gcc_except_table_action_type_filter act,typs !! typs_indx)
       else
         error $ show (typs,act)


  obtain_typeinfo cs_action blockID blockIDs =
    let a  = blockID_to_address cfg blockID
        as = IS.map (blockID_to_address cfg) blockIDs
        l  = (bin,config,l0,fromIntegral entry) 
        indexed_types_infos = map gcc_except_table_action_to_type_info $ traverse_actions $ find_action_at cs_action in
      map (get_trgt_from_indexed_type l a as) $ indexed_types_infos

  get_trgt_from_indexed_type l a as (rdx,type_info) =
    let symstates = unsafePerformIO $ symbolically_execute_until l 0 a as (init_sym_state_with (Reg64 RDX) $ fromIntegral rdx)
        rips      = S.map read_RIP symstates in
      (a,strip_typeinfo_for $ address_to_pointee type_info,rips)


  strip_typeinfo_for str
    | "typeinfo for " `isPrefixOf` str = drop 13 str
    | otherwise = str

  address_to_pointee (Absolute 0) = "*NULL"
  address_to_pointee (Absolute a) =
    case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
      Just sym@(PointerToExternalFunction f)   -> demangle f `orElse` f
      Just sym@(PointerToObject f _ 0 _)       -> demangle f `orElse` f
      Just sym@(Relocated_ResolvedObject f _0) -> demangle f `orElse` f
      _ -> case find (\(Relocation a0 a1) -> a0 == a) $ binary_get_relocations bin of
             Just (Relocation a0 a1) -> address_to_label a1
             _ -> "&0x" ++ showHex a

  address_to_label a = 
    case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
      Just sym@(PointerToObject _ _ _ (Just l)) -> demangle l `orElse` l
      Just sym@(AddressOfLabel l _)             -> demangle l `orElse` l
      Just sym@(AddressOfObject l _)            -> demangle l `orElse` l
      _                                         -> "0x" ++ showHex a
      

blockID_to_address cfg blockID = fromIntegral $ inAddress $ head $ cfg_instrs cfg IM.! blockID
  

-- Compresses a CFG, i.e., removes node that are irrelevant.
-- Removing a node means connecting all parents to all children
-- A node is relevant if it either:
-- 1.) is a start or an end node
-- 2.) contains calls to, e.g., __cxa_throw
-- 3.) has CFI directives
-- 4.) is the beginning or end of a call-site region 
cfg_compress :: BinaryClass bin => Lifting bin pred finit v -> Word64 -> CFG -> CFG
cfg_compress l@(bin,_,_) entry cfg =
  let t                 = find (\t -> function_entry t == entry) $ cfi_gcc_except_tables $ binary_get_cfi bin
      landing_pads      = maybe IS.empty get_landing_pads_from_gcc_except_table t
      regions           = maybe [] get_callsite_regions_from_gcc_except_table t
      all_cfi_addresses = IS.unions [landing_pads] 
  in
    foldr (maybe_remove_node regions all_cfi_addresses) cfg (IM.keys $ cfg_edges cfg)
 where
  maybe_remove_node regions all_cfi_addresses blockID cfg
    | IS.null (intgraph_pre  cfg blockID) = cfg
    | IS.null (intgraph_post cfg blockID) = cfg
    | relevant_calls l cfg blockID /= []  = cfg
    | block_has_cfi_directive all_cfi_addresses blockID = cfg 
    -- | block_has_region_end region_ends blockID = cfg
    -- TODO or end of basic block has cfi_directive end is not start of another?
    | block_overlaps_region regions blockID = cfg
    | otherwise = remove_node blockID cfg


  block_overlaps_region regions blockID     = and
    [ block_contains_region_start_or_end regions blockID
    , any (instruction_overlaps_region regions) $ cfg_instrs cfg IM.! blockID ]
  instruction_overlaps_region regions i     = any (region_contains_address $ inAddress i) regions
  region_contains_address a (end,start,_,_) = start <= a && a < end


  block_contains_region_start_or_end regions blockID =
    let instrs            = cfg_instrs cfg IM.! blockID
        last_i            = last instrs
        block_end_address = fromIntegral (inAddress last_i) + fromIntegral (inSize last_i)
        block_addresses   = block_end_address : (map (fromIntegral . inAddress) $ drop 1 instrs) in
      any (\(end,start,_,_) -> start `elem` (map inAddress instrs) || end `elem` block_addresses) regions

  block_has_region_end region_ends blockID =
    let instrs            = cfg_instrs cfg IM.! blockID
        last_i            = last instrs
        block_end_address = fromIntegral (inAddress last_i) + fromIntegral (inSize last_i)
        block_addresses   = map (fromIntegral . inAddress) $ drop 1 instrs in
      block_end_address `IS.member` region_ends || any (\a -> a `IS.member` region_ends) block_addresses

  block_has_cfi_directive all_cfi_addresses blockID = any (address_has_cfi_directive all_cfi_addresses) $ map (fromIntegral . inAddress) $ cfg_instrs cfg IM.! blockID

  address_has_cfi_directive all_cfi_addresses a = a `IS.member` all_cfi_addresses || IM.lookup a (cfi_directives $ binary_get_cfi bin) /= Nothing


  remove_node blockID cfg = 
    let parents  = IS.delete blockID $ intgraph_pre cfg blockID
        children = IS.delete blockID $ intgraph_post cfg blockID
        prod     = filter (\(x,y) -> x /= y) $ [(x,y) | x <- IS.toList parents, y <- IS.toList children]
        cfg0     = delete_node blockID cfg in
      foldr add_new_edge cfg0 prod

  add_new_edge (parent,child) cfg  = cfg { cfg_edges = IM.insertWith IS.union parent (IS.singleton child) (cfg_edges cfg) }

  delete_node blockID cfg = cfg { cfg_edges = IM.map (IS.delete blockID) $ IM.delete blockID $ cfg_edges cfg, cfg_blocks = IM.delete blockID $ cfg_blocks cfg }




relevant_calls l cfg blockID = filter is_ecfg_relevant $ get_external_calls l cfg blockID


is_ecfg_relevant (a,f) = (f `notElem` ["__cxa_finalize", "__cxa_allocate_exception", "__cxa_free_exception"] && "__cxa_" `isPrefixOf` f) || f `elem` ["_Unwind_Resume", "_Unwind_RaiseException", "_Unwind_ForcedUnwind"] || "std::__throw_" `isPrefixOf` (demangle f `orElse` f)

get_external_calls l cfg blockID = concatMap get_external_call $ get_calls_from_blockID l cfg blockID
 where
  get_external_call (a,External f) = [(a,f)]
  get_external_call _ = []

get_calls_from_blockID l cfg blockID = get_call_target (cfg_instrs cfg IM.! blockID)
 where
  get_call_target instrs
    | is_call $ last instrs = zip (repeat (inAddress $ last instrs)) (get_known_jump_targets l $ last instrs)
    | otherwise             = []

  is_call i = isCall (inOperation i) || (isJump (inOperation i) && jump_is_actually_a_call l i)


