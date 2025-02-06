{-# LANGUAGE DeriveGeneric #-}

module WithAbstractSymbolicValues.FInit where

import Base

import WithAbstractSymbolicValues.Class
import WithAbstractSymbolicValues.Sstate

import Data.SymbolicExpression (FlagStatus(..)) -- TODO
import Data.X86.Instruction
import Data.X86.Register
import Data.Size


import Algorithm.SCC


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set.NonEmpty as NES
import Data.List
import Data.Maybe

import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Serialize as Cereal





-- | Show function initialisation
instance (Eq v, Show v, Show p) => Show (FInit v p) where
 show (FInit sps m) = intercalate "\n" $ filter ((/=) []) $ 
  [ intercalate "\n" $ map show_sp_v $ S.toList sps
  , intercalate "\n" $ map show_entry $ M.toList m ]
  where
    show_sp_v (sp,v) = show_sp sp ++ " === " ++ show v
    show_sp (SSP_Reg r) = show r
    show_sp (SSP_Mem a si) = "*[" ++ show a ++ "," ++ show si ++ "]"
    show_entry ((sp0,sp1),r) = show (sp0,sp1) ++ ": " ++ show r

instance Show p => Show (SStatePart p) where
  show (SSP_Reg r)        = show r
  show (SSP_Mem a si)     = "[" ++ show a ++ ", " ++ show si ++ "]"


-- | The initial predicate.
finit_to_init_sstate :: WithAbstractSymbolicValues ctxt v p => ctxt -> FInit v p -> Sstate v p
finit_to_init_sstate ctxt finit@(FInit sps init_mem) =
  let rsp0                 = smk_init_reg_value ctxt $ Reg64 RSP
      write_stack_pointer  = swrite_rreg ctxt "finit_to_init" (Reg64 RSP) rsp0

      [rsp0_pointer]       = S.toList $ smk_mem_addresses ctxt "RSP0" rsp0
      top_stack_frame      = smk_init_mem_value ctxt "[RSP0,8]" rsp0_pointer (Just $ ByteSize 8)
      write_return_address = swrite_mem ctxt False rsp0 (Just $ ByteSize 8) top_stack_frame

      sregs                = M.empty
      smem                 = M.empty in
    execSstate (write_stack_pointer >> write_return_address >> write_finit (S.toList sps)) (Sstate sregs smem None)
 where
  write_finit []             = return ()
  write_finit ((sp,v):finit) = write_sp ctxt sp v >> write_finit finit 



-- | Convert the current invariant into a function initialisation
sstate_to_finit :: WithAbstractSymbolicValues ctxt v p => ctxt -> Sstate v p -> FInit v p
---invariant_to_finit ctxt p | trace ("invariant_to_finit: "++ show p) False = error "trace"
sstate_to_finit ctxt p = 
  let sps      = S.fromList $ regs_to_sps (sregs p) ++ mem_to_sps (smem p)
      pairs    = S.toList $ S.filter (\(x,y) -> x /= y) $ S.cartesianProduct sps sps
      mem_rels = M.fromList $ mapMaybe (mk_memrel ctxt) pairs in
    FInit (S.filter keep_sp $ S.map (\(sp,v,p) -> (sp,v)) $ sps) mem_rels
 where
    regs_to_sps regs = mapMaybe mk_reg $ filter suitable_reg $ M.assocs regs
    mem_to_sps  mem  = mapMaybe mk_mem $ M.assocs mem

    suitable_reg (Reg64 RIP,_) = False
    suitable_reg (Reg64 RSP,_) = False
    suitable_reg (Reg64 RBP,_) = False
    suitable_reg _           = True

    mk_reg (r,v)
     | regSize r == ByteSize 8 && r /= RegTemp = 
        let ptrs = smk_mem_addresses ctxt "finit" v in
          if S.null ptrs then Nothing else Just $ (SSP_Reg r,v,ptrs)
     | otherwise = Nothing

    mk_mem ((a,Just (ByteSize si)),v) =
      case skeep_for_finit ctxt (SSP_Mem a si) v of
        Nothing   -> Nothing
        Just ptrs -> Just $ (SSP_Mem a $ fromIntegral si,v,ptrs)
    mk_mem _ = Nothing

    mk_memrel ctxt ((sp0,v0,ptrs0),(sp1,v1,ptrs1))
    -- TODO separation should be necc here
      | all (\ptr0 -> all (\ptr1 -> sseparate ctxt "invariant_to_finit" ptr0 unknownSize ptr1 unknownSize) ptrs1) ptrs0 = Just ((sp0,sp1),Separate)
      | all (\ptr0 -> all (\ptr1 -> salias ctxt ptr0 unknownSize ptr1 unknownSize) ptrs1) ptrs0 = Just ((sp0,sp1),Aliassing)
      | otherwise = Nothing

    
    keep_sp (sp@(SSP_Reg _),v)   = skeep_for_finit ctxt sp v /= Nothing
    keep_sp (SSP_Mem _ _,v)      = True



-- | The join between two function initialisations
join_finit :: WithAbstractSymbolicValues ctxt v p => ctxt -> (FInit v p) -> (FInit v p) -> (FInit v p)
join_finit ctxt f0@(FInit sps0 m0) f1@(FInit sps1 m1)
  | f0 == f1 = f0
  | otherwise = FInit (S.intersection sps0 sps1) (M.intersectionWith join_rel m0 m1)
 where
  join_rel r0 r1
    | r0 == r1  = r0
    | otherwise = Unknown


pp_finitC :: (Show p, Show v, Ord p) => (FInit v p) -> String
pp_finitC (FInit sps m) = intercalate "\n" $ map show_group (group_separations $ group_aliasses m) ++ (map show_sp $ S.toList sps)
 where
  group_aliasses m =
    let sps          = S.toList $ S.fromList $ concatMap (\(sp0,sp1) -> [sp0,sp1]) $ M.keys m
        numbered_sps = zip [0..] sps
        g            = Edges $ IM.fromList $ map (\(idx,sp) -> (idx,IS.fromList $ related_to Aliassing (idx,sp) numbered_sps)) numbered_sps
        sccs         = all_sccs g IS.empty
        groups       = map (map (\idx -> fromJust $ IM.lookup idx $ IM.fromList numbered_sps) . IS.toList) sccs in
      groups

  group_separations groups =
    let numbered_groups = zip [0..] $ map S.fromList groups
        g               = Edges $ IM.fromList $ map (\(idx,sps) -> (idx,IS.fromList $ related_to_group (idx,sps) numbered_groups)) numbered_groups
        sccs            = all_sccs g IS.empty
        groups'         = map (map (\idx -> fromJust $ IM.lookup idx $ IM.fromList numbered_groups) . IS.toList) sccs in
      groups'

  show_group group = intercalate " | " $ map show_aliassing_group group 
  show_aliassing_group group
    | S.size group == 1 = show $ S.findMin group
    | otherwise         = "(" ++ (intercalate " aliasses " $ map show $ S.toList group) ++ ")"

  related_to rel (idx,sp) numbered_sps = map fst $ filter (\(idx',sp') -> idx /= idx' && hasRelation rel (sp,sp')) numbered_sps

  related_to_group (idx,sps) numbered_groups = map fst $ filter (\(idx',sps') -> idx /= idx' && any (hasRelation Separate) (S.cartesianProduct sps sps')) numbered_groups

  hasRelation rel (sp,sp') = M.lookup (sp,sp') m == Just rel || M.lookup (sp',sp) m == Just rel

  show_sp (sp,v) = show sp ++ " := " ++ show v

