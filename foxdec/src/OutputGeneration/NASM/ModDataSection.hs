{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : NASM
Description : A datastructure for storing NASM code.
-}



module OutputGeneration.NASM.ModDataSection where

import Base
import Data.X86.Opcode
import Data.X86.Instruction
import Data.Symbol
import Data.Size
import Data.JumpTarget
import Binary.Generic
import Data.L0
import Data.SPointer
import Data.SValue

import OutputGeneration.NASM.NASM
import OutputGeneration.GlobalMemAnalysis

import Binary.Elf

import WithNoAbstraction.SymbolicExecution
import WithAbstractSymbolicValues.Class

import qualified Data.Set as S
import Data.Word
import Data.List
import Data.Bits (testBit)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Debug.Trace



split_data_section l@(bin,config,l0,_) (NASM externals globals sections footer) = NASM externals globals new_sections footer
 where
  new_sections = map split_section sections

  split_section s@(NASM_Section_Text _)  = s
  split_section s@(NASM_Section_Data ds) = NASM_Section_Data $ concatMap split_data_section ds

  split_data_section ds@(NASM_DataSection (seg,sec,a0) align entries) 
    | is_data_section (seg,sec,0,0,0) || is_bss_data_section (seg,sec,0,0,0) =  -- mk_analyzable_section ds
      let ds' = mk_analyzable_section ds
          split = 18 in
        reverse (take split ds') ++ drop split ds'
    | otherwise = [ds]

  mk_analyzable_section ds@(NASM_DataSection (seg,sec,a0) align entries) = 
    case find_section_for_address bin a0 of
      Nothing -> [ds]
      Just (_,_,_,si0,_) ->
        let regions = analyze_gmem l a0 (fromIntegral si0) $ map (gmem . l0_lookup_join l0) $ S.toList $ l0_get_function_entries l0 
            regions' = concat_regions $ IM.assocs regions
            tr = trace (seg ++ sec ++ "\nRegions:\n" ++ (intercalate "\n" (map show_region_info regions')))
            tr' = trace (seg ++ sec ++ "\nRegions:\n" ++ (intercalate "\n" (map show_region_info $ IM.assocs regions))) in

          tr' $ tr $ map (region_to_data_section seg sec align entries) regions'

  region_to_data_section seg sec align entries (a,Variable si)       = NASM_DataSection (seg,sec,fromIntegral a) 0 $ filter (is_within_region a si) entries
  region_to_data_section seg sec align entries (a,RegionInfo _ si _) = NASM_DataSection (seg,sec,fromIntegral a) 0 $ filter (is_within_region a si) entries

  is_within_region a si (a0,_) = a <= fromIntegral a0 && fromIntegral a0 < a + fromIntegral si
            

  concat_regions [] = []
  concat_regions (r@(a,Variable _):rs) = r : concat_regions rs
  concat_regions ((a,r@(RegionInfo dirty si _)) : rs) = 
    let (yes,no) = span is_not_variable rs
        si'      = sum $ map get_size (r:map snd yes) in
      (a,RegionInfo dirty si' []) : concat_regions no

  is_not_variable (_,Variable _) = False
  is_not_variable _ = True

  get_size (RegionInfo _ si _) = si

{--
= concatMap (mk_data_section n align) $ split_entries entries

  mk_data_section n align [] = [] 
  mk_data_section (seg,sec,_) align ((a,e):es) = [NASM_DataSection (seg,sec,a) align $ (a,e):es]

  split_entries [] = [[]]
  split_entries (e@(a,DataEntry_Label l):es) =
    let (ls,rest) = span isLabel es
        (es0:ess) = split_entries rest in
      [] : (e:ls ++ es0) : ess
  split_entries (e:es) = 
    let (es0:ess) = split_entries es in
      (e:es0) : ess

  isLabel (_,DataEntry_Label _) = True
  isLabel _ = False
--}

{--
split_data_section :: [Word64] -> NASM -> NASM
split_data_section breaks0 (NASM externals globals sections footer) = NASM externals globals new_sections footer
 where
  new_sections = map split_section sections

  split_section s@(NASM_Section_Text _) = s
  split_section s@(NASM_Section_Data ds) = NASM_Section_Data $ reverse $ concatMap split_data_section ds
 
  split_data_section ds@(NASM_DataSection (seg,sec,a0) align entries) =
    let breaks' = sort $ filter (\a -> a0 <= a && a < a0 + fromIntegral (entries_length entries)) breaks0
        splits  = break_up a0 entries breaks' in
      map (\(from,es) -> NASM_DataSection (seg,sec,from) align es) $ add_addresses a0 splits --TODO alginment

  add_addresses a [es] = [(a,es)]
  add_addresses a (es0:ess) = (a,es0) : add_addresses (a+fromIntegral (entries_length es0)) ess




  break_up :: -> [(Word64, NASM_DataEntry)] -> [Word64] -> [[(Word64, NASM_DataEntry)]]
  break_up []          breaks        = [[]]
  break_up entries     []            = [entries]
  break_up es@((a0,e):entries) bs@(from:breaks) 
    | from < a0 = [es]
    | a0 == from =
      case breaks of
        [] -> [[], es]
        (to:breaks') ->
          let (new_data_section,remainder) = take_size (fromIntegral $ to-from) es in
            [] : new_data_section : break_up to remainder breaks'
    | from >= a0 + fromIntegral (entry_length e) =
      let (ds:dss) = break_up (a0 + fromIntegral (entry_length e)) entries bs in
        (e:ds):dss
    | otherwise =
      case e of
        DataEntry_BSS si' -> 
          let si = fromIntegral (from - a0)
              entries' = DataEntry_BSS (si'-si):entries in
            [DataEntry_BSS si] : break_up from entries' breaks
        _ -> traceShow ("IGNORING SPLIT " ++ showHex from) $ break_up a0 (e:entries) breaks

        

  entry_length (DataEntry_Byte _)      = 1
  entry_length (DataEntry_String str)  = length str + 1
  entry_length (DataEntry_Pointer _)   = 8
  entry_length (DataEntry_BSS sz)      = sz

  entries_length = sum . map entry_length


  take_size 0 entries      = ([],entries) 
  take_size _ []           = error $ "Taking too many data entries from section."

  take_size si ((DataEntry_BSS si'):entries)
    | si == si' = ([DataEntry_BSS si], entries)
    | si <  si' = ([DataEntry_BSS si], DataEntry_BSS (si'-si):entries)
    | otherwise = error $ "Taking too large part of bss section"

  take_size si (e:entries) 
    | si < entry_length e  = error $ "Taking non-fitting data entries from section."
    | si == entry_length e = ([e],entries)
    | otherwise            =
      let (taken,remainder) = take_size (si - entry_length e) entries in
        (e:taken, remainder)


  isEmpty [] = True
  isEmpty _  = False
--}



