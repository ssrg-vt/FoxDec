{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, DeriveGeneric, StrictData, StandaloneDeriving #-}


module Binary.ELLF where


import Base
import Config

import Binary.Generic
import Binary.FunctionNames

import Data.X86.Instruction
import Data.X86.Opcode
import Data.X86.Register
import Data.Size
import Data.Symbol
import Data.JumpTarget

import Parser.ParserCFI

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.ByteString.Builder
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Word 
import Data.List
import Data.Functor ((<&>))
import Data.Int
import Data.Maybe
import Data.Elf
import Data.Char (showLitChar,isAscii,isHexDigit)


import Data.Serialize.LEB128.Lenient 
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Char8 as BSC (unpack)

import qualified Data.Serialize.Get as S
import qualified Data.Serialize as Cereal hiding (get,put)
import GHC.Generics

import Data.Elf


import Debug.Trace

------------------
-- DATASTRUCTURES
------------------

data MetaDataHeader = MetaDataHeader {
  ellf_header_version :: Word32,
  ellf_header_feature :: Word8,
  ellf_header_num :: Word64
 }
 deriving (Eq,Ord,Show)

data ELLF_Basic_Block = ELLF_Basic_Block {
  ellf_bb_function :: Word64,
  ellf_bb_offset :: Word64,
  ellf_bb_size :: Word64
 }
 deriving (Eq,Ord,Show)

data ELLF_Symbol = ELLF_Symbol {
  ellf_sym_section_idx :: Word64,
  ellf_sym_address     :: Maybe Word64,
  ellf_sym_name        :: String
 }
 deriving (Eq,Ord, Show)

data ELLF_Function = ELLF_Function {
  ellf_func_symbol_idx :: Word64,
  ellf_func_first_bb :: Word64,
  ellf_func_last_bb :: Word64,
  ellf_func_address :: Word64
 }
 deriving (Eq,Ord,Show)

data ELLF_Pointer = ELLF_Pointer {
  ellf_ptr_section_idx :: Word64,
  ellf_ptr_offset      :: Word64,
  ellf_ptr_pointee_idx :: Word64,
  ellf_ptr_base        :: Maybe Word64
 }
 deriving (Eq,Ord, Show)

data ELLF_Pointee = ELLF_Pointee {
  ellf_pte_base :: Word64,
  ellf_pte_target :: Word64,
  ellf_pte_addend :: Int64
 }
 deriving (Eq,Ord, Show)


data ELLF_Section = ELLF_Section {
  ellf_section_align :: Word64,
  ellf_section_size  :: Word64,
  ellf_section_name  :: String
 }
 deriving (Eq,Ord,Show)

data ELLF_Layout = ELLF_Layout {
  ellf_layout_object  :: Word64,
  ellf_layout_offset  :: Word64,
  ellf_layout_size    :: Word64,
  ellf_layout_section :: String
 }
 deriving (Eq,Ord,Show)

data ELLF_Global = ELLF_Global {
  ellf_global_symbol :: Word64,
  ellf_global_size   :: Word64,
  ellf_global_type   :: Word64
 }
 deriving (Eq,Ord,Show)


data ELLF = ELLF {
  ellf_basic_blocks :: [[ELLF_Basic_Block]], 
  ellf_symbols :: [[ELLF_Symbol]],
  ellf_functions :: [[ELLF_Function]],
  ellf_pointers :: [[ELLF_Pointer]],
  ellf_pointees :: [[ELLF_Pointee]],
  ellf_sections :: [[ELLF_Section]],
  ellf_layout :: [ELLF_Layout],
  ellf_globals :: [[ELLF_Global]]
 }
 deriving (Eq,Ord)

instance Show ELLF where
  show (ELLF bbs symbols functions pointers pointees sections layout globals) = intercalate "\n" $ 
    [ "BASIC BLOCKS:"
    , show_list_of_lists 0 bbs
    , "SYMBOLS:"
    , show_list_of_lists 0 symbols
    , "FUNCTIONS:"
    , show_list_of_lists 0 functions
    , "POINTERS:"
    , show_list_of_lists 0 pointers
    , "POINTEES:"
    , show_list_of_lists 0 pointees
    , "SECTIONS:"
    , show_list_of_lists 0 sections
    , "LAYOUT:"
    , show_list layout
    , "GLOBALS:"
    , show_list_of_lists 0 globals
    ]
   where
     show_list_of_lists :: Show a => Int -> [[a]] -> String
     show_list_of_lists object [] = ""
     show_list_of_lists object (l:ls) = "Object " ++ show object ++ "\n" ++ (intercalate "\n" $ map show l) ++ "\n" ++ show_list_of_lists (object+1) ls

     show_list :: Show a => [a] -> String
     show_list = intercalate "\n" . map show


deriving instance Generic ELLF_Basic_Block
deriving instance Generic ELLF_Symbol
deriving instance Generic ELLF_Function
deriving instance Generic ELLF_Pointer
deriving instance Generic ELLF_Pointee
deriving instance Generic ELLF_Layout
deriving instance Generic ELLF_Global
deriving instance Generic ELLF_Section
deriving instance Generic ELLF

instance Cereal.Serialize ELLF_Basic_Block
instance Cereal.Serialize ELLF_Symbol
instance Cereal.Serialize ELLF_Function
instance Cereal.Serialize ELLF_Pointer
instance Cereal.Serialize ELLF_Pointee
instance Cereal.Serialize ELLF_Section
instance Cereal.Serialize ELLF_Layout
instance Cereal.Serialize ELLF_Global
instance Cereal.Serialize ELLF




------------------
-- READING AN ELLF
------------------
read_ellf :: Elf -> Maybe ELLF
read_ellf elf = do
  bbs       <- parse_ellf_blocks    <$> get_section ".ellf.blocks"
  sections  <- parse_ellf_sections  <$> get_section ".ellf.sections"
  layout    <- parse_ellf_layout    <$> get_section ".ellf.layout"
  globals   <- parse_ellf_globals   <$> get_section ".ellf.globals"
  pointees  <- parse_ellf_pointees  <$> get_section ".ellf.pointees"
  symbols   <- parse_ellf_symbols  sections layout  <$> get_section ".ellf.symbols"
  pointers  <- parse_ellf_pointers sections layout <$> get_section ".ellf.pointers"
  functions <- parse_ellf_functions symbols <$> get_section ".ellf.functions"
  return $ ELLF bbs symbols functions pointers pointees sections layout globals
 where
  get_section name = find (\s -> elfSectionName s == name) (elfSections elf)




-- .ellf.blocks:
parse_ellf_blocks :: ElfSection -> [[ELLF_Basic_Block]]
parse_ellf_blocks sec = evalState (repeat_until_empty 0 get_basic_blocks) (elfSectionData sec)
 where
  get_basic_blocks = parse_header_then_repeat . parse_block
  parse_block count = do
    function <- read_uleb128
    offset   <- read_uleb128
    size     <- read_uleb128
    return $ ELLF_Basic_Block function offset size

-- ellf.symbols:
parse_ellf_symbols sections layout sec = prevent_name_clashes_in_symbols 0 $ remove_unneeded_ELLF_symbols $ evalState (repeat_until_empty 0 get_symbols) (elfSectionData sec)
 where
  get_symbols = parse_header_then_repeat . parse_symbol
  parse_symbol count = do
    section_idx <- read_uleb128
    offset      <- read_uleb128
    name        <- read_string

    let section_table = sections !! count    
    let section = section_table !! fromIntegral section_idx
    let l       = find (\l -> ellf_layout_section l == ellf_section_name section && ellf_layout_object l == fromIntegral count) $ layout -- && ellf_layout_size l == ellf_section_size section
    let address = case l of
                    Nothing -> Nothing
                    Just l  -> Just $ ellf_layout_offset l + offset

    return $ ELLF_Symbol section_idx address name


-- ellf.sections:
parse_ellf_sections :: ElfSection  -> [[ELLF_Section]]
parse_ellf_sections sec = evalState (repeat_until_empty 0 get_sections) (elfSectionData sec)
 where
  get_sections = parse_header_then_repeat . parse_section
  parse_section count = do
    align <- read_uleb128
    size  <- read_uleb128
    name  <- read_string
    return $ ELLF_Section align size name

-- ellf.functions:
parse_ellf_functions symbols sec = evalState (repeat_until_empty 0 get_functions) (elfSectionData sec)
 where
  get_functions = parse_header_then_repeat . parse_function
  parse_function count = do
    symbol_idx <- read_uleb128
    first_bb   <- read_uleb128
    last_bb    <- read_uleb128

    let symtable = symbols !! count

    case ellf_entry_address_of_function symtable symbol_idx of
      Just a  -> return $ ELLF_Function symbol_idx first_bb last_bb a
      Nothing -> error $ "Symbol of function " ++ show (ELLF_Function symbol_idx first_bb last_bb 0) ++ " has no address:\n" ++ show symtable

  ellf_entry_address_of_function symtable symbol_idx = 
    let f_symbol  = symtable !! fromIntegral symbol_idx in
      ellf_sym_address f_symbol



-- ellf.pointers:
parse_ellf_pointers sections layout sec = evalState (repeat_until_empty 0 get_pointers) (elfSectionData sec)
 where
  get_pointers = parse_header_then_repeat . parse_pointer
  parse_pointer count = do
    section <- read_uleb128
    offset  <- read_uleb128
    pointee <- read_uleb128

    let section_table = sections !! count    

    let base = ellf_base_of_pointer section_table count section
    return $ ELLF_Pointer section offset pointee base

  ellf_base_of_pointer sections count section_idx = 
    let ptr_section = sections !! fromIntegral section_idx in
      case find (\l -> ellf_layout_section l == ellf_section_name ptr_section && ellf_layout_object l == fromIntegral count) $ layout of -- && ellf_layout_size l == ellf_section_size ptr_section
        Just layout -> Just $ ellf_layout_offset layout
        Nothing -> Nothing



-- ellf.pointees:
parse_ellf_pointees :: ElfSection  -> [[ELLF_Pointee]]
parse_ellf_pointees sec =  evalState (repeat_until_empty 0 get_pointees) (elfSectionData sec)
 where
  get_pointees = parse_header_then_repeat . parse_pointee
  parse_pointee count = do
    base   <- read_uleb128
    target <- read_uleb128
    addend <- read_sleb128
    return $ ELLF_Pointee base target addend


-- ellf.layout:
parse_ellf_layout :: ElfSection  -> [ELLF_Layout]
parse_ellf_layout sec = evalState get_layout (elfSectionData sec)
 where
  get_layout = parse_header_then_repeat parse_layout
  parse_layout = do
    object <- read_uleb128
    offset <- read_uleb128
    size <- read_uleb128
    name <- read_string
    return $ ELLF_Layout object offset size name

-- ellf.globals:
parse_ellf_globals sec = evalState (repeat_until_empty 0 get_globals) (elfSectionData sec)
 where
  get_globals = parse_header_then_repeat . parse_global
  parse_global count = do
    symbol_idx <- read_uleb128
    size       <- read_uleb128
    typ        <- read_uleb128
    return $ ELLF_Global symbol_idx size typ




prevent_name_clashes_in_symbols :: Int -> [[ELLF_Symbol]] -> [[ELLF_Symbol]]
prevent_name_clashes_in_symbols object [] = []
prevent_name_clashes_in_symbols object (syms:rest) = map (mk_sym object $ concat rest) syms : prevent_name_clashes_in_symbols (object+1) rest
 where
  mk_sym object rest sym = 
    case find (\sym' -> ellf_sym_name sym' == ellf_sym_name sym) rest of
      Nothing -> sym
      Just _  -> sym { ellf_sym_name = ".object" ++ show object ++ ellf_sym_name sym }

remove_unneeded_ELLF_symbols :: [[ELLF_Symbol]] -> [[ELLF_Symbol]]
remove_unneeded_ELLF_symbols = map remove
 where
  remove syms = go syms syms
  go [] all_syms = []
  go (sym:syms) all_syms
    | ".__ELLF_SECTION_" `isPrefixOf` ellf_sym_name sym =
      case find (\sym' -> ellf_sym_section_idx sym' == ellf_sym_section_idx sym && ellf_sym_address sym' == ellf_sym_address sym && ellf_sym_address sym' /= Nothing && ellf_sym_name sym' /= ellf_sym_name sym) all_syms of
        Nothing   -> sym:go syms all_syms
        Just sym' -> sym':go syms all_syms

    | otherwise = sym:go syms all_syms







parse_header :: State BS.ByteString MetaDataHeader 
parse_header = do
  version <- read_uint32
  feature <- read_uint8
  num     <- read_uleb128
  return $ MetaDataHeader version feature num


repeat_until_empty :: Int -> (Int -> State BS.ByteString a) -> State BS.ByteString [a]
repeat_until_empty count m = do
  bs <- get
  if BS.null bs then
    return []
  else do
    a <- m count
    as <- repeat_until_empty (count+1) m
    return $ a:as

parse_header_then_repeat :: State BS.ByteString a -> State BS.ByteString [a]
parse_header_then_repeat m = do
  MetaDataHeader version feature num <- parse_header
  read_n_times (fromIntegral num) m
  

read_uint8 :: State BS.ByteString Word8
read_uint8 = consume 1 <&> runGet S.getWord8

read_uint32 :: State BS.ByteString Word32
read_uint32 = consume 4 <&> runGet S.getWord32le

read_uleb128 :: (Show a, LEB128 a) => State BS.ByteString a
read_uleb128 = do
  bs <- get
  let v = runGet getLEB128 bs
  let encoding = toLEB128 v
  let num_bytes = BS.length encoding
  if BS.take num_bytes bs == encoding then do
    _ <- consume num_bytes
    return v
  else
    error $ show (show $ BS.unpack $ BS.take 20 bs, v, encoding,num_bytes)

read_sleb128 :: (Show a, SLEB128 a) => State BS.ByteString a
read_sleb128 = do
  bs <- get
  let v = runGet getSLEB128 bs
  let encoding = toSLEB128 v
  let num_bytes = BS.length encoding
  if BS.take num_bytes bs == encoding then do
    _ <- consume num_bytes
    return v
  else
    error $ show (show $ BS.unpack $ BS.take 20 bs, v, encoding,num_bytes)

read_string :: State BS.ByteString String
read_string = do
  bs <- get
  let (ret,rest) = BS.span ((/=) 0) bs
  put $ BS.tail rest
  return $ BSC.unpack ret
  


runGet :: S.Get a -> BS.ByteString -> a
runGet g bs =
  case S.runGet g bs of
    Left err -> error $ show err
    Right w  -> w

consume :: Int -> State BS.ByteString BS.ByteString
consume n = do
  (read,rest) <- gets $ BS.splitAt n
  put rest
  return read

read_n_times :: Int -> State BS.ByteString a -> State BS.ByteString [a]
read_n_times 0 r = return []
read_n_times n m = do
  a <- m
  as <- read_n_times (n-1) m
  return $ a:as
  



