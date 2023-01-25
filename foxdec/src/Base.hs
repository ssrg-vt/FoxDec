{-# LANGUAGE DeriveGeneric, DefaultSignatures, Strict #-}

{-|
Module      : Base
Description : Some base functions, imported by almost all other modules.
-}


module Base where

import Algorithm.SCC

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word ( Word64, Word8 )
import Data.Traversable (for)
import Data.List
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Numeric (showHex,readHex)
import Debug.Trace
import GHC.Generics
import qualified Data.Serialize as Cereal 
import Control.Monad.State.Strict
import Data.Ord (comparing)
import Data.Bits (shift,testBit,clearBit)


import Data.Serialize.Get (getSetOf)
import Data.Serialize.Put (putSetOf)
import qualified Data.Set.NonEmpty as NES

-- | Show the integer in hex.
showHex i = if i < 0 then Numeric.showHex (fromIntegral i :: Word64) "" else Numeric.showHex i ""
-- | Show an integer list as hex-list.
showHex_list is = "[" ++ intercalate "," (map showHex is) ++ "]"
-- | Show an integer set as hex-list.
showHex_set     = showHex_list . IS.toList
-- | Show an optional integer as an optional hex.
showHex_option Nothing = "Nothing"
showHex_option (Just v) = showHex v
-- | Read an int from a string storing a hex.
readHex' :: (Eq a, Num a) => String -> a
readHex' = fst . head . Numeric.readHex

-- | Lookup and produce error message if key does not exists in map.
im_lookup s m k =
  case IM.lookup k m of
    Nothing -> error s
    Just v  -> v

-- | use a default value in case of @Nothing@
orElse Nothing a  = a
orElse (Just a) _ = a


-- | A value exists (is not Nothing) and satisfies the predicate
existsAndSatisfies Nothing  p = False
existsAndSatisfies (Just a) p = p a  


-- | create a tuple
pair a b = (a,b)

-- | Find the index of one string in another.
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

-- | Take until the occurrence of the string
takeUntilString :: String -> String -> String
takeUntilString search []   = []
takeUntilString search str = if search `isPrefixOf` str then [] else head str : takeUntilString search (tail str)


-- | Strip outer parentheses from a string, if it has them.
strip_parentheses s = if length s > 0 && head s == '(' && last s == ')' then init $ tail s else s



partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
-- ^ Uses a function to determine which of two output lists an input element should join
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs


-- | In little endian, convert a byte-list to a 64 bit word.
-- Assume the list is at most length 8.
bytes_to_word :: [Word8] -> Word64
bytes_to_word [] = 0
bytes_to_word (w:ws) = (fromIntegral w::Word64) + shift (bytes_to_word ws) 8

-- | Convert first @n@ bytes of a word to an integer.
-- Assume @n<8@.
word_to_sint :: Int -> Word64 -> Int
word_to_sint si w =
  let neg = testBit w (si*8-1)
      val = fromIntegral (fromIntegral $ clearBit w (si*8-1):: Word64) in
    if neg then - val else val


-- | average of list of numbers
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

-- crossProduct [[1], [2,3,4], [5,6]] == [[1,2,5],[1,3,5],[1,4,5],[1,2,6],[1,3,6],[1,4,6]]
-- The size of a crossProduct [x_0,x_1,x_i] is the number of produced lists |x_0|*|x_1|*...*|x_i| times the size of each list i.
crossProduct :: [[a]] -> [[a]]
crossProduct []       = [[]]
crossProduct (as:ass) = [ b:bs | b <- as, bs <- crossProduct ass ]

crossProduct_size x = product (length x : map length x) 

neFromList :: Ord a => [a] -> NES.NESet a
neFromList  = NES.unsafeFromSet . S.fromList
neSetToList = S.toList . NES.toSet


--------------------------------------------
-- | Generic graph with ints as vertices.
--------------------------------------------
data Graph = Edges (IM.IntMap IS.IntSet)
  deriving (Generic,Show)

instance Cereal.Serialize Graph


-- | add edges from v to all vertices vs
graph_add_edges (Edges es) v vs = Edges $ IM.unionWith IS.union (IM.alter alter v es) empty_edges
 where
  alter Nothing    = Just $ vs
  alter (Just vs') = Just $ IS.union vs vs'

  empty_edges = IM.fromList $ zip (IS.toList vs) (repeat IS.empty)

-- | delete all edges with v as parent or child
graph_delete (Edges es) v = Edges $ IM.delete v $ IM.map (IS.delete v) es

-- | is v parent of an edge?
graph_is_parent (Edges es) v = IM.member v es

-- | is v a vertex in the graph?
graph_is_vertex (Edges es) v = IM.member v es || any (IS.member v) es

-- | is (v0,v1) an edge?
graph_is_edge (Edges es) v0 v1  =
  case IM.lookup v0 es of
    Nothing -> False
    Just vs -> IS.member v1 vs


instance IntGraph Graph where
  intgraph_post (Edges es) v =
    fromMaybe IS.empty (IM.lookup v es)
  intgraph_V (Edges es) = IS.unions $ IM.keysSet es : IM.elems es


-- | retrieve a non-trivial SCC, if any exists
graph_nontrivial_scc g@(Edges es) =
  let sccs             = all_sccs g IS.empty
      nontrivial_sccs  = filter is_non_trivial sccs
      nontrivial_scc   = maximumBy (comparing IS.size) sccs in
    trace ("Found SCC of mutually recursive function entries: " ++ showHex_set nontrivial_scc) nontrivial_scc
 where
  is_non_trivial :: IS.IntSet -> Bool
  is_non_trivial scc = IS.size scc > 1 || graph_is_edge g (head $ IS.toList scc) (head $ IS.toList scc)



-- | find next vertex to consider: either a terminal vertex (if any) or the head of an SCC
graph_find_next :: Graph -> Maybe Int
graph_find_next g@(Edges es) =
  if IM.null es then
    Nothing
  else case find (IS.disjoint (IM.keysSet es) . snd) $ IM.toList es of
    Nothing    -> Just $ head $ IS.toList $ graph_nontrivial_scc g -- no terminal vertex
    Just (v,_) -> Just v                               -- terminal vertex


------------------------------------------
-- Serialization                        --
------------------------------------------



instance (Ord a, Cereal.Serialize a) => Cereal.Serialize (NES.NESet a) where
    put = putNESetOf Cereal.put
    get = getNESetOf Cereal.get


putNESetOf :: Cereal.Putter a -> Cereal.Putter (NES.NESet a)
putNESetOf pa = putSetOf pa . NES.toSet
{-# INLINE putNESetOf #-}

-- | Read as a list of elements.
getNESetOf :: Ord a => Cereal.Get a -> Cereal.Get (NES.NESet a)
getNESetOf m = NES.unsafeFromSet `fmap` getSetOf m



------------------------------------------
-- Colors                               --
------------------------------------------


-- | decide whether text should be white or black based on background color
hex_color_of_text :: String -> String
hex_color_of_text bgcolor =
  let red   = (fromIntegral (readHex' [bgcolor!!1,bgcolor!!2] :: Int) :: Double)
      green = (fromIntegral (readHex' [bgcolor!!3,bgcolor!!4] :: Int) :: Double)
      blue  = (fromIntegral (readHex' [bgcolor!!5,bgcolor!!6] :: Int) :: Double) in
    if (red*0.299 + green*0.587 + blue*0.114) > 186 then
      "#000000"
    else
      "#ffffff"

-- | A list of RGB colors
hex_colors = [
  "#000000",
  "#FF0000",
  "#00FF00",
  "#0000FF",
  "#FFFF00",
  "#00FFFF",
  "#FF00FF",
  "#808080",
  "#FF8080",
  "#80FF80",
  "#8080FF",
  "#008080",
  "#800080",
  "#808000",
  "#FFFF80",
  "#80FFFF",
  "#FF80FF",
  "#FF0080",
  "#80FF00",
  "#0080FF",
  "#00FF80",
  "#8000FF",
  "#FF8000",
  "#000080",
  "#800000",
  "#008000",
  "#404040",
  "#FF4040",
  "#40FF40",
  "#4040FF",
  "#004040",
  "#400040",
  "#404000",
  "#804040",
  "#408040",
  "#404080",
  "#FFFF40",
  "#40FFFF",
  "#FF40FF",
  "#FF0040",
  "#40FF00",
  "#0040FF",
  "#FF8040",
  "#40FF80",
  "#8040FF",
  "#00FF40",
  "#4000FF",
  "#FF4000",
  "#000040",
  "#400000",
  "#004000",
  "#008040",
  "#400080",
  "#804000",
  "#80FF40",
  "#4080FF",
  "#FF4080",
  "#800040",
  "#408000",
  "#004080",
  "#808040",
  "#408080",
  "#804080",
  "#C0C0C0",
  "#FFC0C0",
  "#C0FFC0",
  "#C0C0FF",
  "#00C0C0",
  "#C000C0",
  "#C0C000",
  "#80C0C0",
  "#C080C0",
  "#C0C080",
  "#40C0C0",
  "#C040C0",
  "#C0C040",
  "#FFFFC0",
  "#C0FFFF",
  "#FFC0FF",
  "#FF00C0",
  "#C0FF00",
  "#00C0FF",
  "#FF80C0",
  "#C0FF80",
  "#80C0FF",
  "#FF40C0",
  "#C0FF40",
  "#40C0FF",
  "#00FFC0",
  "#C000FF",
  "#FFC000",
  "#0000C0",
  "#C00000",
  "#00C000",
  "#0080C0",
  "#C00080",
  "#80C000",
  "#0040C0",
  "#C00040",
  "#40C000",
  "#80FFC0",
  "#C080FF",
  "#FFC080",
  "#8000C0",
  "#C08000",
  "#00C080",
  "#8080C0",
  "#C08080",
  "#80C080",
  "#8040C0",
  "#C08040",
  "#40C080",
  "#40FFC0",
  "#C040FF",
  "#FFC040",
  "#4000C0",
  "#C04000",
  "#00C040",
  "#4080C0",
  "#C04080",
  "#80C040",
  "#4040C0",
  "#C04040",
  "#40C040",
  "#202020",
  "#FF2020",
  "#20FF20"
 ]


------------------------------------------
-- PREDICATES                           --
------------------------------------------

allp :: [a -> Bool] -> a -> Bool
allp ps a = and $ (\pred -> pred a) <$> ps
