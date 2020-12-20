{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataExtra where

import Data.Bifunctor

import Data.Function
import Data.List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Foldable

import Control.Applicative

tpl2arr (x , y) = [x , y]
trpl2arr (x , y , z) = [x , y , z]

range k = take k [0..]

punchOut :: Int -> Int -> Maybe Int
punchOut k i | i < k = Just i
             | i == k = Nothing
             | i > k = Just (i - 1)

punchIn :: Int -> Int -> Int
punchIn k i | i < k = i
            | i >= k = i + 1


punchInMany :: Set.Set Int -> Int -> Int
punchInMany ks i =
  foldl (flip punchIn) i (Set.toAscList ks)

punchOutMany :: Set.Set Int -> Int -> Maybe Int
punchOutMany ks i =
  foldl (\mbj j -> mbj >>= (punchOut j) ) (Just i) (Set.toDescList ks)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

rotateFrom :: (Eq a) => a -> [a] -> a
rotateFrom a (x : y : ys) | not $ elem a (x : y : ys) = error "unalbe to rotate, arg is missing from array"
rotateFrom a (x : y : ys) | x == a = y
rotateFrom a (x : y : ys) | otherwise = rotateFrom a (y : ys ++ [x]) 
rotateFrom a _ = a

rotateFromDir :: (Eq a) => Bool -> a -> [a] -> a
rotateFromDir True = rotateFrom
rotateFromDir False = flip $ (flip rotateFrom) . reverse

--all subsets of cardinality card(x) - 1
explode :: [a] -> [[a]]
explode [] = []
explode [a] = [[]]
explode l =
  let z = zip [0..] l
  in fmap (\(i , _) -> fmap snd $ filter (\(j , _) -> j /= i) z ) z



mapHead :: (a -> a) -> [a] -> [a]
mapHead f [] = []
mapHead f (x : xs) = f x : xs

listInsert :: Int -> a -> [a] -> [a]
listInsert 0 a l = a : l
listInsert _ a [] = [a]
listInsert k a (x : xs) = x : listInsert (k - 1) a xs 

listRemove :: Int -> [a] -> [a]
listRemove _ [] = error "bad index or empty list"
listRemove 0 (x : xs) = xs
listRemove k (x : xs) = x : listRemove (k - 1) ( xs)

listPopAt :: Int -> [a] -> (a , [a])
listPopAt _ [] = error "bad index or empty list"
listPopAt 0 (x : xs) = (x , xs) 
listPopAt k (x : xs) = second ((:) x) $ listPopAt (k - 1) ( xs)

tailAlways :: [a] -> [a]
tailAlways [] = []
tailAlways (_ : l) = l

transposeTuples ((a , b) , (c , d)) = ((a , c) , (b , d))

mapBoth f = bimap f f


average :: (Real a , Fractional b) => [a] -> b
average [] = error "attempt to took average of empty list"
average xs = realToFrac (sum xs) / genericLength xs

halves :: [a] -> ([a] , [a])
halves l =
  let hl = div (length l) 2 
  in (take hl l , drop hl l) 

evalNegFloat :: Bool -> Float -> Float
evalNegFloat False x = (1 - x)
evalNegFloat True x = x

xor :: Bool -> Bool -> Bool
xor b False = b
xor b True = not b

negCompIf :: Bool -> Ordering -> Ordering
negCompIf True LT = GT
negCompIf True GT = LT
negCompIf _ x = x

pickFromPair ::  Bool -> (a , a) -> a
pickFromPair x | x = snd
               | otherwise = fst

dot2 :: (d -> e) -> (a -> b -> d) -> (a -> b -> e)  
dot2 f g a b = f $ g a b 


dot3 :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)  
dot3 f g a b c = f $ g a b c 

dot4 :: (e -> f) -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> f)  
dot4 f g a b c d = f $ g a b c d

dot5 :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> (a -> b -> c -> d -> e -> g)  
dot5 f g a b c d e = f $ g a b c d e

dot6 :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> (a -> b -> c -> d -> e -> f -> h)  
dot6 ff g a b c d e f = ff $ g a b c d e f 

dot7 :: (h -> i) -> (a -> b -> c -> d -> e -> f -> g -> h) -> (a -> b -> c -> d -> e -> f -> g -> i)  
dot7 ff h a b c d e f g = ff $ h a b c d e f g 


curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x



look :: Int -> [ a ] -> Maybe a
look _ [] = Nothing
look 0 (x : _) = Just x
look k (_ : xs) = look (k - 1) xs


mapToSet :: (Ord k , Ord a) => Map.Map k a -> Set.Set (k , a) 
mapToSet = Set.fromList . Map.toList


addIfNotIn :: Eq a => a -> [a] -> [a]
addIfNotIn a l =
  if (elem a l)
  then l
  else a : l

data EnsureFoldResult a b =
    EFREmpty
  | EFREvery b
  | EFRException
     { efreAllBefore :: b
     , efreExceptionalElement :: a
     , efreExceptionalValue :: b
     }
  deriving (Show , Eq)

ensureFold :: (Foldable f , Eq b) => (a -> b) -> f a -> EnsureFoldResult a b 
ensureFold y x
  | null x         = EFREmpty
  | otherwise      =
      let firstB = y (Maybe.fromJust (Data.Foldable.find (const True) x))
      in foldl (\case
                   EFREvery b -> (\z -> if y z == b then EFREvery b else EFRException b z (y z))
                   y -> const y
                   )
         (EFREvery firstB) x



binsBy :: (Ord b , Bounded b) => (a -> b) -> [a] -> Map.Map b [a]
binsBy f = foldr (\a -> Map.insertWith (++) (f a) [a]) Map.empty  

setMapMaybe :: (Ord a , Ord b) => (a -> Maybe b) -> Set.Set a -> Set.Set b
setMapMaybe f = Set.fromList . Maybe.mapMaybe f . Set.toList


foldFL :: [a -> a] -> a -> a
foldFL l x = foldl (\x f -> f x) x l 



disjointSetFamFold :: forall a b. (Foldable a , Ord b) => a (Set.Set b) -> Set.Set (Set.Set b)
disjointSetFamFold = foldl visit Set.empty
  where
    visit :: Set.Set (Set.Set b) -> Set.Set b -> Set.Set (Set.Set b) 
    visit fam s =
      let (toPass , toMerge) = Set.partition (Set.disjoint s) fam
          merged = foldl Set.union s toMerge
      in Set.insert merged toPass
       
