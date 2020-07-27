{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Combi where

-- import Data.Permute

import Data.List

import Data.Foldable

import qualified Data.Set as Set

import qualified Data.Map as Map

import Data.Maybe

import qualified Data.Bifunctor as Bf

import Data.Bool


factorial :: Int -> Int
factorial n = 
  case n of
    0 -> 1
    1 -> 1
    2 -> 2
    3 -> 6   
    4 -> 24
    5 -> 120
    _ -> n * (factorial (n - 1))

data Permutation = Permutation (Map.Map Int Int)

instance Show Permutation where
  show (Permutation x) = show $ toList x

data Permutation2 = Permutation2 [Int]

data Subset = Subset Int (Set.Set Int)

data SubFace = SubFace Int (Map.Map Int Bool)
  deriving (Eq , Ord)


sf2map :: SubFace -> Map.Map Int Bool
sf2map (SubFace _ x) = x

data Never a = Never

range k = take k [0,1..]


class OfDim a where
  getDim :: a -> Int

  checkDim :: Int -> a -> Maybe a
  checkDim i a =
    if (getDim a == i)
    then Just a
    else Nothing

forget :: a -> Never a
forget _ = Never


class ListInterpretable a b | a -> b where
  cardLI :: Never a -> Int -> Int
  enumerate :: Int -> Int -> a
  unemerate :: a -> Int
  toListLI :: a -> [b]
  fromListLI :: [b] -> a
  
  sizeLI :: a -> Int
  sizeLI = length . toListLI

  genAllLIHelp :: Never a -> Int -> [a]
  genAllLIHelp a n = map (enumerate n) (range (cardLI a n))

  genAllLI :: Int -> [a]
  genAllLI = genAllLIHelp Never

  -- checking in enumerate and unemerate are each others inverses
  checkUnEn :: Never a -> Int -> Bool
  checkUnEn na n = map unemerate ((genAllLIHelp na n)) == range (cardLI (na) n)

  cardLi :: a -> Int
  cardLi a = cardLI (forget a) $ sizeLI a 
    
instance (ListInterpretable a b) => OfDim a where
  getDim = sizeLI
  
mapListIndexed :: (Int -> a -> b) -> [a] -> [b]
mapListIndexed f = map (uncurry f) . zip [0..]



listInsert :: Int -> a -> [a] -> [a]
listInsert 0 a l = a : l
listInsert _ a [] = [a]
listInsert k a (x : xs) = x : listInsert (k - 1) a xs 

listPermute2 :: Permutation2 -> [a] -> [a]
listPermute2 (Permutation2 pm2) = foldr (uncurry listInsert) [] . zip pm2



instance ListInterpretable Permutation Int where
  cardLI _ = factorial 

  enumerate n i = Permutation $ Map.fromList $ zip (range n) $ listPermute2 (Permutation2 $ (f n i)) (range n)
    where
      f :: Int -> Int -> [Int]
      f n k =
         if (n > 0)
         then ((mod k n) : (f (n - 1) (k `div` n)) )
         else []

  
  unemerate (Permutation l) = f $ g (toList l)
    where
      f :: [Int] -> Int
      f [] = 0
      f (x : xs) = x + (length (x : xs) * (f xs))

      g :: [Int] -> [Int]
      g [] = []
      g l =
        let k = fromMaybe 0 (elemIndex 0 l)
        in k : g (map (flip (-) 1) $ (delete 0 l))
          
  toListLI (Permutation p) = map snd $ Map.toList p

  fromListLI = Permutation . Map.fromList . mapListIndexed (,)


instance ListInterpretable Subset Bool where
  cardLI _ n = 2 ^ n 

  enumerate n = fromListLI . f n
    where
      f :: Int -> Int -> [Bool]
      f 0 _ = []
      f n k = ((mod k 2) == 1) : f (n - 1) (k `div` 2) 
  
  unemerate = f . toListLI
    where
      f :: [Bool] -> Int
      f [] = 0
      f (False : l) = 2 * f l
      f (True : l) = 1 + 2 * f l
      
  
  toListLI (Subset n s) = map (flip Set.member s) (range n)

  fromListLI l = Subset (length l) $ Set.fromList $ (findIndices id) l

instance ListInterpretable SubFace (Maybe Bool) where
  cardLI _ n = 3 ^ n 

  enumerate n = fromListLI . f n
    where
      f :: Int -> Int -> [(Maybe Bool)]
      f 0 _ = []
      f n k =
        (case (mod k 3) of
           0 -> Nothing
           i -> Just (i == 2)
        ) : f (n - 1) (k `div` 3) 
  
  unemerate = f . toListLI
    where
      f :: [Maybe Bool] -> Int
      f [] = 0
      f ((Nothing) : l) = 3 * f l
      f ((Just False) : l) = 1 + 3 * f l
      f ((Just True) : l) = 2 + 3 * f l
      
  
  toListLI (SubFace n s) = map (flip Map.lookup s) (range n)

  fromListLI l = SubFace (length l) $ Map.fromList $ catMaybes $ map (\(i , x) -> fmap ((,) i) x) $ zip [0..] l 

instance Show SubFace where
  show (SubFace n x) = concat $ map (uncurry (++)) $ map (Bf.bimap show (bool "-" "+")) $ Map.toList x


instance Show Subset where
  show = concat . map (\b -> if b then "-" else "+") . toListLI


type Piece = (Subset , Permutation)

instance ListInterpretable Piece (Bool , Int) where  
  cardLI _ n = cardLI (Never :: (Never Subset)) n * cardLI (Never :: (Never Permutation)) n  
          
  enumerate n i =
    let cardSu = cardLI (Never :: (Never Subset)) n
        iSu = mod i cardSu
        iPm = i `div` cardSu
    in (enumerate n iSu , enumerate n iPm)
  
  unemerate (su , pm) =
    unemerate su + (unemerate pm * (cardLi su))
  
  toListLI (a1 , a2) = zip (toListLI a1) (toListLI a2)

  fromListLI = Bf.bimap fromListLI fromListLI . unzip


data FromLI a c = FromLI Int (a -> c)

instance Functor (FromLI a) where
  fmap f (FromLI n g) = FromLI n (f . g)  

instance (ListInterpretable a b) => Foldable (FromLI a) where 
  foldMap f (FromLI n g) = foldMap f $ (map g $ genAllLI n)




--  zana 32a
