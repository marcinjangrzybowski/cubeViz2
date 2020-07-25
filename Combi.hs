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

import qualified Data.Set as Set

import qualified Data.Map as Map

import qualified Data.Bifunctor as Bf

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

data Subset = Subset Int (Set.Set Int)


data Never a = Never

class ListInterpretable a b | a -> b where
  cardLI :: Never a -> Int -> Int
  enumerate :: Int -> Int -> a
  unemerate :: Int -> a -> Int
  toListLI :: a -> [b]
  fromListLI :: [b] -> a
  
  sizeLI :: a -> Int
  sizeLI = length . toListLI

  genAllLIHelp :: Never a -> Int -> [a]
  genAllLIHelp a n = map (enumerate n) (take (cardLI a n) [01..])

  genAllLI :: Int -> [a]
  genAllLI = genAllLIHelp Never 
    
-- class Rankable b a where
--   rank :: Never b -> a -> Int
--   cardRR :: Never b -> Never a -> Int -> Int
--   enumerate :: Never b -> Int -> Int -> a
--   unemerate :: Never b -> Int -> a -> Int
  
mapListIndexed :: (Int -> a -> b) -> [a] -> [b]
mapListIndexed = undefined

instance ListInterpretable Permutation Int where
  cardLI _ = factorial 

  enumerate n i = undefined
  
  unemerate n a = undefined
  
  toListLI (Permutation p) = map snd $ Map.toList p

  fromListLI = Permutation . Map.fromList . mapListIndexed (,)


instance ListInterpretable Subset Bool where
  cardLI _ n = 2 ^ n 

  enumerate n i = undefined
  
  unemerate n a = undefined
  
  toListLI (Subset n s) = map (flip Set.member s) (take n [01..])

  fromListLI l = Subset (length l) $ Set.fromList $ (findIndices id) l 

type Piece = (Subset , Permutation)

instance ListInterpretable Piece (Bool , Int) where  
  cardLI _ n = cardLI (Never :: (Never Subset)) n * cardLI (Never :: (Never Permutation)) n  
          
  enumerate n i = undefined
  
  unemerate n a = undefined
  
  toListLI (a1 , a2) = zip (toListLI a1) (toListLI a2)

  fromListLI = Bf.bimap fromListLI fromListLI . unzip


