{-# LANGUAGE FlexibleInstances #-}
module Reorientable where


import Drawing.Base

import Syntax

import Combi

import Abstract

import Data.List

import Data.Bifunctor

import qualified Data.Set as Set
import qualified Data.Map as Map

-- class OfDim a => Reorientable a where
  

class Diagonable a where  
  appNegs :: [Bool] -> a -> a
  appNegs _ = id
  
  appDiags :: [(Int,Int)] -> a -> a
  appDiags _ = id

  appPerm :: Permutation -> a -> a
  appPerm _ = id

  appDegen :: [Int] -> a -> a
  appDegen _ = id

  remapDS :: DecomposedSubst -> a -> a  
  remapDS ds =
       appDegen (missingDS ds)
     . appPerm (orderDS ds)
     . appDiags (diagDS ds)
     . appNegs (negsDS ds)

  remapTL :: Int -> [(Int,Bool)] -> a -> a
  remapTL k tl = remapDS $ decomposeSubst k tl 
  
instance OfDim [a] where
  getDim = length
  
instance Diagonable (a , [Int]) where
  -- remapD ds ( = undefined
  appDegen l (k , _) = (k , l)
  -- appNegs l (k , _) = (k , l)

instance Diagonable (Maybe (a , [Int])) where
  -- remapD ds ( = undefined
  appDegen l (Just (k , _)) = Just (k , l)
  appDegen l (Nothing) = Nothing

  -- appNegs l (Just (k , _)) = Just (k , l)
  -- appNegs l (Nothing) = Nothing

-- instance Reorientable (Drawing b) where

data DecomposedSubst =
   DecomposedSubst {
     missingDS :: [Int] ,
     orderDS :: Permutation ,
     negsDS :: [Bool] ,
     diagDS :: [(Int,Int)] }
   deriving Show

decomposeSubst :: Int -> [(Int,Bool)] -> DecomposedSubst
decomposeSubst n t =
  DecomposedSubst {
     missingDS = Set.toList $ Set.difference (Set.fromList (range n)) (Set.fromList (fmap fst t)) ,
     orderDS = fst diag ,
     negsDS = fmap snd t ,
     diagDS = snd diag }

  where
      
    diag =
      let (ks , l) = foldl
             (\(ks , l) -> \(i , k) ->
                 if (elem k l)
                 then (ks ++ [(i , k)] , l)
                 else (ks , (l ++ [k])))
                ([] , []) (zip [0..] (fmap fst t))
          vM = Map.fromList $ zip l [0..]   
      in (Permutation vM , fmap (second $ ((Map.!) vM)) ks)

