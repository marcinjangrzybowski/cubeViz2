{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reorientable where


import Drawing.Color

import Syntax

import DataExtra

import Combi


import Data.List

import Data.Bifunctor

import Drawing.Base

import qualified Data.Set as Set
import qualified Data.Map as Map

-- class OfDim a => Reorientable a where

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

      -- ks will hold pairs of duplicates : (positionOfDuplicateInTail , duplicate)
      -- l holds array without duplicates
      let (ks , l) = foldl
             (\(ks , l) -> \(i , k) ->
                 if (elem k l)
                 then (ks ++ [(i , k)] , l)
                 else (ks , (l ++ [k])))
                ([] , []) (zip [0..] (fmap fst t))
          vM = Map.fromList $ zip l [0..]
          -- vM assigns for each dimensionIndex its position in deduplicated tail
      in (Permutation vM , fmap (second $ ((Map.!) vM)) ks)
                          --( those are pairs of (positionOfDuplicate , positionOfDupicatedValue in deduplciated tail)) 
 

  
class DiaDeg a where
  appNegs :: [Bool] -> a -> a
  -- appNegs _ = id
  
  appDiags :: [(Int,Int)] -> a -> a
  -- appDiags _ = id

  appPerm :: Permutation -> a -> a
  -- appPerm _ = id

  appDegen :: Extrudable b => (a -> ZDrawing b) -> [Int] ->  a -> ZDrawing b
  appDegen fromDiag ds a = degenAll ds (fromDiag a) 

  remapDS :: Extrudable b => (a -> ZDrawing b) -> DecomposedSubst -> a -> ZDrawing b  
  remapDS fromDiag ds =
       appDegen fromDiag (missingDS ds)
     . appPerm (orderDS ds)
     . appDiags (diagDS ds)
     . appNegs (negsDS ds)

  remapTL :: Extrudable b =>  (a -> ZDrawing b) -> Int -> [(Int,Bool)] -> a -> ZDrawing b
  remapTL fromDiag k tl = remapDS fromDiag $ decomposeSubst k tl 

  
instance OfDim [a] where
  
  getDim = length


instance DiaDeg Int where

  appNegs _ = id
  
  appDiags l x = x - (length l)

  appPerm _ = id

instance DiaDeg (FromLI Subset a) where
  appNegs l (FromLI n f) = (FromLI n (f . fromListLI . (zipWith (\x -> xor (not x)) l) . toListLI ))
  
  appDiags l (FromLI n f) = 
     let isDupe = not . (flip elem) (fmap fst l)
     in (FromLI (n - length l) (f . fromListLI . map snd . (filter (isDupe . fst ) )  . (zip [0..]) . toListLI ))

  appPerm prm (FromLI n f) = (FromLI n (f . fromListLI . listPermute prm . toListLI )) 

instance DiaDeg (FromLI Piece a) where
  appNegs l (FromLI n f) = (FromLI n (f . fromListLI . (zipWith (\x (b , y) -> ((xor (not x) b) , y)) l) . toListLI ))
  
  appDiags l (FromLI n f) = 
     let isDupe = not . (flip elem) (fmap fst l)
     in (FromLI (n - length l) (f . fromListLI . map snd . (filter (isDupe . fst ) )  . (zip [0..]) . toListLI ))

  appPerm prm (FromLI n f) = (FromLI n (f . fromListLI . listPermute prm . toListLI )) 

  

  

