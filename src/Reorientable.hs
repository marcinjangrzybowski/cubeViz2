{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reorientable where


import Drawing.Color

import Syntax

import DataExtra

import Combi

import Abstract

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
      let (ks , l) = foldl
             (\(ks , l) -> \(i , k) ->
                 if (elem k l)
                 then (ks ++ [(i , k)] , l)
                 else (ks , (l ++ [k])))
                ([] , []) (zip [0..] (fmap fst t))
          vM = Map.fromList $ zip l [0..]   
      in (Permutation vM , fmap (second $ ((Map.!) vM)) ks)



  
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
  
  appDiags _ = id

  appPerm _ = id

  
-- -- instance Diagonable (a , [Int]) where
-- --   -- remapD ds ( = undefined
-- --   appDegen l (k , _) = (k , l)
-- --   -- appNegs l (k , _) = (k , l)

-- instance Diagonable (Maybe ((Int , Color) , [Int])) where
--   -- remapD ds ( = undefined
--   appDegen l (Just (k , _)) = Just (k , l)
--   appDegen l (Nothing) = Nothing

--   -- appNegs l (Just (k , _)) = Just (k , l)
--   -- appNegs l (Nothing) = Nothing

-- -- instance Reorientable (Drawing b) where

-- instance Diagonable (Maybe (Either Color (Color , Color) , [Int])) where
--   appDegen l (Just (k , _)) = Just (k , l)
--   appDegen l (Nothing) = Nothing

--   appNegs ([ False ]) (Just ((Right (c0 , c1)) , y)) = (Just ((Right (c1 , c0)) , y))
--   appNegs _ x = x

-- instance Diagonable (Either Color (Color , Color) , [Int]) where
--   appDegen l (k , _) = (k , l)
  
--   appNegs ([ False ]) ((Right (c0 , c1)) , y) =  ((Right (c1 , c0)) , y)
--   appNegs _ x = x  

  

