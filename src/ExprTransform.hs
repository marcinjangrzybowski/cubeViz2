{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExprTransform where

import Syntax

-- import Drawing.Base

import Data.Maybe
import Data.Bifunctor
import Data.Traversable
import Data.Functor

import Control.Applicative

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import DataExtra

import Abstract


data CubTransformation a =
     ReplaceAt Address a
   | RemoveCell Address
   | SplitCell Address



applyTransform ::  CubTransformation a -> Cub () a -> Either String (Cub () a)

applyTransform (ReplaceAt addrToReplace valToPut) =
   flip cubMapMayReplace [] $ 
    (\n addr x ->
       if addr == addrToReplace
       then Just $ Right $ (Cub undefined valToPut)
       else Nothing
     ) 

applyTransform (SplitCell addrToSplit ) =
   flip cubMapMayReplace [] $ 
    (\n addr x ->
       if addr == addrToSplit
       then  let sides = Map.fromList
                    (map (\fc -> (toSubFace fc , cubFace fc x )) $ genAllLI n )


             in Just $ Right $ (Hcomp () "splitVar" sides x)
       else Nothing
     )
    
-- applyTransform (SplitCell addrToSplit ) =
--    flip cubMapOld [] $ 
--     ((\n addr x ->
--        if addr == addrToSplit
--        then (let newSides = undefined
--              in Right $ Hcomp () "splitVar" newSides (Cub x
--             )
--        else Right $ x
--      ))
    
applyTransform _ = Right
