{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module AAbstract where

import qualified ASyntax as A


import Syntax
-- import Drawing.Base

import Data.Maybe
import Data.Bifunctor
import Data.Traversable
import Data.Functor
import Data.Function
import Data.Either
import Data.List
import Control.Applicative
import Control.Monad

import Control.Monad.State.Lazy

import Control.Monad.Writer.Lazy


import qualified Data.Foldable as Foldable

import Data.Functor.Identity

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import DataExtra

-- import Reorientable

import Debug.Trace
import Data.Either (fromRight)

import GHC.Stack.Types ( HasCallStack )

import Data.Aeson

import Abstract

-- ClExpr Int (Map.Map (Map.Map Int Bool) OExpr)

fromAIExpr :: A.IExpr -> IExpr
fromAIExpr (A.IExpr x) = IExpr x

-- Partial = Map.Map (Map.Map Int Bool) OExpr

toCylCubA :: Int -> Int -> A.Partial -> CylCub ()
toCylCubA k n mp =
    let f sf e = 
          toOCubA k (n + 1 - subFaceCodim sf) e
    in CylCub (fromMapFLI n ((Map.mapWithKey f (Map.mapKeys (SubFace n) mp))))  

toOCubA :: Int -> Int -> A.OExpr -> OCub ()
toOCubA k n = \case
    A.HComp mbs pa cle ->
      Hcomp () mbs (toCylCubA k n pa)  (toClCubA' k cle)
    A.Cell (A.CellExpr (A.VarIndex v) args) ->
       Cub
        n ()
        (Just (CellExpr (VarIndex (v + (k - n))) (map fromAIExpr args)))
    A.ConstE _ -> Cub n () Nothing
    A.CHole _ -> Cub n () Nothing
    A.Appl _ _ -> undefined --Cub 0 () Nothing  
  
   -- ClCub (fromMapFLI n (Map.mapKeys (SubFace n) (fmap . mp)))  





toClCubA' :: Int -> A.ClExpr -> ClCub ()
toClCubA' k (A.ClExpr n mp) =
   let f sf e = toOCubA k (n - subFaceCodim sf) e
   in ClCub (fromMapFLIUnsafe n ((Map.mapWithKey f (Map.mapKeys (SubFace n) mp))))  



-- fixVarIndexesACl :: Int -> ClCub () -> ClCub ()
-- fixVarIndexesACl k = fmap (

  
toClCubA :: A.ClExpr -> ClCub ()
toClCubA x@(A.ClExpr k _)   = toClCubA' k x
