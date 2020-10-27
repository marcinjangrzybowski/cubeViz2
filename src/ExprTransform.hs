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
     ReplaceAt Address (Cub () a)
   | RemoveCell Address
   | RemoveCellLeaveFaces Address
   | SplitCell Address
   | AddSubFace (Address , SubFace)
   | MapAt Address (Cub () a -> Either String (Cub () a))
  -- deriving (Show)


applyTransform ::  CubTransformation (Either Int a) -> Cub () (Either Int a) -> Either String (Cub () (Either Int a))

applyTransform (ReplaceAt addrToReplace valToPut) =
   flip cubMapMayReplace [] $ 
    (\n addr x ->
       if addr == addrToReplace
       then Just $ Right $ valToPut
       else Nothing
     ) 

applyTransform (SplitCell addrToSplit ) =
   flip cubMapMayReplace [] $ 
    (\n addr x ->
       if addr == addrToSplit
       then  let sides = Map.fromList
                    (map (\fc -> (toSubFace fc , cubHole n )) $ genAllLI n )
                    -- needs injectDim to work!
                    -- (map (\fc -> (toSubFace fc , cubFace fc x )) $ genAllLI n )

             in Just $ Right $ (Hcomp () "splitVar" sides x)
       else Nothing
     )

applyTransform (RemoveCell [] ) = Right
applyTransform (RemoveCell (addrToRemoveHead : addrToRemoveTail) ) =
   flip cubMapMayReplace [] $ 
    (\n addr x ->
       if addr == addrToRemoveTail
       then case x of
               Hcomp () nm sides x ->
                  Just $ Right $ Hcomp () nm (Map.delete addrToRemoveHead sides) x
               _ -> Nothing
       else Nothing
     )

applyTransform (RemoveCellLeaveFaces [] ) = Right
applyTransform (RemoveCellLeaveFaces (addrToRemoveHead : addrToRemoveTail) ) =
   flip cubMapMayReplace [] $ 
    (\n addr parX ->
       if addr == addrToRemoveTail
       then case parX of
               Hcomp () nm sides x ->
                  Just $ Right $ Hcomp () nm
                            (deleteButLeaveFaces
                               -- (const (Cub undefined (Left 0)))
                                (\fc -> cubFace (injFaceSide fc) )
                                addrToRemoveHead sides) x
               _ -> Nothing
       else Nothing
     )

    
applyTransform (AddSubFace (addrToAdd , sf)) = 
   flip cubMapMayReplace [] $ 
    (\n addr x ->
       if addr == addrToAdd
       then case x of
               Hcomp () nm sides x ->
                  Just $ Right $ Hcomp () nm
                            (addSubFace
                               (Cub undefined (Left 0))
                               sf sides) x
               _ -> Nothing
       else Nothing
     )


applyTransform (MapAt addrToMap f) =
   flip cubMapMayReplace [] $ 
    (\n addr x ->
       if addr == addrToMap
       then Just $ f x
       else Nothing
     ) 
