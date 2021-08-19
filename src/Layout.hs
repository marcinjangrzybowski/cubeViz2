{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Layout where

import Abstract

import Syntax

import Drawing.Base
import Drawing.Color

import Data.Maybe
import Data.Either

import Control.Applicative

import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map

-- import Control.Arrow

import Combi

import FloatTools

import Data.Function
import Data.Bool
import Data.List
import qualified Data.Set as Set

import Data.Tuple.Extra (second , first)

import Data.Foldable

import PiecesEval

import Reorientable

import DataExtra

import GenericCell

import Debug.Trace

import Data.Bits

import Drawing.GL




optionsViewports :: Float -> Int -> Viewport -> [Viewport]
optionsViewports maxSize n vp0 =
  let s = Prelude.min maxSize (1.0/(fromIntegral n))
  in [ vp0 { vpScale = s * (vpScale vp0) * 0.95
           , vpScreenDelta = ((-1.0) + (fromIntegral (1 + 2 * i)) * s , (-1.0) +  s )
           }

     | i <- [0.. n]]
