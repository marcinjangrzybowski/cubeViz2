{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConsolePrint where

import Syntax

-- import Drawing.Base

import Data.Maybe
import Data.Bifunctor
import Data.Traversable
import Data.Functor
import Data.Function
import Data.List
import Control.Applicative

import Combi

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Either

import Abstract

import System.Console.Pretty

data CubPrintData =
  CubPrintData
  {
    cpdCursorAddress :: Maybe Address
  , cpdTailAddress :: Maybe Int
  }
  

-- concatCub :: Semigroup a -> Cub a a -> a
-- concatCub (Cub _ a) = a 
-- concatCub (Hcomp b nm pa a) = getDim a
  
printCub :: (Env , Context) -> CubPrintData ->  Address -> Cub b (Either Int CellExpr)  -> String 
printCub (ee , c) cpd addr (Cub _ a) =
  let
      isSelectedNode = Just addr == cpdCursorAddress cpd
      ifSel = (if isSelectedNode then bgColor Green else id)
      clStr (CellExpr (VarIndex k) tl) =
         let sym = fromRight (error "!!") $ getVarSymbol c k
             tailStr = concat $ intersperse (ifSel " ") $
                       zipWith (\i eI ->
                                  let ss = toString (ee , c) $ (remapIExpr (fromDimI c )) $ snd eI
                                  in    
                                     if isSelectedNode && (Just i == cpdTailAddress cpd)
                                     then (bgColor Red ss)  ++ (bgColor Green "")
                                     else ifSel ss
                                  
                               ) [0..] tl
         in sym ++ " " ++ tailStr

      s = (either (const "{!!}") clStr a)

  in ifSel s
  
printCub (ee , c) cpd addr (Hcomp _ nm pa a) =  
  let sides = Map.toList pa
         & map (\(sf@(SubFace _ m) , e) -> let
                                sf2 = Map.mapKeys (fromDimI c) m
                                c2 = addSFConstraintToContext sf2 (addDimToContext c (Just nm))                                  
                                bo = printCub (ee , c2) cpd (sf : addr) e
                                fc = either id id (toCode (ee , c) sf2)
                            in ( (parr $ parr fc ++ " = i1") ++ " → " ++ bo ++ "\n")

                       )
         & intercalate ";"

  
      y = (printCub (ee , c) cpd (fullSF (getDim a) : addr) a)
        
 
      s = ("hcomp " ++ "(λ " ++ nm ++ " → λ  { " ++ (indent 5 ("\n" ++ sides)) ++ "})\n" ++ parr y )      
  in if Just addr == cpdCursorAddress cpd
     then bgColor Green s
     else s
