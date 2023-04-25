{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConsolePrint where

-- (global-set-key [f9] 'haskell-process-load-or-reload)

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


printCub :: (Env , Context) -> CubPrintData ->  ClCub b -> String 
printCub (ee , c) cpd x =
  printCubO (ee , c) cpd (rootAddress (getDim x)) $ clInterior x


printCubO :: (Env , Context) -> CubPrintData -> Address -> OCub b -> String 
printCubO (ee , c) cpd addr (Cub _ _ a) = 
  let
      isSelectedNode = Just addr == cpdCursorAddress cpd
      ifSel = (if isSelectedNode then bgColor Green else id)

      clStr (CellExpr (VarIndex k) tl) =
         let sym = fromRight (error "!!") $ getVarSymbol c k
             tailStr = concat $ intersperse (ifSel " ") $
                       zipWith (\i eI ->
                                  let ss' = toString (ee , c) $ (remapIExpr (fromDimI c )) $ eI
                                      ss = "("++ss'++")"
                                  in    
                                     if isSelectedNode && (Just i == cpdTailAddress cpd)
                                     then (bgColor Red ss)  ++ (bgColor Green "")
                                     else ifSel ss
                                  
                               ) [0..] tl
         in "(" ++ sym ++  tailStr ++ ")"
         
      s = (maybe ("{!!}") clStr a)

  in ifSel s
  
printCubO (ee , c) cpd addr (Hcomp _ nm pa a) = 
  let c' = addDimToContext c nm
      nm' = head (withPlaceholdersDimSymbols c')
      sides = Map.toList (Map.mapMaybe id $ toMapFLI $ cylCub pa)
         & map (\(sf@(SubFace _ m) , e) -> let
                                sf2 = Map.mapKeys (fromDimI c) m
                                
                                c2 = addSF2ConstraintToContext sf2 c'               
                                bo = printCubO (ee , c2) cpd (onCyl addr sf) e
                                fc = either id id (toCode (ee , c) sf2)
                            in
                              if isCoveredIn (occupiedCylCells pa) sf
                              then Nothing
                              else Just ( (parr $ parr fc ++ " = i1") ++ " → " ++ bo ++ "\n")
                       )
         & catMaybes
         & intercalate ";"

  
      y = (printCubO (ee , c) cpd (onBottom addr (fullSF (getDim a))) (clInterior a))
        
 
      s = ("hcomp " ++ "(λ " ++ (nm') ++ " → λ  { " ++ (indent 5 ("\n" ++ sides)) ++ "})\n" ++ parr y )      
  in if Just addr == cpdCursorAddress cpd
     then bgColor Green s
     else s


makeFakeCtx :: OfDim a => a -> (Env , Context)
makeFakeCtx x =
  let n = getDim x
      dims = [ (Just y , Nothing) | y <- take n ["i","j","k","l","m"]]
      vars = [ ("x" ++ show k , undefined) | k <- [0..8]]
  in (undefined , Context vars dims)

printOOutOfCtx :: OCub a -> String
printOOutOfCtx oCub =
  let ee = makeFakeCtx oCub
  in printCubO ee (CubPrintData Nothing Nothing) undefined oCub


printClOutOfCtx :: ClCub a -> String
printClOutOfCtx clCub =
  let ee = makeFakeCtx clCub
  in printCub ee (CubPrintData Nothing Nothing) clCub




printCub' :: (Env , Context) -> CubPrintData ->  ClCub b -> String 
printCub' (ee , c) cpd x =
  printCubO' (ee , c) cpd (rootAddress (getDim x)) $ clInterior x


wrapInAddrSpan :: Address -> String -> String 
wrapInAddrSpan addr s =
     "<span class=\\\"addrWrapper\\\" id=\\\""
   ++ "addr-wrap-" ++ show (Just addr)
   ++ "\\\">"
   ++ s
   ++ "</span>"
  
printCubO' :: (Env , Context) -> CubPrintData -> Address -> OCub b -> String 
printCubO' (ee , c) cpd addr (Cub _ _ a) = 
  let
      isSelectedNode = Just addr == cpdCursorAddress cpd
      ifSel = id

      clStr (CellExpr (VarIndex k) tl) =
         let sym = fromRight (error "!!") $ getVarSymbol c k
             tailStr = concat $ intersperse (ifSel " ") $
                       zipWith (\i eI ->
                                  let ss' = toString (ee , c) $ (remapIExpr (fromDimI c )) $ eI
                                      ss = "("++ss'++")"
                                  in ss
                                  
                               ) [0..] tl
         in "(" ++ sym ++  tailStr ++ ")"
         
      s = (maybe ("{!!}") clStr a)

  in wrapInAddrSpan addr (ifSel s)
  
printCubO' (ee , c) cpd addr (Hcomp _ nm pa a) = 
  let c' = addDimToContext c nm
      nm' = head (withPlaceholdersDimSymbols c')
      sides = Map.toList (Map.mapMaybe id $ toMapFLI $ cylCub pa)
         & map (\(sf@(SubFace _ m) , e) -> let
                                sf2 = Map.mapKeys (fromDimI c) m
                                
                                c2 = addSF2ConstraintToContext sf2 c'               
                                bo = printCubO' (ee , c2) cpd (onCyl addr sf) e
                                fc = either id id (toCode (ee , c) sf2)
                            in
                              if isCoveredIn (occupiedCylCells pa) sf
                              then Nothing
                              else Just ( (parr $ parr fc ++ " = i1") ++ " → " ++ bo ++ "\n")
                       )
         & catMaybes
         & intercalate ";"

  
      y = (printCubO' (ee , c) cpd (onBottom addr (fullSF (getDim a))) (clInterior a))
        
 
      s = ("hcomp " ++ "(λ " ++ (nm') ++ " → λ  { " ++ (indent 5 ("\n" ++ sides)) ++ "})\n" ++ parr y )      
  in wrapInAddrSpan addr s
