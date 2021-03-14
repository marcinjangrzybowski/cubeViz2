module PiecesEval where

import qualified Data.Set as Set

import Syntax

import Combi

import Data.List

import Data.Sort

import Data.Map

import Data.Ord


import Data.Either

import Control.Exception.Base

import qualified Data.Bifunctor as Bf

import Debug.Trace

import DataExtra

pieceComp :: Piece -> (Int , Bool) -> (Int , Bool) -> Ordering
pieceComp (su@(Subset _ suS) , pm@(Permutation pmm)) (x , xb) (y , yb) = 
      let xB = (xor (not (Set.member x suS)) xb)
          yB = (xor (not (Set.member y suS)) yb)
      in
      case (compare xB yB) of
        EQ -> negCompIf xB (compare (pmm ! y) (pmm ! x))
        z -> z

iExprPE :: Piece -> IExpr -> (Int , Bool)
iExprPE pc =
  elimIExpr g
  where
    
    cmp = pieceComp pc
    
    g :: [[(Int,Bool)]] -> (Int , Bool)
    g =  maximumBy cmp . (fmap (minimumBy cmp))  
     

pieceEval :: CellExpr -> Piece -> PieceExpr
pieceEval (CellExpr vi tl) pc =
  let tl2 = (fmap $ iExprPE pc) tl
  in PieceExpr vi tl2 

      
piecesEval :: Int -> CellExpr -> FromLI Piece PieceExpr
piecesEval n ce =
  FromLI n (f)

  where
    f :: Piece -> PieceExpr 
    f = pieceEval ce
