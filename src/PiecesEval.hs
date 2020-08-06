module PiecesEval where

import qualified Data.Set as Set

import Syntax

import Combi

import Data.List

import Data.Sort

import Data.Map

import Data.Ord

import Abstract

import Data.Either

import Control.Exception.Base

xor :: Bool -> Bool -> Bool
xor b False = b
xor b True = not b

negCompIf :: Bool -> Ordering -> Ordering
negCompIf True LT = GT
negCompIf True GT = LT
negCompIf _ x = x


iExprPE :: Piece -> IExpr -> Either Bool (Int , Bool)
iExprPE (su@(Subset _ suS) , pm@(Permutation pmm)) =
  setSetElim (Left False) (Left True)
    (Right . g)
  where
    cmp :: (Int , Bool) -> (Int , Bool) -> Ordering
    cmp (x , xb) (y , yb) =
      let xB = (xor (not (Set.member x suS)) xb)
          yB = (xor (not (Set.member y suS)) yb)
      in
      case (compare xB yB) of
        EQ -> negCompIf xB (compare (pmm ! x) (pmm ! y))
        z -> z

    g :: [[(Int,Bool)]] -> (Int , Bool)
    g =  maximum . fmap minimum  
     

-- reorientExpr :: CellExpr -> [ Either Bool (Int, Bool) ] -> PieceExpr
-- reorientExpr = undefined

pieceEval :: CellExpr -> Piece -> PieceExprNNF
pieceEval (CellExpr vi tl) pc =
  let tl2 = (fmap $ iExprPE pc) tl
  in PieceExprNNF vi tl2 


-- handleFaceMaps :: (Env , Context) -> PieceExprNNF -> PieceExpr
-- handleFaceMaps = undefined  

pieceExprNormal :: (Env , Context) -> PieceExprNNF -> PieceExpr
pieceExprNormal _ (PieceExprNNF vi tl) =
    case (partitionEithers tl) of
      ([] , tl2) -> (PieceExpr vi tl2)  
      _ -> undefined
      
piecesEval :: (Env , Context) -> CellExpr -> FromLI Piece PieceExpr
piecesEval ec@(e , c) ce =
  FromLI (getDim ec) (pieceExprNormal ec . pieceEval ce )
