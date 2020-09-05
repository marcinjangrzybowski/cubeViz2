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

iExprPE :: Piece -> IExpr -> Either Bool (Int , Bool)
iExprPE pc =
  setSetElim (Left False) (Left True)
    (Right . g)
  where
    
    cmp = pieceComp pc
    
    g :: [[(Int,Bool)]] -> (Int , Bool)
    -- g = maximum . (fmap (minimum))
    g =  maximumBy cmp . (fmap (minimumBy cmp))  
     
-- (fmap (iExprPE . enumerate 1) [0 , 1]) <$> [end True , end False ]

-- reorientExpr :: CellExpr -> [ Either Bool (Int, Bool) ] -> PieceExpr
-- reorientExpr = undefined

pieceEval :: CellExpr -> Piece -> PieceExprNNF
pieceEval (CellExpr vi tl) pc =
  let tl2 = (fmap $ iExprPE (Bf.second id pc)) tl
  in PieceExprNNF vi tl2 


-- handleFaceMaps :: (Env , Context) -> PieceExprNNF -> PieceExpr
-- handleFaceMaps = undefined  

pieceExprNormal :: (Env , Context) -> PieceExprNNF -> PieceExpr
pieceExprNormal _ (PieceExprNNF vi tl) =
    case (partitionEithers tl) of
      ([] , tl2) -> (PieceExpr vi tl2)  
      _ -> undefined
      
piecesEval :: (Env , Context) -> CellExpr -> FromLI Piece PieceExpr
-- piecesEval ec@(e , c) ce | trace ("myfun " ++ show ce) False = undefined
piecesEval ec@(e , c) ce =
  FromLI (getDim ec) (f)

  where
    f :: Piece -> PieceExpr 
    f = pieceExprNormal ec . pieceEval (remapCE (toDimI c) ce)

    -- g :: PieceExpr -> PieceExpr
    -- -- g _ (PieceExpr i [(0 , True)]) = (PieceExpr i [(1 , True)])
    -- g x | trace ("myfun " ++ show ce ++ " " ++ show x) False = undefined
    -- g x = x
