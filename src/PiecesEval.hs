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

cornerFilter :: Subset -> Set.Set (Int , Bool) -> Set.Set (Int , Bool)
cornerFilter su@(Subset _ ss) s =
  let n = sizeLI su

      f i x =
        case (Set.size x) of
          0 -> Set.empty
          1 -> x
          2 -> Set.singleton (i , Set.notMember i ss) 
          _ -> error "imposible!"
            
  in Set.unions $ fmap (\i -> f i $ Set.filter (((==) i) . fst ) s) (range n)

permFilter :: Permutation -> Set.Set (Set.Set (Int , Bool)) -> Set.Set (Set.Set (Int , Bool))
permFilter pm@(Permutation pmm) s =
  let n = sizeLI pm

      cmp :: Int -> Int -> Ordering
      cmp x y = compare (pmm ! x) (pmm ! y)
      
      -- s1 = Set.map (sortBy cmp . Set.toList) s
  in undefined

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
  in PieceExprNNF vi  (assert (all isRight tl2 ) tl2) 


handleFaceMaps :: (Env , Context) -> PieceExprNNF -> PieceExpr
handleFaceMaps = undefined  

pieceExprNormal :: (Env , Context) -> PieceExprNNF -> PieceExpr
pieceExprNormal = undefined  
         
piecesEval :: (Env , Context) -> CellExpr -> FromLI Piece PieceExpr
piecesEval ec@(e , c) ce =
  FromLI (getDim ec) (pieceExprNormal ec . pieceEval ce )
