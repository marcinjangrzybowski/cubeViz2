module PiecesEval where

import qualified Data.Set as Set

import Syntax

import Combi

import Data.List

import Data.Sort

import Data.Map



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
  in undefined

iExprPE :: Piece -> IExpr -> (Int , Bool)
iExprPE (su , pm) ie =
  let ie1 = Set.map (Set.filter undefined) ie
      
  in undefined

piecesEval :: CellExpr -> FromLI Piece PieceExpr
piecesEval (CellExpr k tl) = undefined
