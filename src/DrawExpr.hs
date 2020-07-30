module DrawExpr where

import Abstract

import Syntax

import Drawing.Base

import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map

import Control.Arrow

import Combi

import FloatTools

import Data.Bool
import Data.List

import Data.Tuple.Extra

defaultCompPar = 0.3      

hcompDrawings :: (Drawing a) -> (Map.Map Face (Drawing a)) -> Drawing a 
hcompDrawings bot sides =
   combineDrawings
    ((mapCoords (map $ centerTransInv defaultCompPar) bot) :
      (map snd $ Map.toList $ Map.mapWithKey (mapCoords . ambFnOnArr. (sideTransInv defaultCompPar True)) sides)
     ) 


collectDrawings :: Cub a DrawingGL -> DrawingGL
collectDrawings = foldFaces hcompDrawings



drawExpr0 :: ((Env , Context) , Expr) -> DrawingGL 
drawExpr0 x =
     debugRainbow $
       collectDrawings
     $ Bf.second
       -- (const ())
       (const (
              translate [0.01 , 0.01] $
              scale 0.98 $
              Drawing [ ( unitHyCube 2  , SShape ( Rgba 0.0 1.0 1.0 1.0 )  ) ])  )
       -- (makeGrid 2 3)
       ((toCub x) :: (Cub ((Env , Context) , Expr) CellExpr))


-- type Step1 = 

type CellPainter = 
       (Int -> ((Env , Context) , Expr) -> Address -> CellExpr -> Either String DrawingGL)   


-- nThSimplexInNCube :: Piece -> Prll
-- nThSimplexInNCube = undefined 

pieceMask :: Piece -> Prll
pieceMask (su , pm) =
  let crnr = toListLI su
      n = length crnr
      ptCrnr = map (bool 0.0 1.0) $ crnr
  in (n , ptCrnr : snd (mapAccumL
        (  (fmap dupe) . (flip $ updateAt 0.5)             
            ) ptCrnr
          (toListLI pm)))
        
  --   -- (2 , [[0,0] , [0,0] , [1,0] , [0 , 1]]) 
  --  fst $ head $ (\(Drawing l) -> l)
  -- $ translate (map (bool 0.0 0.5) (toListLI su)) $ scale 0.5
  -- $ Drawing [ (unitHyCube (getDim su) , ())]

combinePieces :: FromLI Piece (Drawing a) -> Drawing (MetaColor a)
combinePieces = combineDrawings . mapOnAllLI (\pc -> masked (pieceMask pc)) 

drawCellSquare ::  CellPainter
drawCellSquare _ _ _ _ = 
  Right $ Drawing [ ( unitHyCube 2  , SShape (Rgba 0.0 1.0 1.0 1.0 )  ) ]

drawCellSquareMask ::  CellPainter
drawCellSquareMask _ _ _ _ = 
  Right $ Drawing [ ( unitHyCube 2  , Mask  ) ]



type CellExprPainter a = ((Env , Context) , Expr) -> CellExpr -> Drawing a

drawCellDefault ::  CellPainter
drawCellDefault n eee@(ee@(env , ctx) ,  expr) adr ce = 
  Right $ combinePieces (FromLI (getDim eee) (
           \pc -> Drawing [ ( unitHyCube 2  ,  (nthColor (unemerate pc) )  ) ]
                                             ))
-- TODO dimension of eee


mkDrawExpr :: CellPainter -> ((Env , Context) , Expr) -> Either String DrawingGL
mkDrawExpr drawCell =
          toCub
      >>> cubMap 2 drawCell []
      >>> fmap (collectDrawings)
      -- >>> fmap (debugRainbow . extractMasks)


drawExpr :: ((Env , Context) , Expr) -> Either String DrawingGL 
drawExpr = mkDrawExpr drawCellDefault
          
