module DrawExpr where

import Abstract

import Syntax

import Drawing.Base

import Data.Bifunctor

import qualified Data.Map as Map

hcompDrawings :: (Drawing a) -> (Map.Map Face (Drawing a)) -> Drawing a 
hcompDrawings = undefined

drawExpr :: ((Env , Context) , Expr) -> DrawingGL 
drawExpr x =
       foldFaces hcompDrawings
     $ bimap
       (const ())
       (const (translate [0.1 , 0.1] $ scale 0.8 (Drawing [ ( unitHyCube 2  , SShape [ Rgba 0.0 1.0 1.0 1.0 ]  ) ]))  )
       ((toCub x) :: (Cub ((Env , Context) , Expr) CellExpr))
    
