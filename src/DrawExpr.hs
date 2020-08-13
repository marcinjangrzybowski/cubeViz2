{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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

import PiecesEval

import Reorientable

defaultCompPar = 0.3      

hcompDrawings :: (Drawing a) -> (Map.Map Face (Drawing a)) -> Drawing a 
hcompDrawings bot sides =
   combineDrawings
    ((mapCoords (map $ centerTransInv defaultCompPar) bot) :
      (map snd
       $ Map.toList
       $ Map.mapWithKey (
          \fc@(Face n (i , b)) ->
             
              (mapCoords
             $ ambFnOnArr
             $ (sideTransInv defaultCompPar True) fc)
             . transposeDrw i
          ) sides)
     ) 


collectDrawings :: Cub a (Drawing b) -> (Drawing b)
collectDrawings = foldFaces hcompDrawings



-- drawExpr0 :: ((Env , Context) , Expr) -> DrawingGL 
-- drawExpr0 x =
--      debugRainbow $
--        collectDrawings
--      $ Bf.second
--        -- (const ())
--        (const (
--               translate [0.01 , 0.01] $
--               scale 0.98 $
--               Drawing [ ( unitHyCube 2  , SShape ( Rgba 0.0 1.0 1.0 1.0 )  ) ])  )
--        -- (makeGrid 2 3)
--        ((toCub x) :: (Cub ((Env , Context) , Expr) CellExpr))


-- type Step1 = 

type CellPainter b = 
       (Int -> ((Env , Context) , Expr) -> Address -> CellExpr -> Either String (Drawing (MetaColor b)))

type CellPainterT = 
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
          (toListLI $ invPerm pm)))
        
  --   -- (2 , [[0,0] , [0,0] , [1,0] , [0 , 1]]) 
  --  fst $ head $ (\(Drawing l) -> l)
  -- $ translate (map (bool 0.0 0.5) (toListLI su)) $ scale 0.5
  -- $ Drawing [ (unitHyCube (getDim su) , ())]


-- fmap fst $ Data.List.filter (any (\x ->  Data.Set.size x > 2) . snd ) $ zip [0..] (Data.List.map ((fmap Data.Set.fromList) . transpose .snd . pieceMask) $ genAllLI 3)

combinePieces :: FromLI Piece (Drawing a) -> Drawing (MetaColor a)
combinePieces =
   combineDrawings . mapOnAllLI (\pc -> masked (pieceMask pc)) 
  -- combineDrawings . mapOnAllLI (\pc -> unmasked )
  
-- drawCellSquare ::  CellPainter
-- drawCellSquare _ _ _ _ = 
--   Right $ Drawing [ ( unitHyCube 2  , SShape (Rgba 0.0 1.0 1.0 1.0 )  ) ]

-- drawCellSquareMask ::  CellPainter
-- drawCellSquareMask _ _ _ _ = 
--   Right $ Drawing [ ( unitHyCube 2  , Mask  ) ]

-- type CellExprPainter a = ((Env , Context) , Expr) -> 


class (Colorlike b , Diagonable d) => DrawingCtx a b d | a -> b d where
  fromCtx :: (Env , Context) -> a

  drawGenericTerm :: ((Env , Context) , Expr) -> a -> Piece -> VarIndex -> d

  --Drawing b

  drawD :: Never a -> d -> Drawing b

  drawCellCommon :: ((Env , Context) , Expr) -> a -> CellExpr -> Drawing b
  drawCellCommon _ _ _ = emptyDrawing
  
  drawCellPiece :: ((Env , Context) , Expr) -> a -> Piece -> PieceExpr -> Drawing b  
  drawCellPiece eee@((_ , ctx) , _) a pc (PieceExpr h t) =
     -- let t2 = fmap (Bf.first $ toDimI ctx) t
     -- in    
     drawD (forget a) $ remapTL (getDim eee) t $ drawGenericTerm eee a pc h

  cellPainter :: Never a -> a -> CellPainter b
  cellPainter na dctx n eee@(ee@(env , ctx) ,  expr) adr ce = 
    Right $
     combineDrawings $ [
       unmasked (drawCellCommon eee dctx ce)
       ,
       (combinePieces (fromLIppK (drawCellPiece eee dctx) (piecesEval ee ce)))
       ]
-- TODO dimension of eee

  mkDrawExpr :: Never a  -> ((Env , Context) , Expr) -> Either String (Drawing (MetaColor b))
  mkDrawExpr na w@( envCtx , _ ) =
      let  dctx = fromCtx envCtx
      in
     
      (    toCub
      >>> cubMap (getDim w) (cellPainter na dctx) []
      >>> fmap (collectDrawings)) w

-- instance DrawingCtx () () where    
--   fromCtx _ _ = ()
--   drawGenericTerm _ _ _ _ =
--      let rDrw = ()

--      in  Drawing [ ( unitHyCube 1  , ()  ) ]

instance DrawingCtx () (Color) (Maybe ((Int , Color) , [Int])) where    
  fromCtx _ = ()

  drawCellCommon eee _ _ = emptyDrawing
      -- Drawing [ ( unitHyCube (getDim eee)  , gray 0.3   ) ]

  drawGenericTerm ((ee , ctx) , _) _ pc vi =
    if True --(unemerate pc == 28) -- && unemerate pc < 30)
    then Just ((getCTyDim ee ctx $ getVarType ctx vi ,
          -- nthColor (unemerate pc)
        gray 0.8
          ) ,  [])
    else Nothing

  drawD _ (Just ( (1 , cl) , dg )) =
     let d0 = Drawing [

          --  ( (1 , [[0.2],[0.3]])  , cl   )
          -- , ( (1 , [[0.7],[0.8]])  , cl   )
           

           
            -- ( (1 , [[0.2],[0.25]])  , cl   )
           ( (1 , [[0.45],[0.55]])  , cl   )
           -- ,( (1 , [[0.75],[0.8]])  , cl   )
           ]
     in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
    
  drawD _ (Just ( (k , cl) , dg )) =
     let d0 = translate (replicate k 0.2)  $
           scale 0.6 $ Drawing [ ( unitHyCube (k)  , cl   ) ]
     in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
     
  drawD _ Nothing = emptyDrawing
  
  
  --Drawing [ ( unitHyCube 2  , ()  ) ]  

-- instance DrawingCtx () () where    
--   fromCtx _ = ()
--   drawCellPiece _ _ _ = Drawing [ ( unitHyCube 2  , ()  ) ]  


-- drawCellDefault ::  CellPainterT
-- drawCellDefault n eee@(ee@(env , ctx) ,  expr) adr ce = 
--   Right $ combinePieces (FromLI (getDim eee) (
--            \pc -> Drawing [ ( unitHyCube 2  ,  (nthColor (unemerate pc) )  ) ]
--                                              ))
-- -- TODO dimension of eee


-- mkDrawExprT :: CellPainterT -> ((Env , Context) , Expr) -> Either String DrawingGL
-- mkDrawExprT drawCell =
--           toCub
--       >>> cubMap 2 drawCell []
--       >>> fmap (collectDrawings)
--       -- >>> fmap (debugRainbow . extractMasks)



drawExpr = mkDrawExpr (forget ())
          
