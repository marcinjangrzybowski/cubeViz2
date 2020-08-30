{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module DrawExpr where

import Abstract

import Syntax

import Drawing.Base
import Drawing.Color

import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map

import Control.Arrow

import Combi

import FloatTools

import Data.Bool
import Data.List
import qualified Data.Set as Set

import Data.Tuple.Extra

import PiecesEval

import Reorientable

-- defaultCompPar = 0.3      

-- hcompDrawingsOnlyFaces :: (Drawing a) -> (Map.Map Face (Drawing a)) -> Drawing a 
-- hcompDrawingsOnlyFaces bot sides =
--    combineDrawings
--     ((mapCoords (map $ centerTransInv defaultCompPar) bot) :
--       (map snd
--        $ Map.toList
--        $ Map.mapWithKey (
--           \fc@(Face n (i , b)) ->
             
--               (mapCoords
--              $ ambFnOnArr
--              $ (sideTransInv defaultCompPar True) fc)
--              . transposeDrw i
--           ) sides))


-- subFaceTrans :: SubFace -> Drawing (MetaColor a) -> Drawing (MetaColor a)
-- subFaceTrans sf@(SubFace n m) drw =
--   case (Map.toList m) of
--     [] -> emptyDrawing
--     -- _ : [] -> emptyDrawing 
--     (i , b)  : js ->
--       let augmentedWithMissingTail =
--              foldl (flip (\(i , b) -> embed (i - 1) (const $ if b then 1.0 else 0.0)))
--                (drw) js

--           transformed = (mapCoords
--              $ ambFnOnArr
--              $ (sideTransInv defaultCompPar True) (Face n (i , b)))
--              $ transposeDrw i (augmentedWithMissingTail)

--           extruded = 
--             foldl (flip
--                (\ (k , (i , b)) -> extrude (fmap ((*) (-0.1)) (versor n i)) ))
--               transformed (zip [0..] js)
--       in extruded

-- hcompDrawings :: (Drawing (MetaColor a))
--      -> (Map.Map SubFace (Drawing (MetaColor a)))
--      -> Drawing (MetaColor a) 
-- hcompDrawings bot sides =
--    combineDrawings
--     (
--       (mapCoords (map $ centerTransInv defaultCompPar) (bot)) :
--       (map snd
--        $ Map.toList
--        $ Map.mapWithKey subFaceTrans sides)
--      ) 
    


-- collectDrawings :: Cub a (Drawing (MetaColor b)) -> (Drawing (MetaColor b))
-- collectDrawings =
--   -- foldSubFaces hcompDrawings
--   foldFaces hcompDrawingsOnlyFaces



-- -- drawExpr0 :: ((Env , Context) , Expr) -> DrawingGL 
-- -- drawExpr0 x =
-- --      debugRainbow $
-- --        collectDrawings
-- --      $ Bf.second
-- --        -- (const ())
-- --        (const (
-- --               translate [0.01 , 0.01] $
-- --               scale 0.98 $
-- --               Drawing [ ( unitHyCube 2  , SShape ( Rgba 0.0 1.0 1.0 1.0 )  ) ])  )
-- --        -- (makeGrid 2 3)
-- --        ((toCub x) :: (Cub ((Env , Context) , Expr) CellExpr))


-- -- type Step1 = 

-- type CellPainter b = 
--        (Int -> ((Env , Context) , Expr) -> Address -> CellExpr -> Either String (Drawing (MetaColor b)))

-- type CellPainterT = 
--        (Int -> ((Env , Context) , Expr) -> Address -> CellExpr -> Either String DrawingGL)

       


-- -- nThSimplexInNCube :: Piece -> Prll
-- -- nThSimplexInNCube = undefined 

-- pieceMask :: Piece -> Prll
-- pieceMask (su , pm) =
--   let crnr = toListLI su
--       n = length crnr
--       ptCrnr = map (bool 0.0 1.0) $ crnr
--   in (n , ptCrnr : snd (mapAccumL
--         (  (fmap dupe) . (flip $ updateAt 0.5)             
--             ) ptCrnr
--           (toListLI $ invPerm pm)))
        
--   --   -- (2 , [[0,0] , [0,0] , [1,0] , [0 , 1]]) 
--   --  fst $ head $ (\(Drawing l) -> l)
--   -- $ translate (map (bool 0.0 0.5) (toListLI su)) $ scale 0.5
--   -- $ Drawing [ (unitHyCube (getDim su) , ())]


-- -- fmap fst $ Data.List.filter (any (\x ->  Data.Set.size x > 2) . snd ) $ zip [0..] (Data.List.map ((fmap Data.Set.fromList) . transpose .snd . pieceMask) $ genAllLI 3)

-- combinePieces :: FromLI Piece (Drawing a) -> Drawing (MetaColor a)
-- combinePieces =
--    combineDrawings . mapOnAllLI (\pc -> masked (pieceMask pc)) 
--   -- combineDrawings . mapOnAllLI (\pc -> unmasked )
  
-- -- drawCellSquare ::  CellPainter
-- -- drawCellSquare _ _ _ _ = 
-- --   Right $ Drawing [ ( unitHyCube 2  , SShape (Rgba 0.0 1.0 1.0 1.0 )  ) ]

-- -- drawCellSquareMask ::  CellPainter
-- -- drawCellSquareMask _ _ _ _ = 
-- --   Right $ Drawing [ ( unitHyCube 2  , Mask  ) ]

-- -- type CellExprPainter a = ((Env , Context) , Expr) -> 


-- class (Colorlike b , Diagonable d) => DrawingCtx a b d | a -> b d where
--   fromCtx :: (Env , Context) -> a

--   drawGenericTerm :: ((Env , Context) , Expr) -> a -> Piece -> VarIndex -> d

--   --Drawing b

--   drawD :: Never a -> d -> Drawing b

--   drawCellCommon :: ((Env , Context) , Expr) -> a -> CellExpr -> Drawing b
--   drawCellCommon _ _ _ = emptyDrawing
  
--   drawCellPiece :: ((Env , Context) , Expr) -> a -> Piece -> PieceExpr -> Drawing b  
--   drawCellPiece eee@((_ , ctx) , _) a pc (PieceExpr h t) =
--      -- let t2 = fmap (Bf.first $ toDimI ctx) t
--      -- in    
--      drawD (forget a) $ remapTL (getDim eee) t $ drawGenericTerm eee a pc h

--   cellPainter :: Never a -> a -> CellPainter b
--   cellPainter na dctx n eee@(ee@(env , ctx) ,  expr) adr ce = 
--     Right $
--      combineDrawings $ [
--        unmasked (drawCellCommon eee dctx ce)
--        ,
--        (combinePieces (fromLIppK (drawCellPiece eee dctx) (piecesEval ee ce)))
--        ]
-- -- TODO dimension of eee

--   mkDrawExpr :: Never a  -> ((Env , Context) , Expr) -> Either String (Drawing (MetaColor b))
--   mkDrawExpr na w@( envCtx , _ ) =
--       let  dctx = fromCtx envCtx
--       in
     
--       (    toCub
--       >>> cubMap (getDim w) (cellPainter na dctx) []
--       >>> fmap (collectDrawings)) w

-- -- instance DrawingCtx () () where    
-- --   fromCtx _ _ = ()
-- --   drawGenericTerm _ _ _ _ =
-- --      let rDrw = ()

-- --      in  Drawing [ ( unitHyCube 1  , ()  ) ]

-- instance DrawingCtx () (Color) (Maybe ((Int , Color) , [Int])) where    
--   fromCtx _ = ()

--   drawCellCommon eee _ _ = emptyDrawing
--       -- Drawing [ ( unitHyCube (getDim eee)  , gray 0.3   ) ]

--   drawGenericTerm ((ee , ctx) , _) _ pc vi =
--     if True --(unemerate pc == 28) -- && unemerate pc < 30)
--     then Just ((getCTyDim ee ctx $ getVarType ctx vi ,
--           -- nthColor (unemerate pc)
--         gray 0.8
--           ) ,  [])
--     else Nothing

--   drawD _ (Just ( (1 , cl) , dg )) =
--      let d0 = Drawing [

--           --  ( (1 , [[0.2],[0.3]])  , cl   )
--           -- , ( (1 , [[0.7],[0.8]])  , cl   )
           

           
--             ( (1 , [[0.2],[0.25]])  , cl   )
--            , ( (1 , [[0.45],[0.55]])  , cl   )
--            ,( (1 , [[0.75],[0.8]])  , cl   )
--            ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
    
--   drawD _ (Just ( (k , cl) , dg )) =
--      let d0 = translate (replicate k 0.2)  $
--            scale 0.6 $ Drawing [ ( unitHyCube (k)  , cl   ) ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
     
--   drawD _ Nothing = emptyDrawing

-- data Stripes1 = Stripes1

-- data Field1 = Field1

-- instance DrawingCtx Stripes1 (Color) (Maybe (Either Color (Color , Color) , [Int])) where    
--   fromCtx _ = Stripes1

--   drawCellCommon eee _ _ = emptyDrawing
--       -- Drawing [ ( unitHyCube (getDim eee)  , gray 0.3   ) ]

--   drawGenericTerm ((ee , ctx) , _) _ pc vi@(VarIndex viK) =
--     let (CType _ l) = (getVarType ctx vi) in
--     case l of
--       [] -> Just (Left (nthColor viK)  , [])
--       [ (Var (VarIndex viK0) [] , Var (VarIndex viK1) []) ] ->
--           Just (Right ((nthColor viK0) , (nthColor viK1) ) , [])
--       _ -> error "this convention is not suitable for types of dim>1"
--     -- Just ((getCTyDim ee ctx $ getVarType ctx vi ,
--     --       -- nthColor (unemerate pc)
--     --     gray 0.8
--     --       ) ,  [])
    

--   drawD _ (Just ( Right (c0 , c1)  , dg )) =
--      let d0 = Drawing [

--           --  ( (1 , [[0.2],[0.3]])  , cl   )
--           -- , ( (1 , [[0.7],[0.8]])  , cl   )
           

           
--             ( (1 , [[0.2],[0.25]])  , c0   )
--            -- , ( (1 , [[0.45],[0.55]])  , gray 0.5   )
--            ,( (1 , [[0.65],[0.7]])  , c1   )
--            ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
    
--   drawD _ (Just ( Left cl , dg )) =
--      let d0 = Drawing [ ( unitHyCube 0  , cl   ) ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
     
--   drawD _ Nothing = emptyDrawing

-- instance DrawingCtx Field1 (Color) ((Either Color (Color , Color) , [Int])) where    
--   fromCtx _ = Field1

--   drawCellCommon eee _ _ = emptyDrawing
--       -- Drawing [ ( unitHyCube (getDim eee)  , gray 0.3   ) ]

--   drawGenericTerm ((ee , ctx) , _) _ pc vi@(VarIndex viK) =
--     let (CType _ l) = (getVarType ctx vi) in
--     case l of
--       [] -> (Left (nthColor viK)  , [])
--       [ (Var (VarIndex viK0) [] , Var (VarIndex viK1) []) ] ->
--            (Right ((nthColor viK0) , (nthColor viK1) ) , [])
--       _ -> error "this convention is not suitable for types of dim>1"
--     -- Just ((getCTyDim ee ctx $ getVarType ctx vi ,
--     --       -- nthColor (unemerate pc)
--     --     gray 0.8
--     --       ) ,  [])
    

--   drawD _ ( ( Right (c0 , c1)  , dg0 )) =
--      let
--          k = length dg0
--          -- d0 = translate (replicate k 0.4)  $
--          --          scale 0.2 $ Drawing [ ( unitHyCube (k)  , gray 0.3   ) ]

--          d0 = grid k 3 (gray 0.4)

--          dg :: [Int]
--          dg = [ length $ takeWhile id $ zipWith (==) dg0 [0..]  ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
    
--   drawD _ (( Left cl , dg )) = emptyDrawing
--      -- let d0 = Drawing [ ( unitHyCube 0  , cl   ) ]
--      -- in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
     
  
  
  
-- drawExpr = mkDrawExpr (forget Stripes1)
          
