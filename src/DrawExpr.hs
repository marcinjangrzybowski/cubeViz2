{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DrawExpr where

import Abstract

import Syntax

import Drawing.Base
import Drawing.Color

import Data.Maybe
import Data.Either

import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map

-- import Control.Arrow

import Combi

import FloatTools

import Data.Bool
import Data.List
import qualified Data.Set as Set

import Data.Tuple.Extra (second , first)

import PiecesEval

import Reorientable

import DataExtra

defaultCompPar = 0.3      

hcompDrawingsOnlyFaces :: (Drawing a , SideTransMode ) -> (FromLI Face (Drawing a  , SideTransMode)) -> Drawing a 
hcompDrawingsOnlyFaces (bot , _) sides =
   concat
    ((sMap (map $ centerTransInv defaultCompPar) bot) : 
          (toListFLI (fromLIppK (
                         \fc@(Face n (i , b)) -> \(dr , stm) -> 
                                      (sMap
                                      $ ambFnOnArr
                                      $ (sideTransInv defaultCompPar stm) fc)
                                      $ transposeDrw i dr


                                ) sides))   
    )

subFaceTrans :: SubFace -> Drawing a -> Drawing a
subFaceTrans sf@(SubFace n m) drw =
  case (Map.toList m) of
    [] -> mempty
    -- _ : [] -> emptyDrawing 
    (i , b)  : js ->
      let
          -- putting subface in one of the adjant face to execute face transform
          augmentedWithMissingTail =
             foldl (flip (\(i , b) -> embed (i - 1) (const $ if b then 1.0 else 0.0)))
               (drw) js

          transformed = (sMap
             $ ambFnOnArr
             $ (sideTransInv defaultCompPar STSubFace) (Face n (i , b)))
             $ transposeDrw i (augmentedWithMissingTail)

          -- extruded :: Drawing a
          -- extruded = undefined 
          --   -- foldl (flip
          --   --    (\ (k , (i , b)) -> extrude (fmap ((*) (-0.1)) (versor n i)) ))
          --   --   transformed (zip [0..] js)
      in transformed

hcompDrawings :: (Drawing a)
     -> (Map.Map SubFace (Drawing a))
     -> Drawing a 
hcompDrawings bot sides =
   concat
    (
      (sMap (map $ centerTransInv defaultCompPar) (bot)) :
      (map snd
       $ Map.toList
       $ Map.mapWithKey subFaceTrans sides)
     ) 
    


collectDrawings :: Cub (Drawing b) -> (Drawing b)
collectDrawings =
  foldSubFaces hcompDrawings
  -- foldFaces hcompDrawingsOnlyFaces
  
-- collectFaces :: Cub (Drawing b , SideTransMode) -> (Drawing b)
-- collectFaces =
--   foldSubFaces hcompDrawingsOnlyFaces

type CellPainter b = 
       (Int -> Address -> CellExpr -> Either String (Drawing b))




class (Colorlike b , DiaDeg c , Extrudable b) => DrawingCtx a b c | a -> b c where
  -- a is precomputed data, config, COMMON FOR ALL CELLS,
  -- c is some abstract escriptoon
  -- b is colorlike

  fromCtx :: (Env , Context) -> a

  -- IMPORTANT : (Env , Context) everywhere bellow is global! , cant be trustet with dimensions!! 

  -- HERE PIECE ARGUMENT IS ADDITIONAL!!!, for some special application!
  -- TODO :: Recover local context in cellPainter using Address
  drawGenericTerm :: (Env , Context) -> a -> Piece -> VarIndex -> c

  --Drawing b

  drawD :: Never a -> c -> ZDrawing b

  drawCellCommon :: (Env , Context) -> Int -> a -> CellExpr -> Drawing b
  drawCellCommon _ _ _ _ = []
  
  drawCellPiece :: (Env , Context) -> Int -> a -> PieceExpr -> (Piece -> Drawing b)  
  drawCellPiece ee n a (PieceExpr h t) =     
     (\pc -> appLI pc (remapTL (drawD (forget a)) n t $ drawGenericTerm ee a pc h))


  cellPainter :: (Env , Context) -> Never a -> a -> CellPainter b
  cellPainter ee na dctx n adr ce =
     let zz = fmap (FromLI n . (drawCellPiece ee n dctx)) (piecesEval n ce)
     in Right $
          drawCellCommon ee n dctx ce
            ++
          (evalLI zz)
            

  mkDrawExpr :: Never a  -> ((Env , Context) , Expr) -> Either String (Drawing b)
  mkDrawExpr na w@( envCtx , _ ) = 
      let  dctx = fromCtx envCtx
      in

      do let cub = toCub w
         cubDrw <- cubMap (cellPainter envCtx na dctx )  [] cub
         return $ collectDrawings cubDrw

  fillStyleProcess :: Never a -> Drawing b -> Drawing b
  fillStyleProcess _ = id

  -- TODO :: raczej zrezygnowac z Either w calej tej klasie...
  mkDrawExprFill :: Never a  -> ((Env , Context) , Expr) -> Either String (Drawing b)
  mkDrawExprFill na w@( envCtx , _ ) = 
      let  dctx = fromCtx envCtx


           fillFaces (nm , sides , bot) =
               Map.mapKeys toSubFace
             $ Map.fromSet
               (\fc -> (
                   let d = fromRight [] $ (drawCub (cubFace fc (Hcomp nm sides bot)))
                       d1 = fillStyleProcess na $ extrude (getDim bot) (piramFn defaultCompPar , const 1) d
                       -- d1 = embed (getDim bot) (const 1) d
                   in Cub (getDim bot) undefined (d1 , STFill)
                   ) )
               ((Set.difference (setOfAll (getDim bot)) (Set.fromList $ mapMaybe toFace $ Map.keys sides )) )
               
           -- drawCub :: Cub CellExpr -> Either String (Drawing b)
           drawCub cub =
             do cubDrw <- cubMapFill ((fmap $ (flip (,) STSubFace)) `dot3` (cellPainter envCtx na dctx) )
                              fillFaces
                              [] cub 
                return $ fst (foldFacesFiled ((flip (,) STSubFace) `dot2` hcompDrawingsOnlyFaces ) cubDrw)

      in

      drawCub $ toCub w 


-- XXXX


-- simpleDrawTerm 0 = FromLI 0 (const [([[]]  , ([] , nthColor 4)) ] )
simpleDrawTerm 1 =
    FromLI 1 (bool [([[0.2],[0.3]] , (([] , Basic) , nthColor 1))]
                   [([[0.6],[0.7]] , (([] , Basic) , nthColor 2))] . fst . head . toListLI)
simpleDrawTerm n = FromLI n (const [])

instance DrawingCtx () (([String] , ExtrudeMode) , Color) Int where    
  fromCtx _ = ()
  drawGenericTerm (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  


  drawD _ k = fmap (fmap $ second $ first $ first $ (:) "term") (simpleDrawTerm k)
  
    -- FromLI 1 (bool [ ([[0.3]] , nthColor 1)] [ ([[1 - 0.35]] , nthColor 2)] . fst . head . toListLI)
    -- FromLI 1 (bool [([[0.2],[0.23]] , ())] [] . fst . head . toListLI)

    -- FromLI 1 (bool [([[0.2]] , ()) , ([[0.3]] , ())]
    --                [([[0.6]] , ()) , ([[0.7]] , ())  ]
    --            . fst . head . toListLI)

  
  drawCellCommon _ n _ _ = 
       if n > 1
       then  fmap (Bf.second $ const $ ( (["cellBorder"] , ExtrudeLines) , gray 0.8))
               $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 1
       else []

data ScaffoldPT = ScaffoldPT

instance DrawingCtx ScaffoldPT (([String] , ExtrudeMode) , Color) Int where    
  fromCtx _ = ScaffoldPT
  drawGenericTerm (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  


  drawD _ k = FromLI k (const [])
  
    -- FromLI 1 (bool [ ([[0.3]] , nthColor 1)] [ ([[1 - 0.35]] , nthColor 2)] . fst . head . toListLI)
    -- FromLI 1 (bool [([[0.2],[0.23]] , ())] [] . fst . head . toListLI)

    -- FromLI 1 (bool [([[0.2]] , ()) , ([[0.3]] , ())]
    --                [([[0.6]] , ()) , ([[0.7]] , ())  ]
    --            . fst . head . toListLI)


  fillStyleProcess _ =
    map (\(s , a@((tags , em) , c) ) ->
           case em of
             ExtrudeLines -> (s , ((tags , em) , Rgba 0.8 0.8 0.8 0.1))
             _ -> (s , a) 
        )
    
  drawCellCommon _ n _ _ = 
       if n > -1
       then  fmap (Bf.second $ const $ (( ["cellBorder"] , ExtrudeLines) , gray 0.5))
               $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 1
       else []



data DrawExprMode = Stripes | StripesNoFill | Scaffold 
  deriving (Show , Eq)

drawExprModes = [ Stripes , StripesNoFill , Scaffold ]

drawExpr Stripes = mkDrawExprFill (forget ())
drawExpr StripesNoFill = mkDrawExpr (forget ())
drawExpr Scaffold  = mkDrawExprFill (forget ScaffoldPT)

  
-- drawExpr = mkDrawExpr (forget Stripes1)
          
