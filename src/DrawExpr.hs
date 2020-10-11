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

import Control.Applicative

import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map

-- import Control.Arrow

import Combi

import FloatTools

import Data.Function
import Data.Bool
import Data.List
import qualified Data.Set as Set

import Data.Tuple.Extra (second , first)

import PiecesEval

import Reorientable

import DataExtra

import GenericCell

import Debug.Trace

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
          -- putting subface in one of the adjacent face to execute face transform
          augmentedWithMissingTail =
             foldl (flip (\(i , b) -> embed (i - 1) (const $ if b then 1.0 else 0.0)))
               (drw) js

          transformed = (sMap
             $ ambFnOnArr
             $ (sideTransInv defaultCompPar STSubFace) (Face n (i , b)))
             $ transposeDrw i (augmentedWithMissingTail)


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
    


collectDrawings :: Cub (Drawing b) (Drawing b) -> (Drawing b)
collectDrawings =
  foldSubFaces (\nodeData -> \centerData -> \leafsData -> nodeData ++ (hcompDrawings centerData leafsData))
  -- foldFaces hcompDrawingsOnlyFaces
  
-- collectFaces :: Cub (Drawing b , SideTransMode) -> (Drawing b)
-- collectFaces =
--   foldSubFaces hcompDrawingsOnlyFaces

type CellPainter b = 
       (Int -> Address -> FromLI Face (Cub () (Either Int CellExpr))
                    -> Either Int CellExpr -> Either String (Drawing b))




class (Colorlike b , DiaDeg c , Extrudable b) => DrawingCtx a b c d | d -> a b c where
  -- a is precomputed data, COMMON FOR ALL CELLS,
  -- c is some abstract descriptoon
  -- b is colorlike
  -- d - settings, also main type setting the rest !
  
  fromCtx :: d -> (Env , Context) -> a

  -- IMPORTANT : (Env , Context) everywhere bellow is global! , cant be trustet with dimensions!! 

  -- HERE PIECE ARGUMENT IS ADDITIONAL!!!, for some special application!
  -- TODO :: Recover local context in cellPainter using Address
  -- getting internal cell representation, 
  drawGenericTerm :: d -> (Env , Context) -> a -> Piece -> VarIndex -> c

  --Drawing b

  -- "rendering" internal cell representation to drawing in pieces
  drawD :: d -> a -> c -> ZDrawing b

  drawCellCommon :: d -> (Env , Context) -> Int -> Address -> a ->
                       FromLI Face (Cub () (Either Int CellExpr))
                    -> Either Int CellExpr -> Drawing b
  drawCellCommon _ _ _ _ _ _ _ = []
  
  drawCellPiece :: d -> (Env , Context) -> Int -> Address -> a -> PieceExpr -> (Piece -> Drawing b)  
  drawCellPiece d ee n adr a (PieceExpr h t) =  
     (\pc -> appLI pc (remapTL (drawD d a) n t $ drawGenericTerm d ee a pc h))

  drawHole :: d -> (Env , Context) -> Int -> Address -> a -> Int -> Drawing b  
  drawHole _ _ _ _ _ _ = [] 


  cellPainter :: d -> (Env , Context) -> a -> CellPainter b
  cellPainter d ee dctx n adr fcs x = 
     let zz =
           case x of
             Right ce -> evalLI (fmap (FromLI n . (drawCellPiece d ee n adr dctx)) (piecesEval n ce))
             Left hI -> drawHole d ee n adr dctx hI
     in Right $
          drawCellCommon d ee n adr dctx fcs x
            ++
          ((cellStyleProcess d ee dctx n adr fcs x) zz)
            

  nodePainter :: d -> (Env , Context) -> a ->
      (Int -> Address -> () -> Name -> (Map.Map SubFace (Cub () (Either Int CellExpr)))
                          -> (Cub () (Either Int CellExpr))
                -> Either String (Drawing b))
  nodePainter d ee dctx n addr () nm si center = Right []

  mkDrawExpr :: d  -> ((Env , Context) , Expr) -> Either String (Drawing b)
  mkDrawExpr d w@( envCtx , _ ) = 
      let  dctx = fromCtx d envCtx
      in

      do let cub = (toCub w :: Cub () (Either Int CellExpr))
         cubDrw <- cubMap (nodePainter d envCtx dctx ) (cellPainter d envCtx dctx )  [] cub
         return $ finalProcess d $ collectDrawings (cubDrw)

  fillStyleProcess :: d -> Drawing b -> Drawing b
  fillStyleProcess d = id

  cellStyleProcess :: d -> (Env , Context) -> a -> Int -> Address -> FromLI Face (Cub () (Either Int CellExpr))
                              -> Either Int CellExpr -> Drawing b -> Drawing b
  cellStyleProcess _ _ _ _ _ _ _ = id

  finalProcess :: d -> Drawing b -> Drawing b
  finalProcess d = id

  -- TODO :: raczej zrezygnowac z Either w calej tej klasie...
  mkDrawExprFill :: d -> ((Env , Context) , Expr) -> Either String (Drawing b)
  mkDrawExprFill d w@( envCtx , _ ) = 
      fmap (finalProcess d)  $ drawCub $ toCub w 

      where

           dctx = fromCtx d envCtx

           fillFaces (nm , sides , bot) =
               Map.mapKeys toSubFace
             $ Map.fromSet
               (\fc -> (
                   let d0 = fromRight [] $ (drawCub (cubFace fc (Hcomp () nm sides bot)))
                       d1 = extrude (getDim bot) (piramFn defaultCompPar , const 1) d0

                   in Cub (FromLI (getDim bot) undefined) (fillStyleProcess d $ d1 , STFill)
                   ) )
               ((Set.difference (setOfAll (getDim bot)) (Set.fromList $ mapMaybe toFace $ Map.keys sides )) )

           drawCub cub =
             do cubDrw <- cubMapFill ((fmap $ (flip (,) STSubFace)) `dot4` (cellPainter d envCtx dctx) )
                              fillFaces
                              [] cub 
                return $ fst (foldFacesFiled (const $ (flip (,) STSubFace) `dot2` hcompDrawingsOnlyFaces ) cubDrw)
        
-- XXXX


simpleDrawTerm 0 = FromLI 0 (const [([[]]  , (([] , Basic) , nthColor 4)) ] )
simpleDrawTerm 1 =
    FromLI 1 (bool [([[0.2],[0.3]] , (([] , Basic) , nthColor 1))]
                   [([[0.6],[0.7]] , (([] , Basic) , nthColor 2))] . fst . head . toListLI)
simpleDrawTerm n = FromLI n (const [])



data SimplePT = SimplePT



instance DrawingCtx () (([String] , ExtrudeMode) , Color) Int SimplePT where    
  fromCtx _ _ = ()
  drawGenericTerm _ (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  


  drawD _ _ k = fmap (fmap $ second $ first $ first $ (:) "term") (simpleDrawTerm k)
  
    -- FromLI 1 (bool [ ([[0.3]] , nthColor 1)] [ ([[1 - 0.35]] , nthColor 2)] . fst . head . toListLI)
    -- FromLI 1 (bool [([[0.2],[0.23]] , ())] [] . fst . head . toListLI)

    -- FromLI 1 (bool [([[0.2]] , ()) , ([[0.3]] , ())]
    --                [([[0.6]] , ()) , ([[0.7]] , ())  ]
    --            . fst . head . toListLI)

  
  
  drawCellCommon _ _ n _ _ _ _ = 
       if n > 1
       then  fmap (Bf.second $ const $ ( (["cellBorder"] , ExtrudeLines) , gray 0.5))
               $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 1
       else []

data DefaultPT = DefaultPT { dptCursorAddress ::  Maybe Address
                                           }

addTag :: String -> ColorType -> ColorType
addTag s = first (first (addIfNotIn s))  
  
instance DrawingCtx GCContext ColorType GCData DefaultPT where    
  fromCtx _ = initGCContext
  drawGenericTerm _ (env , ctx) gcc _ (VarIndex vi) = gcc Map.! vi   


  drawD _ _ = renderGCD 
  
    -- FromLI 1 (bool [ ([[0.3]] , nthColor 1)] [ ([[1 - 0.35]] , nthColor 2)] . fst . head . toListLI)
    -- FromLI 1 (bool [([[0.2],[0.23]] , ())] [] . fst . head . toListLI)

    -- FromLI 1 (bool [([[0.2]] , ()) , ([[0.3]] , ())]
    --                [([[0.6]] , ()) , ([[0.7]] , ())  ]
    --            . fst . head . toListLI)

  fillStyleProcess _ = mapStyle (addTag "filling") 

  cellStyleProcess d ee dctx n addr fcs ce drw =
    if (dptCursorAddress d == Just addr)
    then mapStyle (addTag "selected") drw
    else drw
  
  drawCellCommon _ _ n _ _ _ _ = 
       if n > 1
       then  fmap (Bf.second $ const $ ( (["cellBorder"] , ExtrudeLines) , gray 0.5))
               $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 1
       else []

  finalProcess _ = mapStyle
     (\((tags , em) , color)  ->
        if (elem "filling" tags)
        then ((tags , em) , lighter (0.6) color)
        else ((tags , em) , color)

       )

data ScaffoldPT = ScaffoldPT
  { sptDrawFillSkelet :: Bool
  , sptCursorAddress :: Maybe Address
  , sptScaffDim :: Int
  , sptMissingSubFaceCursor :: Maybe (Address , SubFace)
  }


instance Shadelike (([String] , ExtrudeMode) , Color) where
  toShade ((tags , _) , c) =
          let isCursor = elem "cursor" tags
              isSelected = elem "selected" tags
              shadeMode = case (isCursor , isSelected) of
                              (True , _) -> 1
                              (_ , True) -> 3
                              (_) -> 0
          in
          Shade { shadeColor = c 
                , shadeMode = shadeMode
                }  


instance DrawingCtx () (([String] , ExtrudeMode) , Color) Int ScaffoldPT where    
  fromCtx _ _ = ()
  drawGenericTerm _ (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  


  drawD _ _ k = FromLI k (const [])

  -- fillStyleProcess _ =
  --   map (\(s , a@((tags , em) , c) ) ->
  --          case em of
  --            ExtrudeLines -> (s , ((tags , em) , Rgba 0.9 0.9 0.9 1.0))
  --            _ -> (s , a) 
  --       )

  nodePainter spt ee dctx n addr () nm si center = Right $
          let cursor = if (sptCursorAddress spt == Just addr)
                       then fmap (Bf.second $ const $ (( ["cursor"] , Basic) , Rgba 0.0 1.0 0.0 0.3))
                            $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 2
                       else []
              sfCursor =
                   (sptMissingSubFaceCursor spt)
                   & maybe [] 
                     (\(addrM , sf) ->
                         if addrM == addr
                         then 
                               subFaceTrans sf
                              $ fmap (Bf.second $ const $ (( ["cursor"] , Basic) , Rgba 0.0 0.0 1.0 0.3))
                              $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel (subFaceDimEmb sf + 1) 2 
                         else []
                     )
                            
          in cursor ++ sfCursor
          
  drawCellCommon spt _ n addr _ _ x =
    let g = if ( sptDrawFillSkelet spt) then 0.85 else 0.0
    
    
        lines = if n > -1
                then  fmap (Bf.second $ const $ (( ["cellBorder"] , ExtrudeLines) , gray g))
                       $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 1
                else []

        cursor = 
          if (sptCursorAddress spt == Just addr && isLeft x )
          then fmap (Bf.second $ const $ (( ["cursor" , "hole"] , Basic) , Rgba 1.0 0.0 0.0 0.3))
                $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 2
          else []

    
    
    in case (n , (sptCursorAddress spt == Just addr)) of
         (1 , True) -> cursor
         _ -> cursor ++ lines
    




-- data CursorPT = CursorPT { cursorAddress :: Address }

-- instance DrawingCtx () (([String] , ExtrudeMode) , Color) Int CursorPT where    
--   fromCtx _ _ = ()
--   drawGenericTerm _ (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  


--   drawD _ _ k = FromLI k (const [])

--   -- fillStyleProcess _ =
--   --   map (\(s , a@((tags , em) , c) ) ->
--   --          case em of
--   --            ExtrudeLines -> (s , ((tags , em) , Rgba 0.9 0.9 0.9 1.0))
--   --            _ -> (s , a) 
--   --       )
    
--   drawCellCommon cpt _ n addr _ = 
--     let cAddr = cursorAddress cpt 
--     in
--        if cAddr == addr
--        then fmap (Bf.second $ const $ (( ["cursor"] , Basic) , Rgba 1.0 0.0 0.0 0.3))
--                $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCube n
--        else []


  
-- -- drawExpr = mkDrawExpr (forget Stripes1)
          
