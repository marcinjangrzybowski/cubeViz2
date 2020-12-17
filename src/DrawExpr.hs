{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

import Data.Foldable

import PiecesEval

import Reorientable

import DataExtra

import GenericCell

import Debug.Trace

defaultCompPar = 0.3

subFaceTrans :: SubFace -> Drawing a -> Drawing a
subFaceTrans sf@(SubFace n m) drw =
  case Map.toList m of
    [] -> mempty
    -- _ : [] -> emptyDrawing 
    (i , b)  : js ->
      let
          -- putting subface in one of the adjacent face to execute face transform
          augmentedWithMissingTail =
             foldl (flip (\(i , b) -> embed (i - 1) (const $ if b then 1.0 else 0.0)))
               drw js

          transformed = sMap (ambFnOnArr
             $ sideTransInv defaultCompPar STSubFace (Face n (i , b)))
             $ transposeDrw i augmentedWithMissingTail


      in transformed

hcompDrawings :: Drawing a
     -> Map.Map SubFace (Drawing a)
     -> Drawing a
hcompDrawings bot sides =
   concat
    (
      sMap (map $ centerTransInv defaultCompPar) bot :
      map snd (Map.toList
       $ Map.mapWithKey subFaceTrans sides)
     )


type CellPainter b =
       Int -> Address
         -> Maybe CellExpr -> Drawing b



sideTransform :: SideTransMode -> Face -> Drawing b ->  Drawing b
sideTransform stm fc =
               sMap (ambFnOnArr
             $ sideTransInv defaultCompPar stm fc)

sideTransformSF :: SideTransMode -> SubFace -> Drawing b ->  Drawing b
sideTransformSF stm sf =
   let (fc@(Face n (i , b)) , sf') = unconsSF sf
       augmentWithMissingTail = embedSF sf'
       sideTrans = sideTransform stm fc

   in  sideTrans
       . transposeDrw i
       . augmentWithMissingTail


cellTransformPA :: AddressPart -> Drawing b ->  Drawing b
cellTransformPA (AOnCylinder sf) = sideTransformSF STSubFace sf
cellTransformPA (AOnBottom sf) =
   sMap (map $ centerTransInv defaultCompPar) . embedSF sf

cellTransform :: Address -> Drawing b -> Drawing b
cellTransform (Address sf addr) =
   embedSF sf . flip (foldl (flip cellTransformPA)) addr


collectAndOrient :: ClCub (Drawing b) -> Drawing b
collectAndOrient = fold . cubMapWAddr cellTransform

collectAndOrientOnlyInterior :: ClCub (Drawing b) -> Drawing b
collectAndOrientOnlyInterior x =
     fold
   $ cubMapWAddr cellTransform (ClCub (fromLIppK f (clCub x)))

  where
    f sf y | isFullSF sf = y
           | otherwise = Cub (subFaceDimEmb sf) [] Nothing

fillCub ::  forall b. Extrudable b => (Drawing b -> Drawing b) -> ClCub (Drawing b) -> ClCub (Drawing b)
fillCub fillStyle x = foldFL (fmap fillCubAt  [1..(getDim x)]) x

  where
    fillCubAt :: Extrudable b => Int -> ClCub (Drawing b) -> ClCub (Drawing b)
    fillCubAt k = cubMapWithB funAtComp funAtCell
      where

        funAtCell :: Extrudable b =>  Int -> Address -> BdCub (Drawing b) -> Drawing b
                      -> Maybe CellExpr
                      -> Drawing b
        funAtCell _ _ _ x _ = x

        funAtComp :: Extrudable b => Int -> Address -> BdCub (Drawing b) -> Drawing b
                       -> Maybe Name
                       -> CylCub (Drawing b)
                       -> ClCub (Drawing b) -> Drawing b
        funAtComp n _ dBd dOld _ cy cen | n == k =
            let
                missing = Set.toList $ missingSubFaces (getDim (cylCub cy)) (keysSetMapFLI (cylCub cy))


                filledSF sf =
                  let d0 = collectAndOrientOnlyInterior
                         $ bdCubPickSF sf dBd
                      pmf = piramFn defaultCompPar
                      d1 = extrude (subFaceDimEmb sf + 1)
                            (pmf , const 1.0)
                            -- (pmf , \x -> interp (pmf x) 1.0 (fillFactor d))
                            -- (pmf , \x -> interp (pmf x) 1.0 (0.5))            
                            d0
                  in sideTransformSF STFill sf d1


                dNew = fillStyle $ concatMap filledSF missing
            in --trace (show (drawingDim dNew) ++ " " ++ show n ++ " " ++ show (drawingDim dOld))
                     (dOld ++ dNew)

        funAtComp n _ _ dOld _ cy cen  = dOld

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
  drawGenericTerm :: d -> a -> Piece -> VarIndex -> c

  --Drawing b

  -- "rendering" internal cell representation to drawing in pieces
  drawD :: d -> a -> c -> ZDrawing b

  drawCellCommon :: d -> Int -> Address -> a ->
                    Maybe CellExpr -> Drawing b
  drawCellCommon _ _ _ _ _ = []

  drawCellPiece :: d -> Int -> Address -> a -> PieceExpr -> (Piece -> Drawing b)
  drawCellPiece d n adr a (PieceExpr h t) =
     \pc -> appLI pc (remapTL (drawD d a) n t $ drawGenericTerm d a pc h)

  drawHole :: d -> Int -> Address -> a -> Drawing b
  drawHole _ _ _ _ = []

  cellStyleProcess :: d -> a -> Int -> Address
                              -> Maybe CellExpr -> Drawing b -> Drawing b
  cellStyleProcess _ _ _ _ _ = id

  nodeStyleProcess :: d -> a -> Int -> Address
                              -> Drawing b -> Drawing b
  nodeStyleProcess _ _ _ _ = id

  cellPainter :: d -> a -> CellPainter b
  cellPainter d dctx n adr x =
     let zz =
           case x of
             Just ce -> evalLI (fmap (FromLI n . drawCellPiece d  n adr dctx) (piecesEval n ce))
             Nothing -> drawHole d n adr dctx
     in
          drawCellCommon d n adr dctx x
            ++
          cellStyleProcess d dctx n adr x zz


  nodePainterCommon :: d -> a ->
      (Int -> Address -> Maybe Name -> CylCub ()
                          -> ClCub ()
                          -> Drawing b)
  nodePainterCommon d dctx n addr nm si center = []


  drawAllCells :: d  -> (Env , Context) -> Expr -> ClCub (Drawing b)
  drawAllCells d envCtx expr =
      let  dctx = fromCtx d envCtx
           cub = (toClCub envCtx expr :: ClCub ())
      in  cubMap (\i addr _ -> nodePainterCommon d dctx i addr)
                  (\i addr _ -> cellPainter d dctx i addr) cub




  mkDrawExpr :: d  -> (Env , Context) -> Expr -> Drawing b
  mkDrawExpr d envCtx expr =
    let  dctx = fromCtx d envCtx
    in
        finalProcess d
      $ collectAndOrient
      $ cubMap (\n addr b _ _ _ -> nodeStyleProcess d dctx n addr b) (\_ _ x _ -> x)
      $ fillCub (fillStyleProcess d)
      $ drawAllCells d envCtx expr


  fillStyleProcess :: d -> Drawing b -> Drawing b
  fillStyleProcess d = id


  finalProcess :: d -> Drawing b -> Drawing b
  finalProcess d = id




data DefaultPT = DefaultPT { dptCursorAddress ::  Maybe Address
                           , dptShowFill      :: Bool
                           , dptFillFactor       :: Float
                           }

addTag :: String -> ColorType -> ColorType
addTag s = first (first (addIfNotIn s))

instance DrawingCtx GCContext ColorType GCData DefaultPT where
  fromCtx _ = initGCContext
  drawGenericTerm _ gcc _ (VarIndex vi) = gcc Map.! vi


  drawD _ _ = renderGCD

  drawHole spt k addr ee =
      if k <= 3
      then let holeS = (( ["hole" , "hollowBox"] , Basic) , gray 0.0)
           in  scaleCell 0.8 $ Bf.second (const holeS) <$> unitHyCubeSkel k 1
      else []

  fillStyleProcess d drw =
    if dptShowFill d
    then mapStyle (addTag "filling") drw
    else []

  nodeStyleProcess spt ee n addr drw =
    case dptCursorAddress spt of
      Just ca ->
        if isJust (mbSubAddress ca addr)
        then mapStyle ((addTag "selected")) drw -- zaznaczone
        else mapStyle (addTag "notselected") drw
      Nothing -> drw

  cellStyleProcess spt ee n addr _ drw =
    case dptCursorAddress spt of
      Just ca ->
        if isJust (mbSubAddress ca addr)
        then mapStyle ((addTag "selected")) drw -- zaznaczone
        else mapStyle (addTag "notselected") drw
      Nothing -> drw

    -- if (dptCursorAddress d == Just addr)
    -- then mapStyle (addTag "selected") drw
    -- else drw

  drawCellCommon spt k addr _ _ = []
     -- let partOfSelectedCellBndr =
     --          maybe False (not . isInternalAddress)
     --        $ flip mbSubAddress addr =<< dptCursorAddress spt
     --     scaffS = if partOfSelectedCellBndr
     --              then (( ["cellBorder"] , ExtrudeLines) , Rgba 1.0 0.0 0.0 1.0)
     --              else (( ["cellBorder"] , ExtrudeLines) , gray 0.9)
     --     scaff = if k <= 1
     --         then Bf.second (const scaffS) <$> unitHyCube k
     --         else []

     -- in if partOfSelectedCellBndr
     --    then scaff
     --    else []

  finalProcess _ =
    let fillP = mapStyle
                 (\((tags , em) , color)  ->

                    if "filling" `elem` tags
                    then ((tags , em) , lighter 0.6 color)
                    else ((tags , em) , color)
                   )
        selectP = mapStyle
                 (\((tags , em) , color)  ->

                    if "notselected" `elem` tags
                    then ((tags , em) , lighter 0.2 $ desat 0.4 color)
                    else ((tags , em) , color)
                   )
        removeFillHoles = 
           filter ((\((tags , em) , color) -> not ("filling" `elem` tags && "hole" `elem` tags)) . snd) 
                 
    in
       removeFillHoles
     . fillP
     . selectP

data ScaffoldPT = ScaffoldPT
  { sptDrawFillSkelet :: Bool
  , sptCursorAddress :: Maybe Address
  , sptScaffDim :: Int
  , sptMissingSubFaceCursor :: Maybe (Address , SubFace)
  }


instance Shadelike ColorType where
  toShade ((tags , _) , c) =
          let isCursor = elem "cursor" tags
              hollowBox = elem "hollowBox" tags
              animStripes = elem "animated-stripes" tags
              shadeMode = case (isCursor , animStripes , hollowBox) of
                              (True , _ , _) -> 1
                              (_ , True , _) -> 3
                              (_ , _ , True) -> 4
                              _ -> 0
          in
          Shade { shadeColor = c
                , shadeMode = shadeMode
                }



instance DrawingCtx (Env , Context) ColorType Int ScaffoldPT where
  fromCtx _ = id

  drawGenericTerm _ (env , ctx) _ vI = getCTyDim env ctx (getVarType ctx vI)

  drawD _ _ k = FromLI k (const [])

  drawCellCommon spt k addr _ _ =
     let scaffS = if isJust $ flip mbSubAddress addr =<< sptCursorAddress spt
                  then (( ["cellBorder"] , ExtrudeLines) , Rgba 1.0 0.0 0.0 1.0)
                  else (( ["cellBorder"] , ExtrudeLines) , gray 0.9)
         scaff = if k <= 1
             then Bf.second (const scaffS) <$> unitHyCube k
             else []

     in scaff

  fillStyleProcess _ _ = []

data CursorPT = CursorPT
  { cptCursorAddress :: Maybe Address
  }



instance DrawingCtx (Env , Context) ColorType Int CursorPT where
  fromCtx _ = id

  drawGenericTerm _ (env , ctx) _ vI = getCTyDim env ctx (getVarType ctx vI)

  drawD _ _ k = FromLI k (const [])

  drawCellCommon cpt k addr _ _ =
     let partOfSelectedCellBndr =
              maybe False (not . isInternalAddress)
            $ flip mbSubAddress addr =<< cptCursorAddress cpt
         scaffS = if partOfSelectedCellBndr
                  then (( ["cellBorder" , "cursor"] , ExtrudeLines) , Rgba 1.0 0.0 0.0 1.0)
                  else (( ["cellBorder"] , ExtrudeLines) , gray 0.9)
         scaff = if k <= 1
             then Bf.second (const scaffS) <$> unitHyCube k
             else []

     in if partOfSelectedCellBndr
        then scaff
        else []

  fillStyleProcess _ _ = []


-- instance DrawingCtx () (([String] , ExtrudeMode) , Color) Int ScaffoldPT where    
--   fromCtx _ _ = ()
--   drawGenericTerm _ (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  


--   drawD _ _ k = FromLI k (const [])

--   -- fillStyleProcess _ =
--   --   map (\(s , a@((tags , em) , c) ) ->
--   --          case em of
--   --            ExtrudeLines -> (s , ((tags , em) , Rgba 0.9 0.9 0.9 1.0))
--   --            _ -> (s , a) 
--   --       )

--   nodePainter spt ee dctx n addr () nm si center = Right $
--           let cursor = if (sptCursorAddress spt == Just addr)
--                        then fmap (Bf.second $ const $ (( ["cursor"] , Basic) , Rgba 0.0 1.0 0.0 0.3))
--                             $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 2
--                        else []
--               sfCursor =
--                    (sptMissingSubFaceCursor spt)
--                    & maybe [] 
--                      (\(addrM , sf) ->
--                          if addrM == addr
--                          then 
--                                subFaceTrans sf
--                               $ fmap (Bf.second $ const $ (( ["cursor"] , Basic) , Rgba 0.0 0.0 1.0 0.3))
--                               $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel (subFaceDimEmb sf + 1) 2 
--                          else []
--                      )

--           in cursor ++ sfCursor

--   drawCellCommon spt _ n addr _ _ x =
--     let g = if ( sptDrawFillSkelet spt) then 0.85 else 0.0


--         lines = if n > -1
--                 then  fmap (Bf.second $ const $ (( ["cellBorder"] , ExtrudeLines) , gray g))
--                        $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 1
--                 else []

--         cursor = 
--           if (sptCursorAddress spt == Just addr)
--           then fmap (Bf.second $ const $
--                        (( ["cursor" , "hole" , either (const "hole") (const "cell") x] , Basic)
--                           , Rgba 1.0 0.0 0.0 0.3))
--                 $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 2
--           else []



--     in case (n , (sptCursorAddress spt == Just addr)) of
--          (1 , True) -> cursor
--          _ -> cursor ++ lines





-- -- data CursorPT = CursorPT { cursorAddress :: Address }

-- -- instance DrawingCtx () (([String] , ExtrudeMode) , Color) Int CursorPT where    
-- --   fromCtx _ _ = ()
-- --   drawGenericTerm _ (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  


-- --   drawD _ _ k = FromLI k (const [])

-- --   -- fillStyleProcess _ =
-- --   --   map (\(s , a@((tags , em) , c) ) ->
-- --   --          case em of
-- --   --            ExtrudeLines -> (s , ((tags , em) , Rgba 0.9 0.9 0.9 1.0))
-- --   --            _ -> (s , a) 
-- --   --       )

-- --   drawCellCommon cpt _ n addr _ = 
-- --     let cAddr = cursorAddress cpt 
-- --     in
-- --        if cAddr == addr
-- --        then fmap (Bf.second $ const $ (( ["cursor"] , Basic) , Rgba 1.0 0.0 0.0 0.3))
-- --                $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCube n
-- --        else []



-- -- -- drawExpr = mkDrawExpr (forget Stripes1)

