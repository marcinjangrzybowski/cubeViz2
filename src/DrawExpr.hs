{-# LANGUAGE LambdaCase #-}
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
import Control.Monad
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
import ConcreteCell

import Debug.Trace

import Data.Bits

defaultCompPar = 0.3

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

fillCub ::  forall b. Extrudable b => (Drawing b -> Drawing b) -> (Drawing b -> Drawing b)
                     -> ClCub (Drawing b) -> ClCub (Drawing b)
fillCub fillStyle bleedStyle x = foldFL (fmap fillCubAt  [1..(getDim x)]) x

  where
    fillCubAt :: Extrudable b => Int -> ClCub (Drawing b) -> ClCub (Drawing b)
    fillCubAt k = cubMapWithB funAtComp funAtCell
      where


        
        funAtCell :: Extrudable b =>  Int -> Address -> BdCub (Drawing b) -> Drawing b
                      -> Maybe CellExpr
                      -> Drawing b
        funAtCell n _ bd x y = if n > 2 then x else
          case y of
            Just _ -> x
            Nothing ->
               let l = map (first bd2SubFace) $ filter (not . isHole . snd) $ Map.toList (toMapFLI $ bdCub bd)
                   sfToBleed = filter (not . (isCoveredIn (Set.fromList $ (fst <$> l))) . fst ) $ l
                   hBleed (sf , d) =
                     let crnr = map (second fromJust) $ filter (isJust . snd) $ zip [0..] $ toListLI sf
                         d0 = collectAndOrientOnlyInterior
                              $ bdCubPickSF (sf2BdSubFace sf) bd
                     in foldl'
                           (flip $ \(k , b) ->
                              let bleedPar = 0.03
                                  (s , e) =
                                     if b
                                     then (const 1.0 , const $ 1.0 - bleedPar)
                                     else (const 0.0 , const bleedPar)
                              in extrude k (s,e)
                           ) d0 crnr 
                   hsBleed = concatMap hBleed sfToBleed
               in bleedStyle hsBleed ++ x 
               

        funAtComp :: Extrudable b => Int -> Address -> BdCub (Drawing b) -> Drawing b
                       -> Maybe Name
                       -> CylCub (Drawing b)
                       -> ClCub (Drawing b) -> Drawing b
        funAtComp n _ dBd dOld _ cy cen | n == k =
            let
                missing = Set.toList $ missingSubFaces (getDim (cylCub cy)) (keysSetMapFLI (cylCub cy))


                filledSF sf =
                  let d0 = collectAndOrientOnlyInterior
                         $ bdCubPickSF (sf2BdSubFace sf) dBd
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
  drawTermFromContext :: d -> a -> Piece -> VarIndex -> c

  --Drawing b

  -- "rendering" internal cell representation to drawing in pieces
  drawD :: d -> a -> c  -> ZDrawing b

  drawCellCommon :: d -> Int -> Address -> a ->
                    Maybe CellExpr -> Drawing b
  drawCellCommon _ _ _ _ _ = []

  drawCellPiece :: d -> Int -> Address -> a -> PieceExpr -> (Piece -> Drawing b)
  drawCellPiece d n adr a (PieceExpr h t) =
     \pc -> appLI pc (remapTL (drawD d a) n t $ drawTermFromContext d a pc h)

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


  drawAllCells :: d  -> (Env , Context) -> ClCub () -> ClCub (Drawing b)
  drawAllCells d envCtx cub =
      let  dctx = fromCtx d envCtx
      in  cubMap (\i addr _ -> nodePainterCommon d dctx i addr)
                  (\i addr _ -> cellPainter d dctx i addr) cub

  mkDrawCub :: d  -> (Env , Context) -> ClCub () -> Drawing b
  mkDrawCub d envCtx clcub =
    let  dctx = fromCtx d envCtx
    in
        finalProcess d
      $ collectAndOrient
      $ cubMap (\n addr b _ _ _ -> nodeStyleProcess d dctx n addr b) (\_ _ x _ -> x)
      $ fillCub (fillStyleProcess d) (holeBleedStyleProcess d)
      $ drawAllCells d envCtx clcub


  fillStyleProcess :: d -> Drawing b -> Drawing b
  fillStyleProcess d = id

  holeBleedStyleProcess :: d -> Drawing b -> Drawing b
  holeBleedStyleProcess d _ = []


  finalProcess :: d -> Drawing b -> Drawing b
  finalProcess d = id




data DefaultPT = DefaultPT { dptCursorAddress ::  Maybe Address
                           , dptShowFill      :: Bool
                           , dptFillFactor       :: Float
                           , dptTags          :: Set.Set String
                           , dptShowLowDim    :: Int
                           }

addTag :: String -> ColorType -> ColorType
addTag s ((x , y , z) , w) = ((addIfNotIn s x , y , z) , w)

instance DrawingCtx GCContext ColorType (Either GCData ConcreteCellData) DefaultPT where
  fromCtx _ = initGCContext
  drawTermFromContext _ gcc _ (VarIndex vi) =
    let g@(GCData s _) = gcc Map.! vi
    in case renderNamedCell s of
         Just x -> Right (ConcreteCellData x)
         Nothing -> Left g

  drawD _ _ = either renderGCD ccdDrawing

  drawHole spt k addr ee = undefined
      -- if k <= 3
      -- then let holeS = (( ["hole" , "hollowBox"] , Basic) , gray 0.0)
      --      in  scaleCell 0.8 $ Bf.second (const holeS) <$> unitHyCubeSkel k 1
      -- else []

  fillStyleProcess d drw =
    if dptShowFill d
    then filter (\(_ , ((tags , em , _) , color)) ->
               not ("ms0" `elem` tags) &&
               not ("zeroCellCPI" `elem` tags)
           ) $ (mapStyle (addTag "filling") drw)
    else []

  holeBleedStyleProcess d drw =
    mapStyle (addTag "holeBleed") drw

  nodeStyleProcess spt ee n addr drw =
    case dptCursorAddress spt of
      Just ca ->
        if isJust (mbSubAddress ca addr)
        then mapStyle ((addTag "selected")) drw -- zaznaczone
        else mapStyle (addTag "notselected") drw
      Nothing -> drw

  cellStyleProcess spt ee n addr _ drw' =
    let drw =
           filter
            ((\((tags , em , _) , color) -> not ("ms0" `elem` tags && n == 2)) . snd)
             drw'
           -- if n == 2 then [] else drw'
    in case dptCursorAddress spt of
           Just ca ->
             if isJust (mbSubAddress ca addr)
             then mapStyle ((addTag "selected")) drw -- zaznaczone
             else mapStyle (addTag "notselected") drw
           Nothing -> drw

    -- if (dptCursorAddress d == Just addr)
    -- then mapStyle (addTag "selected") drw
    -- else drw

  drawCellCommon spt k addr _ _ =
     if k == 0 then
     [
       ( [[]] , ((["zeroCellCPI","m0"] , Midpoints , (0,0)) , gray 0))
     ] else []
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

  finalProcess settings =
    let fillP = mapStyle
                 (\((tags , em , sqPt) , color)  ->
                    ((tags, em, sqPt), color)
                    -- if "filling" `elem` tags
                    -- then ((tags , em) , lighter 0.6 color)
                    -- else ((tags , em) , color)
                   )
        selectP = mapStyle
                 (\((tags , em, sqPt) , color)  ->

                    if "notselected" `elem` tags
                    then ((tags , em, sqPt) , lighter 0.2 $ desat 0.4 color)
                    else ((tags , em, sqPt) , color)
                   )
        removeFillHoles = 
           filter ((\((tags , em, sqPt) , color) -> not ("filling" `elem` tags && "hole" `elem` tags)) . snd)

        removePoints = filter
           (\(x , ((tags , em, sqPt) , color)) ->
              length x > 1 || ("zeroCellCPI" `elem` tags)
           )
                 
        toggleStrands =
          filter (\(x, ((tags , em, sqPt) , color)) -> (not ("m2" `elem` tags) || not ("gcd" `elem` tags)
                                                  || not (Set.null (Set.intersection (Set.fromList tags) (dptTags settings)))))

        toggleLowDim =
          filter (\(x, ((tags , em, sqPt) , color)) -> not ("m0" `elem` tags))
    in
       toggleStrands
     . toggleLowDim
     . removePoints
     . removeFillHoles
     . fillP
     . selectP



instance Shadelike ColorType where
  toShade ((tags , _ , sqPt) , c) =
          let isCursor = elem "cursor" tags
              isCursorSec = elem "cursorSec" tags
              hollowBox = elem "hollowBox" tags
              animStripes = elem "animated-stripes" tags
              moveFibres = elem "moveFibres" tags
              shadeMode = case (isCursor , isCursorSec , animStripes , hollowBox,moveFibres) of
                              (True , _ , _ , _ , _) -> 6
                              (_ , True , _ , _ , _) -> 7
                              (_ , _ , True , _ , _) -> 3
                              (_ , _ , _ , True , _) -> 4
                              (_ , _ , _ , _ , True) -> 10
                              _ -> 0
              vfg = foldl setBit 1 $ catMaybes
                     $ map (\case
                               'v':'f':'g':'T' :i -> Just $ read i
                               _ -> Nothing
                           )
                       tags

              vfg' = foldl clearBit vfg $ catMaybes
                     $ map (\case
                               'v':'f':'g':'F':i -> Just $ read i
                               _ -> Nothing
                           )
                       tags
          in
          Shade { shadeColor = c
                , shadeMode = shadeMode
                , shadeModeVFG = vfg'
                , sqPt = sqPt     
                }
    
    -- [
    --   ( [ replicate k 0.5 ] , ((addr , Midpoints) , gray 0))
    -- ]
