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

import Safe (readMay)

defaultCompPar = 0.3

defaultCongPar = 0.1


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
cellTransformPA (AArg 1 sf) = 
   sMap (map $ centerTransInv defaultCongPar) . embedSF sf
cellTransformPA (AArg _ sf) = \x -> x 

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
    fillCubAt k = cubMapWithB funAtComp funAtCell (\_ _ _ _ _ -> [])
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
  cellStyleProcess _ _ _ addr _ = id

  nodeStyleProcess :: d -> a -> Int -> Address
                              -> Drawing b -> Drawing b
  nodeStyleProcess _ _ _ _ = id

  cellPainter :: d -> a -> CellPainter b
  cellPainter d dctx n (Address _ (AArg 0 _ : _)) x = []
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
                  (\i addr _ -> cellPainter d dctx i addr)
                  (\addr _ -> [])
           cub

  mkDrawCub :: d  -> (Env , Context) -> ClCub () -> Drawing b
  mkDrawCub d envCtx clcub =
    let  dctx = fromCtx d envCtx
    in
        finalProcess d
      $ collectAndOrient
      $ cubMap (\n addr b _ _ _ -> nodeStyleProcess d dctx n addr b) (\_ _ x _ -> x)
          (\_ _ -> [])
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
addTag s = first (first (addIfNotIn s))

instance DrawingCtx GCContext ColorType (Either GCData ConcreteCellData) DefaultPT where
  fromCtx _ = initGCContext
  drawTermFromContext _ gcc _ (VarIndex vi) =
    let g@(GCData s _) = gcc Map.! vi
    in case renderNamedCell s of
         Just x -> Right (ConcreteCellData x)
         Nothing -> Left g

  drawD _ _ = either (renderGCD) ccdDrawing

  drawHole spt k addr ee =
      if k <= 3
      then let holeS = (( ["hole" , "hollowBox"] , Basic) , gray 0.0)
           in  scaleCell 0.8 $ Bf.second (const holeS) <$> unitHyCubeSkel k 1
      else []

  fillStyleProcess d drw =
    if dptShowFill d
    then filter (\(_ , ((tags , em) , color)) ->
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
    let addrStr = show addr
        drw =
           (mapStyle (addTag ("addrTag-" ++ addrStr)))
           (filter
            ((\((tags , em) , color) -> not ("ms0" `elem` tags && n == 2)) . snd)
             drw')
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
     case (k , k==2||k==3) of
       (0 , _) -> [
            ( [[]] , ((["zeroCellCPI","m0"] , Midpoints) , gray 0))
             ]
       (_ , True) -> Bf.second (const ((["cellSpace","animated-stripes","addrTag-" ++ (show addr)] , Basic) , gray 0.5)) <$> 
            (unitHyCubeSkel k 2)
             
       _ -> [];
     -- if k == 0 then
     -- [
     --   ( [[]] , ((["zeroCellCPI","m0"] , Midpoints) , gray 0))
     -- ] else []
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
                 (\((tags , em) , color)  ->
                    ((tags, em), color)
                    -- if "filling" `elem` tags
                    -- then ((tags , em) , lighter 0.6 color)
                    -- else ((tags , em) , color)
                   )
        selectP = mapStyle
                 (\((tags , em) , color)  ->

                    if "notselected" `elem` tags
                    then ((tags , em) , lighter 0.2 $ desat 0.4 color)
                    else ((tags , em) , color)
                   )
        removeFillHoles = 
           filter ((\((tags , em) , color) -> not ("filling" `elem` tags && "hole" `elem` tags)) . snd)

        removePoints = filter
           (\(x , ((tags , em) , color)) ->
              length x > 1 || ("zeroCellCPI" `elem` tags)
           )
                 
        toggleStrands =
          filter (\(x, ((tags , em) , color)) -> (not ("m2" `elem` tags) || not ("gcd" `elem` tags)
                                                  || not (Set.null (Set.intersection (Set.fromList tags) (dptTags settings)))))

        toggleLowDim =
          filter (\(x, ((tags , em) , color)) -> not ("m0" `elem` tags))
    in
       toggleStrands
     . toggleLowDim
     . removePoints
     . removeFillHoles
     . fillP
     . selectP


data DefaultPTA = DefaultPTA { dptCursorAddressA ::  Maybe Address
                           , dptShowFillA      :: Bool
                           , dptFillFactorA       :: Float
                           , dptTagsA          :: Set.Set String
                           , dptShowLowDimA    :: Int
                           }




instance DrawingCtx GCContext ColorType (Either GCData ConcreteCellData) DefaultPTA where
  fromCtx _ = initGCContextA

  drawTermFromContext _ gcc _ (VarIndex vi) = 
    let g@(GCData s _) =
            fromMaybe (error $ "lookupFail! " ++ show gcc ++ " " ++ show vi)
             (Map.lookup vi gcc)
    in  Left g

  drawTermFromContext _ gcc _ (SomeDef nm) = 
      case renderNamedCell nm of
         Just x -> Right (ConcreteCellData x)
         Nothing -> error ("definition not recognised:" ++ nm)



  
  -- drawTermFromContext _ gcc _ (VarIndex vi) = 
  --   let g@(GCData s _) =
  --           fromMaybe (error $ "lookupFail! " ++ show gcc ++ " " ++ show vi)
  --            (Map.lookup vi gcc)

  drawD _ _ = either (renderGCD) ccdDrawing

  drawHole spt k addr ee =
      if k <= 3
      then let holeS = (( ["hole" , "hollowBox"] , Basic) , gray 0.0)
           in  scaleCell 0.8 $ Bf.second (const holeS) <$> unitHyCubeSkel k 1
      else []

  fillStyleProcess d drw =
    if dptShowFillA d
    then filter (\(_ , ((tags , em) , color)) ->
               not ("ms0" `elem` tags) &&
               not ("zeroCellCPI" `elem` tags)
           ) $ (mapStyle (addTag "filling") drw)
    else []

  holeBleedStyleProcess d drw =
    mapStyle (addTag "holeBleed") drw

  nodeStyleProcess spt ee n addr drw =
    case dptCursorAddressA spt of
      Just ca ->
        if isJust (mbSubAddress ca addr)
        then mapStyle ((addTag "selected")) drw -- zaznaczone
        else mapStyle (addTag "notselected") drw
      Nothing -> drw

  cellStyleProcess spt ee n addr _ drw' =
    let addrStr = show addr
        drw =
           (mapStyle (addTag ("addrTag-" ++ addrStr)))
           (filter
            ((\((tags , em) , color) -> not ("ms0" `elem` tags && n == 2)) . snd)
             drw')
           -- if n == 2 then [] else drw'
    in case dptCursorAddressA spt of
           Just ca ->
             if isJust (mbSubAddress ca addr)
             then mapStyle ((addTag "selected")) drw -- zaznaczone
             else mapStyle (addTag "notselected") drw
           Nothing -> drw

    -- if (dptCursorAddress d == Just addr)
    -- then mapStyle (addTag "selected") drw
    -- else drw

  drawCellCommon spt k addr _ _ =
     case (k , k==2||k==3) of
       (0 , _) -> [
            ( [[]] , ((["zeroCellCPI","m0"] , Midpoints) , gray 0))
             ]
       (_ , True) -> Bf.second (const ((["cellSpace","animated-stripes","addrTag-" ++ (show addr)] , Basic) , gray 0.5)) <$> 
            (unitHyCubeSkel k 2)
             
       _ -> [];
     -- if k == 0 then
     -- [
     --   ( [[]] , ((["zeroCellCPI","m0"] , Midpoints) , gray 0))
     -- ] else []
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
                 (\((tags , em) , color)  ->
                    ((tags, em), color)
                    -- if "filling" `elem` tags
                    -- then ((tags , em) , lighter 0.6 color)
                    -- else ((tags , em) , color)
                   )
        selectP = mapStyle
                 (\((tags , em) , color)  ->

                    if "notselected" `elem` tags
                    then ((tags , em) , lighter 0.2 $ desat 0.4 color)
                    else ((tags , em) , color)
                   )
        removeFillHoles = 
           filter ((\((tags , em) , color) -> not ("filling" `elem` tags && "hole" `elem` tags)) . snd)

        removePoints = filter
           (\(x , ((tags , em) , color)) ->
              length x > 1 || ("zeroCellCPI" `elem` tags)
           )
                 
        toggleStrands =
          filter (\(x, ((tags , em) , color)) -> (not ("m2" `elem` tags) || not ("gcd" `elem` tags)
                                                  || not (Set.null (Set.intersection (Set.fromList tags) (dptTagsA settings)))))

        toggleLowDim =
          filter (\(x, ((tags , em) , color)) -> not ("m0" `elem` tags))
    in
       toggleStrands
     . toggleLowDim
     . removePoints
     . removeFillHoles
     . fillP
     . selectP


extractAddrFromTags :: [String] -> Maybe Address
extractAddrFromTags [] = Nothing
extractAddrFromTags (x : xs) =
   case readMay (drop 8 x) of
     Nothing -> extractAddrFromTags xs
     Just y -> Just y


instance Shadelike ColorType where
  toShade ((tags , _) , c) =
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
                , shadeMbAddress = extractAddrFromTags (reverse tags)      
                }

data ScaffoldPT = ScaffoldPT
  { sptDrawFillSkelet :: Bool
  , sptCursorAddress :: Maybe Address
  , sptScaffDim :: Int
  , sptMissingSubFaceCursor :: Maybe (Address , SubFace)
  }


instance DrawingCtx (Env , Context) ColorType Int ScaffoldPT where
  fromCtx _ = id

  drawTermFromContext _ (env , ctx) _ vI = getCTyDim env ctx (getVarType ctx vI)

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


data ConstraintsViewPT a = ConstraintsViewPT
  { cvptDrawFillSkelet :: Bool
  , cvptCursorAddress :: Maybe Address
  , cvptScaffDim :: Int
  , cvptCub :: ClCub (Maybe a)
  }


instance DrawingCtx (Env , Context) ColorType Int (ConstraintsViewPT a) where
  fromCtx _ = id

  drawTermFromContext _ (env , ctx) _ vI = getCTyDim env ctx (getVarType ctx vI)

  drawD _ _ k = FromLI k (const [])

  drawCellCommon spt k addr _ _ =
     let cdata = clCubPickData addr $ cvptCub spt

         scaff =
           case (fromJust cdata) of
             Nothing -> let s = (( ["cellBorder"] , ExtrudeLines) , gray 0.9)
                        in if k <= -1
                           then Bf.second (const s) <$> unitHyCube k
                           else []
             Just _ ->
                 let s = (( ["cellBorder"] , ExtrudeLines) , Rgba 1.0 0.0 0.0 1.0)
                 in Bf.second (const s) <$> unitHyCubeSkel k 2

     in scaff



  nodePainterCommon spt _ k addr _ _ _ = 
     let cdata = clCubPickData addr $ cvptCub spt

         scaff =
           case (fromJust cdata) of
             Nothing -> let s = (( ["cellBorder"] , ExtrudeLines) , gray 0.9)
                        in if k <= -1
                           then Bf.second (const s) <$> unitHyCube k
                           else []
             Just _ ->
                 let s = (( ["cellBorder"] , ExtrudeLines) , Rgba 1.0 0.0 0.0 1.0)
                 in Bf.second (const s) <$> unitHyCubeSkel k 2

     in scaff

  fillStyleProcess _ _ = []

data CursorPT = CursorPT
  { cptCursorAddress :: Maybe (Address , CAddress)
  , cptSecCursorAddress :: Maybe Address
  , cptSelectedAddressCorners :: Set.Set Address
  }


cursorDrw :: Bool -> CursorPT -> Int -> Address -> Drawing  ColorType
cursorDrw node1Fix cpt k addr =
     let partOfSelectedCellBndrF addr' = 
           if node1Fix then (addr == addr') else
              maybe False (not . isInternalAddress)
            $ mbSubAddress addr' addr

         partOfSelectedCellBndr =
           (maybe False $ any partOfSelectedCellBndrF)
--           -- $ fmap (Set.singleton . fst)
           $ fmap (cAddress . snd) 
           $ cptCursorAddress cpt

         partOfSecSelectedCellBndr =
           if node1Fix then (Just addr == cptSecCursorAddress cpt) else
              maybe False (not . isInternalAddress)
            $ flip mbSubAddress addr =<< cptSecCursorAddress cpt

         faceSelScaffS =
             (( ["faceSelectorScaff" , "vfgF0","vfgT1"] , ExtrudeLines) , gray 1.2)

         isSelectedAddressCorner =
            (Set.member addr $ cptSelectedAddressCorners cpt)
              
            
              
         scaffS =
            case (partOfSelectedCellBndr , partOfSecSelectedCellBndr , isSelectedAddressCorner) of
               (_ , _,True) -> (( ["cellBorder" , "cursor"] , ExtrudeLines) , Rgba 1.0 1.0 0.0 1.0)
               (True , _,_) -> (( ["cellBorder" , "cursor"] , ExtrudeLines) , Rgba 0.7 0.7 0.7 1.0)
               (_ , True,_) -> (( ["cellBorder" , "cursorSec"] , ExtrudeLines) , Rgba 1.0 0.0 0.0 1.0)
               (_ , _,_) -> (( ["cellBorder"] , ExtrudeLines) , gray 0.9)
         scaff = if k <= 1 
             then ((Bf.second (const scaffS) <$> unitHyCube k)
                    ++ (Bf.second (const faceSelScaffS) <$> unitHyCube k))
             else []

     in if partOfSelectedCellBndr || partOfSecSelectedCellBndr 
        then scaff
        else []


faceSelectorDrw :: Int -> Maybe CAddress -> Address -> Drawing ColorType
faceSelectorDrw k = \case
  Nothing -> \_ -> []
  Just cAddr -> \addr ->
    let z = nub $ catMaybes $ (join . (fmap toFace) . (flip mbSubFaceAddr addr)) <$> (Set.toList (cAddress cAddr))
    in Bf.second (const (( ["faceSelector","vfgF0"]
                             ++ [ "vfgT" ++ (show $ ((unemerate f) + 2) ) | f <- z]
                         , ExtrudeLines) , Rgba 1.0 0.0 0.0 1.0)) <$> unitHyCube k

instance DrawingCtx (Env , Context) ColorType Int CursorPT where
  fromCtx _ = id

  drawTermFromContext _ (env , ctx) _ vI = getCTyDim env ctx (getVarType ctx vI)

  drawD _ _ k = FromLI k (const [])

  nodePainterCommon cpt _ k addr _ _ _ =
    faceSelectorDrw k (snd <$> cptCursorAddress cpt) addr ++
    (if (k==1) then
     cursorDrw True cpt k addr
     else []) 
    
  drawCellCommon cpt k addr _ _ =
    faceSelectorDrw k (snd <$> cptCursorAddress cpt) addr
    ++ cursorDrw False cpt k addr 
  fillStyleProcess _ _ = []

  finalProcess _ =
     fattenOn
      ( elem "cursor" . fst . fst )
      0.02
      id
      


data ClickPoints = ClickPoints
  { 
  }

instance DrawingCtx (Env , Context) (ColorType2 Address) Int ClickPoints where
  fromCtx _ = id

  drawTermFromContext _ (env , ctx) _ vI = getCTyDim env ctx (getVarType ctx vI)

  drawD _ _ k = FromLI k (const [])

  nodePainterCommon cpt _ k addr _ _ _ = []
    
  drawCellCommon cpt k addr _ _ =
     [
       ( [ replicate k 0.5 ] , ((addr , Midpoints) , gray 0))
     ]
  fillStyleProcess _ _ = []


data FaceHandles = FaceHandles
  { 
  }

faceHandleDepth = 0.1
faceHandleDist = 0.3
faceHandleSize = 0.3

instance DrawingCtx (Env , Context) (ColorType2 Address) Int FaceHandles where
  fromCtx _ = id

  drawTermFromContext _ (env , ctx) _ vI = getCTyDim env ctx (getVarType ctx vI)

  drawD _ _ k = FromLI k (const [])

  nodePainterCommon cpt _ k addr _ _ _ = []
    
  drawCellCommon cpt k addr _ _ =
     [
     ]
  fillStyleProcess _ _ = []

  drawHole _ k addr _ = concat
    [

      translate [ if j==i
                  then (faceHandleDist * (if s then (1.0) else (-1.0)))
                  else  0.0
                | j <- [0..(k - 1)] ] $
      scaleNonUniformOrigin
                [ if j==i
                  then faceHandleDepth
                  else faceHandleSize
                | j <- [0..(k - 1)] ] (replicate k 0.5) $
      fmap (Bf.second (const ((addr , Basic) , gray 0)))
        (unitHyCube k)

    | i <- [0..(k - 1)]
    , s <- [False , True]
    ]
    
    -- [
    --   ( [ replicate k 0.5 ] , ((addr , Midpoints) , gray 0))
    -- ]

-- instance DrawingCtx () (([String] , ExtrudeMode) , Color) Int ScaffoldPT where    
--   fromCtx _ _ = ()
--   drawTermFromContext _ (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  


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
-- --   drawTermFromContext _ (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  


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
