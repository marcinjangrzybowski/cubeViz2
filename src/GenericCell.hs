{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE ScopedTypeVariables #-}

module GenericCell where

import Abstract

import Syntax


import Drawing.Base
import Drawing.Color

import Data.Maybe
import Data.Either
import Data.Function

import Control.Applicative

import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map

-- import Control.Arrow

import Combi

import FloatTools

import Data.Bool
import Data.List
import qualified Data.Set as Set

import Data.Tuple.Extra (second , first , dupe)

import PiecesEval

import Reorientable

import DataExtra

import Debug.Trace

              -- first int refers to "color" (index of def of dim0)
              -- second int refers to uniqCounter for this primitive
data GCData = GCData String (FromLI Subset (Int , Int))




primitivePiece :: Bool -> (Float , Float) -> Piece -> Smplx
primitivePiece mirror (distCorner , distCenter) (su , pm) =
  let crnr = toListLI su
      n = length crnr
      ptCrnr = map (bool (0.0 + distCorner) (1.0 - distCorner)) crnr
      ptCenter = map (bool (0.0 + distCenter) (1.0 - distCenter)) crnr
      restPts :: ([Float] , [[Float]] )
      restPts = mapAccumL
                      (  fmap dupe . (\ pt k -> updateAt (ptCenter !! k ) k pt) )
                      ptCrnr
                      (toListLI $ invPerm pm)
      pt = ptCrnr : snd restPts
      cornerVector = [if k == unemerate pm then 0 else (if b then 1 else -1) | (k, b) <- zip [0..n-1] crnr]
      mean pts = map (\x -> x / fromIntegral (length pts)) (map sum (transpose pts))
      middle = translatePar cornerVector parTranslate (mean [ptCrnr, ptCenter])
      translatedPt = translatePar cornerVector parTranslate pt
      mirroredPt =
        if mirror
        then scaleOrigin (-1.0) middle translatedPt
        else translatedPt
      squishedPt = mirroredPt
      -- TODO how to fix this?
      -- squishedPt =
      --   scaleNonUniformOrigin [0.25, 1] middle mirroredPt
  in
    if n > 2
    then error "unimplemented: dim > 2"
    else squishedPt

primitivePieceBd :: Bool -> (Float , Float) -> Piece -> [Smplx]
primitivePieceBd mirror x y | (getDim y == 0) = []
                     | otherwise =
  [ init (primitivePiece mirror x y) , tail (primitivePiece mirror x y) ]

primitivePieceLoop1d :: (Float , Float) -> Piece -> ([Smplx], [Smplx])
primitivePieceLoop1d (distCorner , distCenter) (su , pm) =
  let crnr = toListLI su
      n = length crnr
      width = distCenter - distCorner
      distCornerPrime = distCorner - 0.6 * width
      distCenterPrime = distCenter - (0.6 * width + 0.03)
      smallCubeCenter = map (bool 0.25 0.75) crnr
      ptCrnr = map (bool (0.0 + distCornerPrime) (1.0 - distCornerPrime)) crnr
      ptCenter = map (bool (0.0 + distCenterPrime) (1.0 - distCenterPrime)) crnr
      smallTriangleMirrorTransformation =
        sMap (\[x] -> [(-1.0) * (x - 0.25) + 0.25])
      half = [[ptCrnr, ptCenter]]
      pt = (smallTriangleMirrorTransformation half, half)
  in pt


primitivePieceLoop2 :: Bool -> Bool -> (Float , Float) -> Piece -> ([Smplx], [Smplx], [Smplx])
primitivePieceLoop2 withCrossing stacking (distCorner , distCenter) (su , pm) =
  let crnr = toListLI su
      n = length crnr
      width = distCenter - distCorner
      distCornerPrime = distCorner - 0.6 * width
      distCenterPrime = distCenter - (0.6 * width + 0.03)
      smallCubeCenter = map (bool 0.25 0.75) crnr
      ptCrnr = map (bool (0.0 + distCornerPrime) (1.0 - distCornerPrime)) crnr
      ptCenter = map (bool (0.0 + distCenterPrime) (1.0 - distCenterPrime)) crnr
      smallTriangleMirrorTransformation =
        sMap (\[x,y] ->
                 let xx = x - 0.5
                     yy = y - 0.5
                     u = xx + yy
                     v = xx - yy
                     xxx = ((-1.0) * u + v)/2.0
                     yyy = ((-1.0) * u + (-1.0) * v)/2.0
                 in [xxx, yyy])
      pt = if not withCrossing
           then
             let half = [[ptCrnr, ptCenter, [0.2, 0.0]],
                         [ptCenter, [0.2, 0.0], [0.3, 0.0]]]
             in (smallTriangleMirrorTransformation half, half, [])
           else
             let alpha = ((1.0/8) - (width / ((sqrt 2.0) * 2.0)))
                 beta = 0.05
                 gamma = 0.025
                 c1 = [alpha + beta, 0.5 - alpha - beta]
                 c2 = [0.25 - alpha + beta, 0.25 + alpha - beta]
                 c3 = [alpha + beta, 0.25 + alpha - beta]
                 c1Prime = [alpha + beta - gamma, 0.5 - alpha - beta]
                 c2Prime = [0.25 - alpha + beta, 0.25 + alpha - beta]
                 c3Prime = [alpha + beta - gamma, 0.25 + alpha - beta]
                 c1PrimePrime = [alpha + beta, 0.5 - alpha - beta]
                 c2PrimePrime = [0.25 - alpha + beta, 0.25 + alpha - beta - gamma]
                 c3PrimePrime = [alpha + beta, 0.25 + alpha - beta - gamma]
                 redHalfs =
                   ([[c1, [0.0, 0.2], [0.0, 0.3]],
                     [c3, c1, [0.0, 0.2]]],
                    [[c1Prime, [0.0, 0.2], [0.0, 0.3]],
                     [c3Prime, c1Prime, [0.0, 0.2]]])
                 blueHalfs =
                   ([[c2, ptCrnr, ptCenter],
                     [c3, c2, ptCrnr]],
                    [[c2PrimePrime, ptCrnr, ptCenter],
                     [c3PrimePrime, c2PrimePrime, ptCrnr]])
                 (redHalf, redHalfM, blueHalf, blueHalfM) =
                   if not stacking
                   then (fst redHalfs, snd redHalfs, snd blueHalfs, fst blueHalfs)
                   else (snd redHalfs, fst redHalfs, fst blueHalfs, snd blueHalfs)
                 crossing =
                   [[c1, c3, c2]]
             in (redHalf ++ smallTriangleMirrorTransformation blueHalfM,
                 blueHalf ++ smallTriangleMirrorTransformation redHalfM,
                 crossing ++ smallTriangleMirrorTransformation crossing)
  in
    pt

-- primitivePieceLoop2Bd :: Bool -> (Float , Float) -> Piece -> [Smplx]
-- primitivePieceLoop2Bd mirror x y | (getDim y == 0) = []
--                      | otherwise =
--   [ init (primitivePieceLoop2 mirror x y) , tail (primitivePieceLoop2 mirror x y) ]

par1 = 0.3
par2 = 0.4
parTranslate = 0.2

par1Loop2 = 0.2
par2Loop2 = 0.3
parTranslateLoop2 = 0.1

triangle1 = [[0.0, 0.05], [0.05, 0.05], [0.0, 0.0]]

renderGCD :: GCData -> ZDrawing ColorType
renderGCD (GCData "loop₂" fli@(FromLI 1 f)) =
   let n = 1 in
   FromLI 1 (\pc@(sbst , prm) ->
        let (_ , uniqN) = appLI sbst fli
            rotQuarter = scaleNonUniformOrigin [if b then -1.0 else 1.0 | b <- toListLI sbst] [0.5]
            pcPrime = (fromListLI [False], prm)
            (mainSmplxs, mainSmplxs2) =
              rotQuarter (primitivePieceLoop1d (par1Loop2 , par2Loop2) pcPrime)
            -- (bdSmplxs, bdSmplxs2) = primitivePieceLoop2Bd undefined (par1Loop2 , par2Loop2) pc
            -- mainStyle = if n == 0 then [ExtrudeLines] else []
            -- TODO maybe vary color by permutation?
            colorFst = nthColor 1
            colorSnd = nthColor 2
            (colorOver, colorUnder) =
              case toListLI sbst of
                [False] -> (colorFst, colorFst)
                [True] -> (colorSnd, colorSnd)
        in [(mainSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , colorUnder)) | mainSmplx <- mainSmplxs] ++
           [(mainSmplx2 , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , colorOver)) | mainSmplx2 <- mainSmplxs2])

renderGCD (GCData "loop₂" fli@(FromLI 2 f)) =
   let n = 2 in
   FromLI n (\pc@(sbst , prm) ->
        let (_ , uniqN) = appLI sbst fli
            shouldTranspose = foldr xor False (toListLI sbst)
            stacking =
              case (toListLI sbst) of
                [False, False] -> True
                [True, True] -> True
                [True, False] -> True
                [False, True] -> True
            withCrossing = not ((unemerate prm == 0) `xor` (foldr xor False (toListLI sbst)))
            rotQuarter = scaleNonUniformOrigin [if b then -1.0 else 1.0 | b <- toListLI sbst] [0.5, 0.5]
                         . sMap (\[x,y] -> if shouldTranspose then [y,x] else [x,y])
            pcPrime = (fromListLI [False, False], prm)
            (mainSmplxs, mainSmplxs2, mainSmplxs3) =
              rotQuarter (primitivePieceLoop2 withCrossing stacking (par1Loop2 , par2Loop2) pcPrime)
            -- (bdSmplxs, bdSmplxs2) = primitivePieceLoop2Bd undefined (par1Loop2 , par2Loop2) pc
            -- mainStyle = if n == 0 then [ExtrudeLines] else []
            -- TODO maybe vary color by permutation?
            colorFst = nthColor 1
            colorSnd = nthColor 2
            (colorOver, colorUnder) =
              case toListLI sbst of
                [False, False] -> (colorFst, colorFst)
                [True, True] -> (colorSnd, colorSnd)
                [True, False] -> (colorFst, colorSnd)
                [False, True] -> (colorSnd, colorFst)
        in [(mainSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , colorUnder)) | mainSmplx <- mainSmplxs] ++
           [(mainSmplx2 , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , colorOver)) | mainSmplx2 <- mainSmplxs2] ++
           [(mainSmplx3 , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , colorOver)) | mainSmplx3 <- mainSmplxs3])

renderGCD (GCData "loop₃" fli@(FromLI 3 f)) =
  FromLI 3 (\_ -> [])

renderGCD (GCData nm fli@(FromLI n f)) =
   let (par1', par2') = (if nm == "loop₁" then (par1Loop2, par2Loop2) else (par1, par2)) in
   FromLI n (\pc@(sbst , prm) ->
        let (_ , uniqN) = appLI sbst fli
            colId = uniqN -- unemerate sbst
            mainSmplx = primitivePiece False (par1' , par2') pc
            mirrorSmplx = primitivePiece True (par1' , par2') pc
            bdSmplxs = primitivePieceBd False (par1' , par2') pc
            mirrorBdSmplxs = primitivePieceBd True (par1' , par2') pc
            -- mainStyle = if n == 0 then [ExtrudeLines] else []
            -- TODO maybe vary color by permutation?
            color = nthColor colId
        in
          [(mainSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , color)) ] ++
          [(mirrorSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , color)) ] ++
          [(x  , ((["m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , color)) | x <- bdSmplxs ] ++
          [(x  , ((["m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , color)) | x <- mirrorBdSmplxs ])

type GCContext = Map.Map Int GCData


-- mapping variableIds of definitions ofDim0 into ( colorNr , uniqCounter) 
type GCContextGenState = Map.Map Int (Int , Int)

initialGCContextGenState :: GCContextGenState
initialGCContextGenState = Map.empty

makeGCD :: (Env , Context) -> GCContextGenState
                  -> Int -> (String , CType) -> (GCContextGenState , GCData)
makeGCD (ee , ctx0) gccGS i (defName , ct) =
  let n = getCTyDim ee ctx0 ct
      ctx = foldl addDimToContext ctx0 (replicate n Nothing )
      crnrs = exprCorners ctx (mkVar ctx (VarIndex i) (dim <$> range n))

      visitCorner :: GCContextGenState -> VarIndex -> (GCContextGenState , (Int , Int))
      visitCorner gCCGS (VarIndex k) =
         let xx =
                  Map.lookup  k gCCGS
                & maybe (Set.size (Map.keysSet gCCGS) , 0) (second $ (+) 1)

         in (Map.insert k xx gCCGS , xx)



      ( newGCCGS , newGCD) =  mapAccumL visitCorner gccGS (toListFLI crnrs)
  in ( newGCCGS , GCData defName $ fromListFLI n newGCD)

initGCContext :: (Env , Context) -> GCContext
initGCContext (ee , Context l _) =
  (\x -> foldr x (initialGCContextGenState , Map.empty ) (zip (reverse (range (length l))) l) )
      (\ (i , y) (gCCGS , gcc) -> 
           let (newGCCGS , newGCD) =  (makeGCD (ee , Context l []) gCCGS i y)
           in (newGCCGS , Map.insert i newGCD gcc)
       ) & snd


instance DiaDeg GCData where
  appNegs l (GCData defName x) = GCData defName $ appNegs l x

  appDiags l (GCData defName x) = GCData defName $ appDiags l x

  appPerm l (GCData defName x) = GCData defName $ appPerm l x
