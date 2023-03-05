{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE ScopedTypeVariables #-}

module ConcreteCell where

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

import GenericCell

data ConcreteCellData = ConcreteCellData { ccdDrawing :: (ZDrawing ColorType) }



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


par1Loop2 = 0.2
par2Loop2 = 0.3
parTranslateLoop2 = 0.1

triangle1 = [[0.0, 0.05], [0.05, 0.05], [0.0, 0.0]]


renderNamedCell :: String -> Maybe (ZDrawing ColorType)
renderNamedCell "loop₂" = Just $
  let n = 2
  in FromLI 2 (\pc@(sbst , prm) ->
        let shouldTranspose = foldr xor False (toListLI sbst)
            pcId = unemerate prm + 1
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
            colorFst = nthColor 1 --nthColor (unemerate pcOrg) --
            colorSnd = nthColor 2 --nthColor (unemerate pcOrg) -- nthColor 2
            (colorOver, colorUnder) =
              case toListLI sbst of
                [False, False] -> (colorFst, colorFst)
                [True, True] -> (colorSnd, colorSnd)
                [True, False] -> (colorFst, colorSnd)
                [False, True] -> (colorSnd, colorFst)
        in [(mainSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show pcId), "gcd"] , Basic) , colorUnder)) | mainSmplx <- mainSmplxs] ++
           [(mainSmplx2 , ((["ms"++(show n), "m"++(show n), "piece"++(show pcId), "gcd"] , Basic) , colorOver)) | mainSmplx2 <- mainSmplxs2] ++
           [(mainSmplx3 , ((["ms"++(show n), "m"++(show n), "piece"++(show pcId), "gcd"] , Basic) , colorOver)) | mainSmplx3 <- mainSmplxs3])


-- renderNamedCell "loop₂" = Just $ 
--    let n = 1 in
--    FromLI 1 (\pc@(sbst , prm) ->
--         let rotQuarter = scaleNonUniformOrigin [if b then -1.0 else 1.0 | b <- toListLI sbst] [0.5]
--             pcId = unemerate prm + 1
--             pcPrime = (fromListLI [False], prm)
--             (mainSmplxs, mainSmplxs2) =
--               rotQuarter (primitivePieceLoop1d (par1Loop2 , par2Loop2) pcPrime)
--             -- (bdSmplxs, bdSmplxs2) = primitivePieceLoop2Bd undefined (par1Loop2 , par2Loop2) pc
--             -- mainStyle = if n == 0 then [ExtrudeLines] else []
--             -- TODO maybe vary color by permutation?
--             colorFst = nthColor (unemerate pc) --nthColor 1
--             colorSnd = nthColor (unemerate pc) --nthColor 2
--             (colorOver, colorUnder) =
--               case toListLI sbst of
--                 [False] -> (colorFst, colorFst)
--                 [True] -> (colorSnd, colorSnd)
--         in [(mainSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show pcId), "gcd"] , Basic) , colorUnder)) | mainSmplx <- mainSmplxs] ++
--            [(mainSmplx2 , ((["ms"++(show n), "m"++(show n), "piece"++(show pcId), "gcd"] , Basic) , colorOver)) | mainSmplx2 <- mainSmplxs2])




renderNamedCell "loop₁" = Just $
   renderGCD'
    (par1Loop2 , par2Loop2 , parTranslateLoop2)
    (GCData "" $ FromLI 1 (\pc -> (unemerate pc , (unemerate pc + 1) , pc)))
  where
   par1Loop2 = 0.2
   par2Loop2 = 0.3
   parTranslateLoop2 = 0.1


-- renderNamedCell "s" = Just $
--    renderGCD'Points
--     (par1 , par2 , parTranslate)
--     (GCData "" $ FromLI 2 (\pc -> (unemerate pc , (unemerate pc + 1) , pc)))

renderNamedCell _ = Nothing

-- renderGCD (GCData "loop₂" fli@(FromLI 2 f)) =
--    let n = 2 in
--    FromLI n (\pc@(sbst , prm) ->
--         let (_ , uniqN , pcOrg@(sbstOrg , prmOrg)) = appLI pc fli
--             shouldTranspose = foldr xor False (toListLI sbstOrg)
--             stacking =
--               case (toListLI sbstOrg) of
--                 [False, False] -> True
--                 [True, True] -> True
--                 [True, False] -> True
--                 [False, True] -> True
--             withCrossing = not ((unemerate prmOrg == 0) `xor` (foldr xor False (toListLI sbstOrg)))
--             rotQuarter = scaleNonUniformOrigin [if b then -1.0 else 1.0 | b <- toListLI sbst] [0.5, 0.5]
--                          . sMap (\[x,y] -> if shouldTranspose then [y,x] else [x,y])
--             pcPrime = (fromListLI [False, False], prm)
--             (mainSmplxs, mainSmplxs2, mainSmplxs3) =
--               rotQuarter (primitivePieceLoop2 withCrossing stacking (par1Loop2 , par2Loop2) pcPrime)
--             -- (bdSmplxs, bdSmplxs2) = primitivePieceLoop2Bd undefined (par1Loop2 , par2Loop2) pc
--             -- mainStyle = if n == 0 then [ExtrudeLines] else []
--             -- TODO maybe vary color by permutation?
--             colorFst = nthColor 1 --nthColor (unemerate pcOrg) --
--             colorSnd = nthColor 2 --nthColor (unemerate pcOrg) -- nthColor 2
--             (colorOver, colorUnder) =
--               case toListLI sbstOrg of
--                 [False, False] -> (colorFst, colorFst)
--                 [True, True] -> (colorSnd, colorSnd)
--                 [True, False] -> (colorFst, colorSnd)
--                 [False, True] -> (colorSnd, colorFst)
--         in [(mainSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , colorUnder)) | mainSmplx <- mainSmplxs] ++
--            [(mainSmplx2 , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , colorOver)) | mainSmplx2 <- mainSmplxs2] ++
--            [(mainSmplx3 , ((["ms"++(show n), "m"++(show n), "piece"++(show uniqN), "gcd"] , Basic) , colorOver)) | mainSmplx3 <- mainSmplxs3])

-- renderGCD (GCData "loop₃" fli@(FromLI 3 f)) =
--   FromLI 3 (\_ -> [])


instance DiaDegPiece [(Smplx, ColorType)] where
  pieceAppNegs ngs =
    scaleNonUniformOrigin [if b then 1.0 else -1.0 | b <- ngs] [0.5, 0.5]
                         
  
  pieceAppDiags _ = id

  pieceAppPerm prm = sMap (listPermute prm)


instance DiaDeg ConcreteCellData where
  appNegs l (ConcreteCellData x) = ConcreteCellData $ appNegs l x

  appDiags l (ConcreteCellData x) = ConcreteCellData $ appDiags l x

  appPerm l (ConcreteCellData x) = ConcreteCellData $ appPerm l x
