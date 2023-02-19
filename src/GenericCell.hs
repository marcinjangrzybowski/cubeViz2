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
data GCData = GCData (FromLI Subset (Int , Int))




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

parSize = 0.05
par1 = 0.3 -- 0.3 - parSize
par2 = 0.4 -- 0.3 + parSize
parTranslate = 0.2

renderGCD :: GCData -> ZDrawing ColorType
renderGCD (GCData fli@(FromLI n f)) =
   FromLI n (\pc@(sbst , prm) ->
        let (_ , uniqN) = appLI sbst fli
            colId = uniqN -- unemerate sbst
            mainSmplx = primitivePiece False (par1 , par2) pc
            mirrorSmplx = primitivePiece True (par1 , par2) pc
            bdSmplxs = primitivePieceBd False (par1 , par2) pc
            mirrorBdSmplxs = primitivePieceBd True (par1 , par2) pc
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
  in ( newGCCGS , GCData $ fromListFLI n newGCD)

initGCContext :: (Env , Context) -> GCContext
initGCContext (ee , Context l _) =
  (\x -> foldr x (initialGCContextGenState , Map.empty ) (zip (reverse (range (length l))) l) )
      (\ (i , y) (gCCGS , gcc) -> 
           let (newGCCGS , newGCD) =  (makeGCD (ee , Context l []) gCCGS i y)
           in (newGCCGS , Map.insert i newGCD gcc)
       ) & snd


instance DiaDeg GCData where
  appNegs l (GCData x) = GCData $ appNegs l x

  appDiags l (GCData x) = GCData $ appDiags l x

  appPerm l (GCData x) = GCData $ appPerm l x
