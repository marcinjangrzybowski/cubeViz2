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
data GCData = GCData String (FromLI Piece (Int , Int , Piece))









renderGCD' :: (Float , Float , Float) -> GCData -> ZDrawing ColorType
renderGCD' (par1 , par2 , parTranslate) (GCData nm fli@(FromLI n f)) =
   FromLI n (\pc@(sbst , prm) ->
        let (_ , uniqN , pcOrg) = appLI pc fli
            pieceId = unemerate pcOrg + 1
            vfg = show (pieceId + 4)
            
            colId = if nm == "loop₁" then uniqN else pieceId            
            mainSmplx = primitivePiece False (par1 , par2) pc
            mirrorSmplx = primitivePiece True (par1 , par2) pc
            bdSmplxs = primitivePieceBd False (par1 , par2) pc
            mirrorBdSmplxs = primitivePieceBd True (par1 , par2) pc
            -- mainStyle = if n == 0 then [ExtrudeLines] else []
            -- TODO maybe vary color by permutation?
            color = nthColor colId
        in
          [(mainSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show pieceId), "gcd","vfgT"++vfg, "vfgF0"] , Basic) , color)) ] ++
          [(mirrorSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show pieceId), "gcd","vfgT"++vfg, "vfgF0"] , Basic) , color)) ] ++
          [(x  , ((["m"++(show n), "piece"++(show pieceId), "gcd","vfgT"++vfg, "vfgF0"] , Basic) , color)) | x <- bdSmplxs ] ++
          [(x  , ((["m"++(show n), "piece"++(show pieceId), "gcd","vfgT"++vfg, "vfgF0"] , Basic) , color)) | x <- mirrorBdSmplxs ]
         -- if n == 2 then [ ([[0.5+x*x',0.5+y*y']] , (([] , Basic) , nthColor 2))
         --                 | x <- [-1.0 , 1.0]
         --                 , y <- [-1.0 , 1.0]
         --                 , (x' , y') <- [(0.2,0.3),(0.3,0.2)] ] else []
            )
 where
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

renderGCD'Points :: (Float , Float , Float) -> GCData -> ZDrawing ColorType
renderGCD'Points (par1 , par2 , parTranslate) (GCData nm fli@(FromLI n f)) =
   FromLI n (\pc@(sbst , prm) ->
        let (_ , uniqN , pcOrg) = appLI pc fli
            pieceId = unemerate pcOrg + 1
            vfg = show (pieceId + 4)
            
            colId = if nm == "loop₁" then uniqN else pieceId            
            gridPoints = primitivePiecePoint (par1 , par2) pc
            -- mirrorSmplx = primitivePiece True (par1 , par2) pc
            -- bdSmplxs = primitivePieceBd False (par1 , par2) pc
            -- mirrorBdSmplxs = primitivePieceBd True (par1 , par2) pc
            -- mainStyle = if n == 0 then [ExtrudeLines] else []
            -- TODO maybe vary color by permutation?
            color = nthColor colId
        in
          -- [(mainSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show pieceId), "gcd","vfgT"++vfg, "vfgF0"] , Basic) , color)) ] ++
          -- [(mirrorSmplx  , ((["ms"++(show n), "m"++(show n), "piece"++(show pieceId), "gcd","vfgT"++vfg, "vfgF0"] , Basic) , color)) ] ++
          -- [(x  , ((["m"++(show n), "piece"++(show pieceId), "gcd","vfgT"++vfg, "vfgF0"] , Basic) , color)) | x <- bdSmplxs ] ++
          -- [(x  , ((["m"++(show n), "piece"++(show pieceId), "gcd","vfgT"++vfg, "vfgF0"] , Basic) , color)) | x <- mirrorBdSmplxs ]
         if n == 2 then [ (mainPoint , ((["vfgT"++vfg, "vfgF0" , "moveFibres"] , Basic) , nthColor colId))
                         | mainPoint <- gridPoints ] else []
            )
 where
   primitivePiecePoint :: (Float , Float) -> Piece -> [Smplx]
   primitivePiecePoint (distCorner , distCenter) (su , pm) =
     let crnr = toListLI su
         n = length crnr
         ptCrnr = map (bool (0.0 + distCorner) (1.0 - distCorner)) crnr
         ptCenter = map (bool (0.0 + distCenter) (1.0 - distCenter)) crnr
         [bX , bY] = map (bool (-1.0) (1.0)) crnr
         grdN = 5
         
         dX = 0.5 / fromIntegral grdN
         dY = 0.5 / fromIntegral grdN
         pts = [[[0.5+bX*(fromIntegral i + if (unemerate pm == 0) then 0.25 else 0.5)*dX
                  ,0.5+bY*(fromIntegral j +if (unemerate pm == 0) then 0.5 else 0.25)*dY]]  | i <- range (grdN - 2) , j <- range (grdN - 2)  , let pmB = (i > j) `xor` (unemerate pm == 0) , pmB ];

         
         -- TODO how to fix this?
         -- squishedPt =
         --   scaleNonUniformOrigin [0.25, 1] middle mirroredPt
     in
       if n > 2
       then error "unimplemented: dim > 2"
       else pts



par1 = 0.3
par2 = 0.4
parTranslate = 0.2


renderGCD :: GCData -> ZDrawing ColorType
renderGCD = renderGCD' (par1 , par2 , parTranslate)


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
      
  in ( newGCCGS ,
          GCData defName $
            (FromLI n (\pc -> (1 , 1 , pc))))

initGCContext :: (Env , Context) -> GCContext
initGCContext (ee , Context l _) =
  (\x -> foldr x (initialGCContextGenState , Map.empty ) (zip (reverse (range (length l))) l) )
      (\ (i , y) (gCCGS , gcc) -> 
           let (newGCCGS , newGCD) =  (makeGCD (ee , Context l []) gCCGS i y)
           in (newGCCGS , Map.insert i newGCD gcc)
       ) & snd


instance DiaDegPiece (Int, Int, Piece) where
  pieceAppNegs _ = id
  
  pieceAppDiags _ = id

  pieceAppPerm _ = id


  
instance DiaDeg GCData where
  appNegs l (GCData defName x) = GCData defName $ appNegs l x

  appDiags l (GCData defName x) = GCData defName $ appDiags l x

  appPerm l (GCData defName x) = GCData defName $ appPerm l x
