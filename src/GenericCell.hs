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




primitivePiece :: (Float , Float) -> Piece -> Smplx
primitivePiece (distCorner , distCenter) (su , pm) =
  let crnr = toListLI su
      n = length crnr
      ptCrnr = map (bool (0.0 + distCorner) (1.0 - distCorner)) crnr
      ptCenter = map (bool (0.0 + distCenter) (1.0 - distCenter)) crnr
      restPts :: ([Float] , [[Float]] )
      restPts = mapAccumL
                      (  fmap dupe . (\ pt k -> updateAt (ptCenter !! k ) k pt) )
                      ptCrnr
                      (toListLI $ invPerm pm)

  in ptCrnr : snd restPts

primitivePieceBd :: (Float , Float) -> Piece -> [Smplx]
primitivePieceBd x y | (getDim y == 0) = []
                     | otherwise =
  [ init (primitivePiece x y) , tail (primitivePiece x y) ]

par1 = 0.4
par2 = 0.5


renderGCDSolid :: GCData -> ZDrawing ColorType
renderGCDSolid (GCData fli@(FromLI n f)) =
   FromLI n (\pc@(sbst , prm) ->
        let (colId , uniqN) = appLI sbst fli
            mainSmplx = primitivePiece (par1 , par2) pc
        in [(mainSmplx  , (([] , Basic) , nthColor colId)) ]
               )

renderGCD :: GCData -> ZDrawing ColorType
renderGCD (GCData fli@(FromLI n f)) =
   FromLI n (\pc@(sbst , prm) ->
        let (colId , uniqN) = appLI sbst fli
            mainSmplx = primitivePiece (par1 , par2) pc
            bdSmplxs = primitivePieceBd (par1 , par2) pc
            -- mainStyle = if n == 0 then [ExtrudeLines] else []
        in
          [(mainSmplx  , ((["ms"++(show n)] , Basic) , nthColor colId)) ] ++
          [(x  , (([] , Basic) , nthColor colId)) | x <- bdSmplxs ])

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
