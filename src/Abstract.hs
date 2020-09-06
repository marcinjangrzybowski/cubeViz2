{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Abstract where

import Syntax

-- import Drawing.Base

import Data.Maybe
import Data.Bifunctor
import Data.Traversable

import Control.Applicative

import Combi

import qualified Data.Map as Map

data Cub a = Cub Int (FromLI Face (Cub a)) a | Hcomp Name (Map.Map SubFace (Cub a)) (Cub a)
  deriving (Show , Functor)


type Address = [SubFace]



instance OfDim (Cub a) where
  getDim (Cub n _ _) = n 
  getDim (Hcomp nm pa x) = getDim x

     
cubMap :: (Int -> Address -> a -> Either e b)
                 -> Address
                 -> Cub a 
                 -> Either e (Cub b)
cubMap f addr (Cub n fcs a) =
   do cell <- f n addr a
      fcs2 <- traverseMapLI (\fc -> cubMap f (toSubFace fc : addr)) $ fcs
      return $ Cub n fcs2 cell

cubMap f addr (Hcomp name pa a) = 
   do 
      bot <- cubMap f (fullSF (getDim a) : addr) a
      sides <- traverseMap
                 (\sf -> cubMap f (sf : addr))
                 $ pa
      return (Hcomp name sides bot) 

foldSubFaces :: (a -> (Map.Map SubFace a) -> a) -> Cub a -> a
foldSubFaces f (Cub _ _ a) = a
foldSubFaces f (Hcomp _ pa a) =
  f (foldSubFaces f a) $ (fmap (foldSubFaces f) pa)


foldFaces :: (a -> (Map.Map Face a) -> a) -> Cub a -> a
foldFaces f (Cub _ _ a) = a
foldFaces f (Hcomp _ pa a) =
   f (foldFaces f a) $
     (fmap (foldFaces f) $ Map.fromList $
        mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
      $ Map.toList pa)


  
class OfDim a => ToCub a c where
  toCub :: a -> (Cub c)

class FromCub a c where
  fromCub :: Cub c -> a 


instance ToCub ((Env , Context) , Expr) CellExpr where
  toCub ee@((env , ct) , (HComp nam pa e)) =
       let ct2 = addDimToContext ct nam
           dim = getDim ee
           pa2 = Map.mapKeys (SubFace dim . Map.mapKeys (toDimI ct2)) $
                 Map.mapWithKey (\sf -> toCub . ((env , addSFConstraintToContext sf ct2),)) pa
           b   = (toCub ((env , ct) , e))
       in (Hcomp nam pa2 b)

  toCub (ee@(env , ct) , (Var vI tl)) =
     let (cell , fcs) = mkCellExpr ee vI tl
         fcss = fromLIppK
                  (\fc -> \e -> toCub ((env , addFaceConstraintToContext fc ct) , e) )
                 fcs
     in Cub (getDim ee) fcss cell  
  toCub ee@((env , ct) , (ILam nam x)) = toCub (first (second (flip addDimToContext nam)) ee) 


-- -- UNTESTED IN ANY WAY
-- makeGrid :: Int -> Int -> Cub ()
-- makeGrid dim 0 = Cub 0 ()
-- makeGrid dim depth =
--   let prev = (makeGrid dim (depth - 1))
--   in Hcomp "z" (Map.fromList $ map (flip (,) prev) (map toSubFace (genAllLI dim)) ) prev 
