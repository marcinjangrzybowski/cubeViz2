{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Abstract where

import Syntax

import Drawing.Base

import Data.Maybe
import Data.Bifunctor
import Data.Traversable

import qualified Data.Map as Map

data Cub t a = Cub t a | Hcomp t Name (Map.Map SubFace (Cub t a)) (Cub t a)


type Address = [SubFace]



instance Bifunctor Cub where
  bimap f g (Cub t a) = Cub (f t) (g a)
  bimap f g (Hcomp t n p a) = Hcomp (f t) n ((fmap $ bimap f g) p) (bimap f g a)

  
-- cubMap :: Cub t1 a1 -> Cub t2 a2
-- cubMap = undefined



foldFaces :: (a -> (Map.Map Face a) -> a) -> Cub t a -> a
foldFaces f (Cub t a) = a
foldFaces f (Hcomp _ _ pa a) =
   f (foldFaces f a) $
     (fmap (foldFaces f) $ Map.fromList $
        mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
      $ Map.toList pa)

class OfDim a where
  getDim :: a -> Int

  checkDim :: Int -> a -> Maybe a
  checkDim i a =
    if (getDim a == i)
    then Just a
    else Nothing

instance OfDim ((Env , Context) , Expr) where
  getDim = uncurry $ uncurry $ getExprDim

instance OfDim (Drawing a) where
  getDim = fromMaybe 0 . getDrawingDim



class OfDim a => ToCub a b c where
  toCub :: a -> (Cub b c)

class FromCub a b c where
  fromCub :: Cub b c -> a 


instance ToCub ((Env , Context) , Expr) ((Env , Context) , Expr) CellExpr where
  toCub ee@((env , ct) , (HComp n pa e)) =
    -- do
       --let botDim = getDim ee
       let pa2 = fmap (toCub . ((env , ct),)) pa
           b = (toCub ((env , ct) , e))
       in (Hcomp ee n pa2 b)

  toCub ee@((env , ct) , (Var vI tl)) = Cub ee (CellExpr vI tl) 
  toCub ee@((env , ct) , (ILam n x)) = toCub (first (second (flip addDimToContext n)) ee) 

-- class ofdim a => xxx a where
--   xmap :: floating b => ([b] -> [b]) -> ([b] -> [b]) -> a -> a

-- cmp :: (a -> b) -> Cub () a -> Cub () b
-- cmp f = fmap f
