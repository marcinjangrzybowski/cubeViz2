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

data Cub t a = Cub t a | Hcomp t Name (Map.Map SubFace (Cub t a)) (Cub t a)
  deriving Show






type Address = [SubFace]



instance Bifunctor Cub where
  bimap f g (Cub t a) = Cub (f t) (g a)
  bimap f g (Hcomp t n p a) = Hcomp (f t) n ((fmap $ bimap f g) p) (bimap f g a)



-- instance OfDim (Drawing a) where
--   getDim = fromMaybe 0 . getDrawingDim

instance OfDim t => OfDim (Cub t a) where
  getDim (Cub t _) = getDim t 
  getDim (Hcomp t _ _ _) = getDim t

-- instance OfDim (CellExpr) where
--   getDim = fromMaybe 0 . getDrawingDim




     
cubMap :: (OfDim t) => Int -> (Int -> t -> Address -> a -> Either e b)
                 -> Address
                 -> Cub t a 
                 -> Either e (Cub t b)
cubMap n f addr (Cub t a) = Cub t <$> f n t addr a 
cubMap n f addr (Hcomp t name pa a) =
   do 
      bot <- cubMap n f (fullSF (getDim a) : addr) a
      sides <- traverseMap
                 (\sf -> cubMap (n + 1 - getDim sf) f (sf : addr))
                 $ pa
      return (Hcomp t name sides bot) 

foldSubFaces :: (a -> (Map.Map SubFace a) -> a) -> Cub t a -> a
foldSubFaces f (Cub t a) = a
foldSubFaces f (Hcomp _ _ pa a) =
  f (foldSubFaces f a) $ (fmap (foldSubFaces f) pa)


foldFaces :: (a -> (Map.Map Face a) -> a) -> Cub t a -> a
foldFaces f (Cub t a) = a
foldFaces f (Hcomp _ _ pa a) =
   f (foldFaces f a) $
     (fmap (foldFaces f) $ Map.fromList $
        mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
      $ Map.toList pa)


class OfDim a => ToCub a b c where
  toCub :: a -> (Cub b c)

class FromCub a b c where
  fromCub :: Cub b c -> a 


instance ToCub ((Env , Context) , Expr) ((Env , Context) , Expr) CellExpr where
  toCub ee@((env , ct) , (HComp n pa e)) =
       let ct2 = addDimToContext ct n
           dim = getDim ee
           pa2 = Map.mapKeys (SubFace dim . Map.mapKeys (toDimI ct2)) $
                 Map.mapWithKey (\sf -> toCub . ((env , addSFConstraintToContext sf ct2),)) pa
           b   = (toCub ((env , ct) , e))
       in (Hcomp ee n pa2 b)

  toCub ee@((env , ct) , (Var vI tl)) = Cub ee (CellExpr vI tl) 
  toCub ee@((env , ct) , (ILam n x)) = toCub (first (second (flip addDimToContext n)) ee) 

-- -- cmp :: (a -> b) -> Cub () a -> Cub () b
-- -- cmp f = fmap f


-- makeGrid :: Int -> Int -> Cub () ()
-- makeGrid dim 0 = Cub () ()
-- makeGrid dim depth =
--   let prev = (makeGrid dim (depth - 1))
--   in Hcomp () "z" (Map.fromList $ map (flip (,) prev) (map faceToSubFace2 (allFaces dim)) ) prev 
