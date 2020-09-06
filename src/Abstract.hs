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

data Cub a = Cub Int a | Hcomp Name (Map.Map SubFace (Cub a)) (Cub a)
  deriving Show


type Address = [SubFace]

instance Functor Cub where
  fmap f (Cub n a) = Cub n (f a)
  fmap f (Hcomp name p a) = Hcomp name ((fmap $ fmap f) p) (fmap f a)




instance OfDim (Cub a) where
  getDim (Cub n _) = n 
  getDim (Hcomp nm pa x) = getDim x

     
cubMap :: (Int -> Address -> a -> Either e b)
                 -> Address
                 -> Cub a 
                 -> Either e (Cub b)
cubMap f addr (Cub n a) = Cub n <$> f n addr a 
cubMap f addr (Hcomp name pa a) = 
   do 
      bot <- cubMap f (fullSF (getDim a) : addr) a
      sides <- traverseMap
                 (\sf -> cubMap f (sf : addr))
                 $ pa
      return (Hcomp name sides bot) 

foldSubFaces :: (a -> (Map.Map SubFace a) -> a) -> Cub a -> a
foldSubFaces f (Cub _ a) = a
foldSubFaces f (Hcomp _ pa a) =
  f (foldSubFaces f a) $ (fmap (foldSubFaces f) pa)


foldFaces :: (a -> (Map.Map Face a) -> a) -> Cub a -> a
foldFaces f (Cub _ a) = a
foldFaces f (Hcomp _ pa a) =
   f (foldFaces f a) $
     (fmap (foldFaces f) $ Map.fromList $
        mapMaybe (\( sf , x) -> fmap (,x) $ toFace sf )
      $ Map.toList pa)



class OfDim a => Faceable a where

  getFaceFCpriv :: Face -> a -> a

  getFaceFC :: Face -> a -> a
  getFaceFC fc@(Face n bl) a | n /= getDim a = error "dim of face diferent than dim of Faceble arg"
                             | otherwise = getFaceFCpriv fc a

instance (OfDim a , Faceable a) => Faceable (Cub a) where

  getFaceFCpriv fc (Cub n a) = Cub (n - 1) (getFaceFC fc a) 
  getFaceFCpriv (Face n bl) (Hcomp m pa a) = undefined
  
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

  toCub ee@((env , ct) , (Var vI tl)) = Cub (getDim ee) (remapCE (toDimI ct) (CellExpr vI tl))  
  toCub ee@((env , ct) , (ILam nam x)) = toCub (first (second (flip addDimToContext nam)) ee) 


-- UNTESTED IN ANY WAY
makeGrid :: Int -> Int -> Cub ()
makeGrid dim 0 = Cub 0 ()
makeGrid dim depth =
  let prev = (makeGrid dim (depth - 1))
  in Hcomp "z" (Map.fromList $ map (flip (,) prev) (map toSubFace (genAllLI dim)) ) prev 
