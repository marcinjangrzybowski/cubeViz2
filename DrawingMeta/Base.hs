{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Drawing.Base where

import Data.Bifunctor
import Data.Tuple
-- import Data.Colour
import Data.Maybe

import Data.List

-- import Data.Floating.Classes

import Combi

import Drawing.Color

-- type NPoint = [ Float ]

-- data Seg = Pt Float | Seg Float Float


class InSpace a where
  sMap :: ([Float] -> [Float]) -> a -> a


instance {-# OVERLAPPING #-} InSpace [Float] where
  sMap = id

instance (InSpace b)  => InSpace [b] where
  sMap = fmap . sMap



-- instance Bi a => InSpace (a [ Float ]) where
--   sMap = fmap   

type Smplx = ([[ Float ]] )     


type Drawing a = [(Smplx , a)]


type Pt3D = (Float , Float , Float)

data Renderable = Point Pt3D | Line (Pt3D , Pt3D) | Triangle (Pt3D , Pt3D , Pt3D)

type Renderables = [(Renderable,Color)]  


-- data Renderable
