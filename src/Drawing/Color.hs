{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Drawing.Color where

import Data.Bifunctor
import Data.Tuple
-- import Data.Colour
import Data.Maybe

import Data.List

-- import Data.Floating.Classes

import Combi

-- type NPoint = [ Float ]

-- data Seg = Pt Float | Seg Float Float
data Color = Rgba Float Float Float Float
  deriving (Show)
            
gray x = Rgba x x x 1.0 

color2arr (Rgba r g b a) = [r , g , b , a] 

class Colorlike a where
  toColor :: a -> Color


instance Colorlike Color where
  toColor = id


instance Colorlike a => Colorlike (b , a) where
  toColor = toColor . snd

instance Colorlike [Color] where
  toColor = head


instance Colorlike () where
  toColor _ = (Rgba 0.5 0.5 0.5 1.0 )


mod1 :: Float -> Float
mod1 x = x - fromIntegral (floor x)

hsv :: Float -> Float -> Float -> Color
hsv h s v = case hi of
    0 -> Rgba v t p 1.0
    1 -> Rgba q v p 1.0
    2 -> Rgba p v t 1.0
    3 -> Rgba p q v 1.0
    4 -> Rgba t p v 1.0
    5 -> Rgba v p q 1.0
 where
  hi = floor (h/60) `mod` 6
  f = mod1 (h/60)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)

phiNumber :: Float
phiNumber = 1.618033988

nthColor :: Int -> Color
nthColor i = hsv (phiNumber * fromIntegral i * 360.0) 1.0 0.5


data Shade =
   Shade { shadeColor :: Color
         , shadeMode :: Int
         }

class Shadelike a where
  toShade :: a -> Shade
  toShade _ =
         Shade { shadeColor = (Rgba 0.5 0.5 0.5 1.0)
              , shadeMode = 0
            }
         
instance Shadelike Color where
  toShade c =
     Shade { shadeColor = c 
           , shadeMode = 0
            }  


-- instance Shadelike a => Shadelike (b , a) where
--   toShade = toShade . snd

instance Shadelike [Shade] where
  toShade = head


