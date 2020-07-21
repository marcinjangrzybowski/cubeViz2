{-# LANGUAGE DeriveFunctor #-}

module Abstract where

import Syntax



data Cub t a = Cub t a | Hcomp t Name (SubFace -> Maybe (Cub a t)) (Cub a t)

type Address = [SubFace]




class OfDim a where
  getDim :: a -> Int


class OfDim a => Xxx a where
  xMap :: Floating b => ([b] -> [b]) -> ([b] -> [b]) -> a -> a

-- cmp :: (a -> b) -> Cub () a -> Cub () b
-- cmp f = fmap f
