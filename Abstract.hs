{-# LANGUAGE DeriveFunctor #-}

module Abstract where

import Syntax



data Cub t a = Cub t a | Hcomp t Name (SubFace -> Maybe (Cub a t)) (Cub a t)

type Address = [SubFace]




-- class ofdim a where
--   getdim :: a -> int


-- class ofdim a => xxx a where
--   xmap :: floating b => ([b] -> [b]) -> ([b] -> [b]) -> a -> a

-- cmp :: (a -> b) -> Cub () a -> Cub () b
-- cmp f = fmap f
