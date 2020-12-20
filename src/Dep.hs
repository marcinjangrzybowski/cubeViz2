{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Dep where

import Data.Finite
import GHC.TypeLits

import qualified Data.Set as Set

import qualified Data.Map as Map

import Algebra.Lattice

factorial :: Integer -> Integer
factorial n =
  case n of
    0 -> 1
    1 -> 1
    2 -> 2
    3 -> 6
    4 -> 24
    5 -> 120
    _ -> n * factorial (n - 1)

newtype Vecc a (n :: Nat) = Vecc [a] deriving (Eq, Ord)


mkVecc :: (KnownNat n) => [a] -> Vecc a n
mkVecc la = result          
  where
    result = if length la == fromIntegral (natVal result)
             then Vecc la
             else error $ "length do not match! (expected :" ++ show (natVal result)
                           ++ ") but legth of arg was: " ++ show (length la)

data OfDimK a = forall n. KnownNat n => OfDimK (a n)

data OfDimU a = forall n. OfDimU (a n)

getDim :: OfDimK a -> Integer
getDim (OfDimK x) = natVal x

emptyVecc :: Vecc a 0
emptyVecc = Vecc []

consVecc :: a -> Vecc a n -> Vecc a (n + 1)
consVecc a (Vecc la) = (Vecc (a : la))


toVeccU :: forall a. [a] -> OfDimK (Vecc a)
toVeccU x =
  case length x of
    0 -> OfDimK (Vecc x :: Vecc a 0 )
    1 -> OfDimK (Vecc x :: Vecc a 1 )
    2 -> OfDimK (Vecc x :: Vecc a 2 )
    3 -> OfDimK (Vecc x :: Vecc a 3 )
    4 -> OfDimK (Vecc x :: Vecc a 4 )
    5 -> OfDimK (Vecc x :: Vecc a 5 )
    6 -> OfDimK (Vecc x :: Vecc a 6 )



newtype Permutation (n :: Nat) = Permutation (Vecc (Finite n) n)
  deriving Eq

newtype Subset (n :: Nat) = Subset (Set.Set (Finite n))
  deriving Eq

newtype Face (n :: Nat) = Face (Finite n , Bool)
  deriving Eq

instance Show (Face n) where
  show (Face (i , b)) = "F" ++ show (getFinite i) ++ "-" ++ (if b then "1" else "0")

newtype SubFace (n :: Nat) = SubFace (Map.Map (Finite n) Bool)
  deriving (Eq , Show)


class HasCard a where
  cardinality :: a -> Integer

class Gradable a where

  grade :: KnownNat n => a n -> Finite n

newtype Graded a (n :: Nat) (m :: Nat) = Graded (a n)
  deriving (Show)

mkGraded :: (KnownNat n , KnownNat m , Gradable a) => a n -> Graded a n m
mkGraded a | gradeTest = result
           | otherwise = error "grade Error"              
  where

    result = Graded a
    
    gradeTest = getFinite (grade a) == natVal result
  



newtype GradedMap a b (n :: Nat) = GradedMap (Map.Map (a n) (OfDimK b))

mkGradedMap :: (KnownNat n , Gradable a) => Map.Map (a n) (OfDimK b) -> GradedMap a b n
mkGradedMap mp | gradesTest = GradedMap mp
               | otherwise = error "grades Error"
  where
    gradesTest = all (\ (a, b) -> getFinite (grade a) == getDim b) (Map.toList mp)


instance KnownNat a => HasCard (Permutation a) where
  cardinality a = factorial (natVal a)

instance KnownNat a => HasCard (Subset a) where
  cardinality a = 2 ^ natVal a

instance KnownNat a => HasCard (Face a) where
  cardinality a = 2 * natVal a

instance KnownNat a => HasCard (SubFace a) where
  cardinality a = 3 ^ natVal a

instance Gradable SubFace where
  grade sf@(SubFace mp) = finite (natVal sf - fromIntegral (Map.size mp))

instance KnownNat n => Enum (Face n) where
  toEnum k = Face (finite (fromIntegral (div k 2)) , mod k 2 == 1)
  fromEnum (Face (i , b)) = fromIntegral (getFinite i) * 2 + (if b then 1 else 0)

instance (KnownNat n , HasCard (a n) , Enum (a n)) => Bounded (a n) where
  minBound = toEnum 0
  maxBound = result
    where
      result = toEnum (fromIntegral $ cardinality result - 1)


class Listable a where
  listAll :: [a]


instance (Enum a , Bounded a) => Listable a where
  listAll = [minBound .. maxBound]


class SubFaceal a n | a -> n where
  toSubFace :: a -> SubFace n

instance  SubFaceal (SubFace n) n where
  toSubFace = id

instance  SubFaceal (Face n) n where
  toSubFace (Face (i , b)) = SubFace (Map.singleton i b)

-- -- instance  (KnownNat n) => SubFaceal (Subset n) n where
-- --   toSubFace (Subset (Vecc l)) = SubFace (Map.fromList (zip finites l))       

-- -- instance (KnownNat n) => Lattice (SubFace n) where
-- --   (SubFace x) \/ (SubFace y) = undefined
-- --   (SubFace x) /\ (SubFace y) = undefined


allF3 :: [ Face 3 ]
allF3 = listAll


ttt = fmap toSubFace allF3

grTest :: Graded (SubFace) 3 2
grTest = mkGraded (toSubFace (head allF3))


data SSubFace n (sf :: SubFace n) = SSubFace (SubFace n)

instance SubFaceal (SSubFace n sf) n where
  toSubFace (SSubFace x) = x 

data SSubFace' n (sf :: SubFace n) = forall m. SSubFace' (SubFace m)

-- instance SubFaceal (SSubFace' n sf) n where
--   toSubFace (SSubFace' (SubFace x)) = SubFace x 




zz :: KnownNat n => Vecc Bool n -> Vecc Bool n -> [Bool]
zz (Vecc a) (Vecc b) = a ++ b
