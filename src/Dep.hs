{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE AllowAmbiguousTypes #-}


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
  
data OfDim a = forall n. KnownNat n => OfDim (a n)

getDim :: OfDim a -> Integer
getDim (OfDim x) = natVal x

-- toVecc :: [a] -> OfDim (Vecc a)
-- toVecc la = OfDim undefined

newtype GradedMap a b (n :: Nat) = GradedMap (Map.Map (a n) (OfDim b))

mkGradedMap :: (KnownNat n , Gradable a) => Map.Map (a n) (OfDim b) -> GradedMap a b n
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

