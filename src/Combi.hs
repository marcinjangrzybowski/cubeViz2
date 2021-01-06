{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}



{-# LANGUAGE FlexibleInstances #-}


{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Combi where

-- import Data.Permute

import Data.List

import Data.Foldable

import qualified Data.Set as Set

import qualified Data.Map as Map

import Data.Tuple
import Data.Function
import Data.Maybe

import qualified Data.Bifunctor as Bf

import Data.Bool

import DataExtra

import Data.Semigroup

import Debug.Trace

import Algebra.Lattice


factorial :: Int -> Int
factorial n =
  case n of
    0 -> 1
    1 -> 1
    2 -> 2
    3 -> 6
    4 -> 24
    5 -> 120
    _ -> n * factorial (n - 1)

newtype Permutation = Permutation (Map.Map Int Int)
  deriving (Ord , Eq)

instance Show Permutation where
  show (Permutation x) = show $ toList x

newtype Permutation2 = Permutation2 [Int]

data Subset = Subset Int (Set.Set Int)
  deriving (Ord , Eq)

data Face = Face Int (Int , Bool)
  deriving (Ord , Eq)

instance Show Face where
  show (Face _ x) = show x




data SubFace = SubFace Int (Map.Map Int Bool)
  deriving (Eq , Ord)


sfMissing :: SubFace -> Subset
sfMissing (SubFace n m) = Subset n (Set.difference (Set.fromList (range n)) (Map.keysSet m))

toFace :: SubFace -> Maybe Face
toFace (SubFace n sfm) =
  case Map.toList sfm of
    [ x ] -> Just (Face n x)
    _ -> Nothing

toSubFace :: Face -> SubFace
toSubFace (Face n fc) = SubFace n (Map.fromList [fc])

fullSF :: Int -> SubFace
fullSF = flip SubFace Map.empty

isFullSF :: SubFace -> Bool
isFullSF (SubFace _ m) = Map.null m

sf2map :: SubFace -> Map.Map Int Bool
sf2map (SubFace _ x) = x

subFaceCodim :: SubFace -> Int
subFaceCodim (SubFace _ m) = Map.size m

subFaceDimEmb :: SubFace -> Int
subFaceDimEmb (SubFace n m) = n - Map.size m

data Never a = Never



class OfDim a where
  getDim :: a -> Int

  checkDim :: Int -> a -> Maybe a
  checkDim i a =
    if getDim a == i
    then Just a
    else Nothing

forget :: a -> Never a
forget _ = Never



class Ord a => ListInterpretable a b | a -> b where
  cardLI :: Never a -> Int -> Int
  enumerate :: Int -> Int -> a
  unemerate :: a -> Int
  toListLI :: a -> [b]
  fromListLI :: [b] -> a

  sizeLI :: a -> Int
  sizeLI = length . toListLI

  genAllLIHelp :: Never a -> Int -> [a]
  genAllLIHelp a n = map (enumerate n) (range (cardLI a n))

  genAllLI :: Int -> [a]
  genAllLI = genAllLIHelp Never

  -- checking in enumerate and unemerate are each others inverses
  checkUnEn :: Never a -> Int -> Bool
  checkUnEn na n = map unemerate (genAllLIHelp na n) == range (cardLI na n)

  cardLi :: a -> Int
  cardLi a = cardLI (forget a) $ sizeLI a

  mapOnAllLI :: (a -> d -> c) -> FromLI a d -> [c]
  mapOnAllLI g (FromLI n f) = map (\x -> g x (f x)) (genAllLI n)

  project :: Int -> a -> a
  project k = fromListLI . listRemove k . toListLI

  projectSplit :: Int -> a -> (b , a)
  projectSplit k a = Bf.second fromListLI (listPopAt k (toListLI a))

  setOfAll :: Int -> Set.Set a
  setOfAll = Set.fromList . genAllLI

  rotateLI :: Int -> a -> a
  rotateLI k a = enumerate (sizeLI a) $ mod (unemerate a + k) (cardLi a)

instance OfDim SubFace where
  getDim = sizeLI

instance OfDim Face where
  getDim = sizeLI

instance OfDim Subset where
  getDim = sizeLI

instance OfDim Permutation where
  getDim = sizeLI

instance OfDim Piece where
  getDim = sizeLI

-- instance OfDim (Face) where
--   getDim = sizeLI

mapListIndexed :: (Int -> a -> b) -> [a] -> [b]
mapListIndexed f = zipWith f [0..]


listPermute2 :: Permutation2 -> [a] -> [a]
listPermute2 (Permutation2 pm2) = foldr (uncurry listInsert) [] . zip pm2

listPermute :: Permutation -> [a] -> [a]
listPermute = (\ ks xs -> fmap (xs !!) ks) . toListLI


updateAt :: a -> Int -> [a] ->   [a]
updateAt _ _ [] = []
updateAt a 0 (_ : xs) = a : xs
updateAt a k (x : xs) = x : updateAt a (k - 1) xs

invPerm :: Permutation -> Permutation
invPerm (Permutation m) = Permutation ( Map.fromList $ swap <$> Map.toList m)

instance ListInterpretable Permutation Int where
  cardLI _ = factorial

  enumerate n i = Permutation $ Map.fromList $ zip (range n) $ listPermute2 (Permutation2 (f n i)) (range n)
    where
      f :: Int -> Int -> [Int]
      f n k =
         if n > 0
         then mod k n : f (n - 1) (k `div` n)
         else []


  unemerate (Permutation l) = f $ g (toList l)
    where
      f :: [Int] -> Int
      f [] = 0
      f (x : xs) = x + (length (x : xs) * f xs)

      g :: [Int] -> [Int]
      g [] = []
      g l =
        let k = fromMaybe 0 (elemIndex 0 l)
        in k : g (map (flip (-) 1) (delete 0 l))

  toListLI (Permutation p) = map snd $ Map.toList p

  fromListLI = Permutation . Map.fromList . mapListIndexed (,)


  projectSplit k a =
    let (j , l) = listPopAt k (toListLI a)
    in (j , fromListLI $ fmap (\x -> if x >= j then x - 1 else x) l)

  project k = snd . projectSplit k

instance ListInterpretable Subset Bool where
  cardLI _ n = 2 ^ n

  enumerate n = fromListLI . f n
    where
      f :: Int -> Int -> [Bool]
      f 0 _ = []
      f n k = (mod k 2 == 1) : f (n - 1) (k `div` 2)

  unemerate = f . toListLI
    where
      f :: [Bool] -> Int
      f [] = 0
      f (False : l) = 2 * f l
      f (True : l) = 1 + 2 * f l


  toListLI (Subset n s) = map (`Set.member` s) (range n)

  fromListLI l = Subset (length l) $ Set.fromList $ findIndices id l

instance ListInterpretable SubFace (Maybe Bool) where
  cardLI _ n = 3 ^ n

  enumerate n = fromListLI . f n
    where
      f :: Int -> Int -> [Maybe Bool]
      f 0 _ = []
      f n k =
        (case mod k 3 of
           0 -> Nothing
           i -> Just (i == 2)
        ) : f (n - 1) (k `div` 3)

  unemerate = f . toListLI
    where
      f :: [Maybe Bool] -> Int
      f [] = 0
      f (Nothing : l) = 3 * f l
      f ((Just False) : l) = 1 + 3 * f l
      f ((Just True) : l) = 2 + 3 * f l


  toListLI (SubFace n s) = map (`Map.lookup` s) (range n)

  fromListLI l = SubFace (length l) $ Map.fromList $ mapMaybe (\(i , x) -> fmap (i,) x) (zip [0..] l)

instance Show SubFace where
  show sf =
    intercalate "|"
    $ zipWith (curry (uncurry (++) . Bf.bimap show (maybe "_" (bool "-" "+")))) [0..] (toListLI sf)
    -- concat
    -- $ map (uncurry (++))
    -- $ map (Bf.bimap show (bool "-" "+")) $ Map.toList x


instance Show Subset where
  show = concatMap (\b -> if b then "-" else "+") . toListLI


type Piece
  = (Subset , Permutation)

instance ListInterpretable Piece (Bool , Int) where
  cardLI _ n = cardLI (Never :: (Never Subset)) n * cardLI (Never :: (Never Permutation)) n

  enumerate n i =
    let cardSu = cardLI (Never :: (Never Subset)) n
        iSu = mod i cardSu
        iPm = i `div` cardSu
    in (enumerate n iSu , enumerate n iPm)

  unemerate (su , pm) =
    unemerate su + (unemerate pm * cardLi su)

  toListLI (a1 , a2) = zip (toListLI a1) (toListLI a2)

  fromListLI = Bf.bimap fromListLI fromListLI . unzip


  project k = Bf.bimap (project k) (project k)

  projectSplit k = transposeTuples . Bf.bimap (projectSplit k) (projectSplit k)

instance ListInterpretable Face (Maybe Bool) where

  cardLI _ n = 2 * n

  genAllLI n = map (Face n) $ concatMap (\i -> [(i,False) , (i,True)]) (range n)

  enumerate n i = Face n $ Bf.second (1 ==) $ divMod i 2

  unemerate (Face _ (k , b)) = k*2 + bool 0 1 b

  toListLI (Face n (i,b)) = map ( bool Nothing (Just b) . (i ==) ) $ range n

  fromListLI l =
    let i = fromJust $ findIndex isJust l
    in Face (length l) (i , head $ catMaybes l)



data FromLI a c = FromLI Int (a -> c)

instance OfDim (FromLI a c) where
  getDim (FromLI n _) = n

instance (ListInterpretable a b , Semigroup c) => Semigroup (FromLI a c) where
 (<>) (FromLI i f) (FromLI j g) =
    if i == j
    then FromLI i (\x -> f x <> g x)
    else error "unable to concatenate FromLI of diferent dimensions"


evalLI :: (ListInterpretable a b , Monoid c) =>
               FromLI a (FromLI a c) -> c
evalLI = foldl (<>) mempty . toListFLI . fromLIppK (\ a (FromLI _ f) -> f a)

appLI :: (ListInterpretable a b) => a -> FromLI a c -> c
appLI a (FromLI n f) =
  if sizeLI a == n
  then f a
  else error $ "argument of wrong dimension! " ++ show (sizeLI a) ++ " expected:"  ++ show n



fromLIppK :: (a -> c -> b) -> FromLI a c -> FromLI a b
fromLIppK f (FromLI n g) = FromLI n (\x -> f x $ g x)

toListFLI :: ListInterpretable a b =>  FromLI a c -> [c]
toListFLI (FromLI n g) = map g $ genAllLI n

fromListFLI :: ListInterpretable a b => Int -> [c] -> FromLI a c
fromListFLI n l = FromLI n ((!!) l . unemerate)


toMapFLI :: ListInterpretable a b =>  FromLI a c -> Map.Map a c
toMapFLI (FromLI n g) = Map.fromList $ map (\x -> (x , g x) ) $ genAllLI n

fromMapFLI :: ListInterpretable a b => Int -> Map.Map a c -> FromLI a (Maybe c)
fromMapFLI n = FromLI n . flip Map.lookup

keysSetMapFLI :: ListInterpretable a b => FromLI a (Maybe c) -> Set.Set a
keysSetMapFLI = Map.keysSet . Map.filter isJust . toMapFLI


fromMapFLIUnsafe :: ListInterpretable a b => Int -> Map.Map a c -> FromLI a c
fromMapFLIUnsafe n = fmap fromJust . fromMapFLI n


mkFaceLI :: [(a , a)] -> FromLI Face a
mkFaceLI l = FromLI (length l) (\(Face _ (k , b)) -> pickFromPair b (l !! k))

instance Functor (FromLI a) where
  fmap f (FromLI n g) = FromLI n (f . g)

instance (Show c ,  ListInterpretable a b) => (Show (FromLI a c)) where
  show x = unwords . map show $ toList x

showMbFLI :: (Show c ,  Show a , ListInterpretable a b) =>
               FromLI a (Maybe c) -> String
showMbFLI = intercalate " ; " . map (\(k , v) -> show k ++ "->" ++ show v) . filter (isJust . snd)   . Map.toList . toMapFLI

instance (ListInterpretable a b) => Foldable (FromLI a) where
  foldMap f = foldMap f . toListFLI

instance (ListInterpretable a b) => Traversable (FromLI a) where
  traverse f x@(FromLI n g) = fromMapFLIUnsafe n <$> traverse f (toMapFLI x)

traverseMapLI :: (Applicative f , ListInterpretable a b)  =>
         ( a -> v -> f w) -> FromLI a v -> f (FromLI a w)
traverseMapLI f x@(FromLI n g) = fmap (fromMapFLIUnsafe n) $ traverseMap f $ toMapFLI x

traverseMap :: (Ord k , Applicative f) =>
         ( k -> v -> f w) -> Map.Map k v -> f (Map.Map k w)
traverseMap f x =
   Map.fromList <$> traverse ff (Map.toList x)

   where
     ff (k , v) = (k,) <$> f k v



traverseMapAndKeys :: (Ord k , Ord l , Applicative f) =>
         ( (k , v) -> f (l , w)) -> Map.Map k v -> f (Map.Map l w)
traverseMapAndKeys f x =
   Map.fromList <$> traverse f (Map.toList x)

whereJust :: [Maybe a] -> [(Int, a)]
whereJust = fmap fromJust . filter isJust . zipWith (fmap . (,) ) [0..]





--- possible isoin  cubicla agda, by some quotient
intoRuns :: [ Either a b ] -> [ Either [a] [b] ]
intoRuns = foldr (flip f) []
  where
    f :: [ Either [a] [b] ] -> Either a b -> [ Either [a] [b] ]
    f [] z = pure $ Bf.bimap pure pure z
    f (x : xs) z =
      case (z , x) of
        (Left zz , Left ys) -> Left (zz : ys) : xs
        (Right zz , Right ys) -> Right (zz : ys) : xs
        _ ->  Bf.bimap pure pure z : (x : xs)

fromRuns :: [ Either [b] [c] ] ->  [ Either b c ]
fromRuns = concatMap (either (fmap Left) (fmap Right))


-- about families of sets

-- TODO : type of hereditary sets, and maybe tyoe of hereditary maps

-- this removes Subsets that are BIG
makeAntiH :: Ord a => Set.Set (Set.Set a) -> Set.Set (Set.Set a)
makeAntiH y = Set.filter (\x -> not (any (`Set.isProperSubsetOf` x) y)) y

makeAntiHKeys :: (Ord a , Ord b) => Map.Map (Map.Map a b) c -> Map.Map (Map.Map a b) c
makeAntiHKeys y =
  let allKeysAsSets = Set.map mapToSet (Map.keysSet y)
  in Map.filterWithKey
         (\ x _ ->
              not (any (`Set.isProperSubsetOf` mapToSet x) allKeysAsSets)) y

-- this removes Subsets that are SMALL
makeAntiH2 :: Ord a => Set.Set (Set.Set a) -> Set.Set (Set.Set a)
makeAntiH2 y = Set.filter (\x -> not (any (Set.isProperSubsetOf x) y)) y



--- subface specyfic code

-- works only for SFacesd of codim >= 1 !
unconsSF :: SubFace -> (Face , SubFace)
unconsSF (SubFace n mp) =
  case Map.toList mp of
    [] -> error "subFace of codim 0!"
    x : xs -> (Face n x , SubFace (n - 1) ( Map.fromList (map (Bf.first (\x -> x - 1)) xs)))


elimSubFaceSide :: FromLI SubFace a -> FromLI SubFace a -> FromLI SubFace a -> (SubFace -> a)
elimSubFaceSide _ _ f@(FromLI n fli) sf@(SubFace m sfm) | m /= n + 1 = error "not matching dimensions (cyl)"
elimSubFaceSide f@(FromLI n fli) _ _ sf@(SubFace m sfm) | m /= n + 1 = error "not matching dimensions (bottom)"
elimSubFaceSide _ f@(FromLI n fli) _ sf@(SubFace m sfm) | m /= n + 1 = error "not matching dimensions (top)"
elimSubFaceSide f0 f1 f sf@(SubFace m sfm) =
  case Map.lookup (m - 1) sfm of
    Just b -> appLI (SubFace (m - 1) (Map.delete (m - 1) sfm)) (if b then f1 else f0)
    Nothing -> appLI (SubFace (m - 1) sfm) f

elimSubFaceSideLI :: FromLI SubFace a -> FromLI SubFace a -> FromLI SubFace a -> FromLI SubFace a
elimSubFaceSideLI f0 f1 f@(FromLI n fli) = FromLI (n + 1) (elimSubFaceSide f0 f1 f)

injFace :: SubFace -> Face ->  SubFace
injFace sf@(SubFace n m) f@(Face _ (k , b))
             | getDim f /= subFaceDimEmb sf = error "face dimension do not match subfaceEmbdDim"
             | otherwise =
                let k2 = punchInMany (Map.keysSet m) k
                in SubFace n (Map.insert k2 b m)

injSubFaceSafe :: SubFace -> SubFace -> Maybe SubFace
injSubFaceSafe sfL@(SubFace k sfLm) sf@(SubFace n m)
   | getDim sfL /= subFaceDimEmb sf = Nothing
   | otherwise =
       let sfLm2 = Map.mapKeys (punchInMany (Map.keysSet m)) sfLm
                in Just $ SubFace n (Map.union sfLm2 m)


injSubFace :: SubFace -> SubFace -> SubFace
injSubFace sfL@(SubFace k sfLm) sf@(SubFace n m)
   | getDim sfL /= subFaceDimEmb sf = error $ "subface dimension do not match subfaceEmbdDim: "
                                              ++ show (getDim sfL) ++ " vs " ++ show (subFaceDimEmb sf)
   | otherwise =
       let sfLm2 = Map.mapKeys (punchInMany (Map.keysSet m)) sfLm
                in SubFace n (Map.union sfLm2 m)

-- injSubFaceTest =  


-- takes

faceOfCylAtSide :: Bool -> SubFace -> SubFace
faceOfCylAtSide b sf = toSubFace (Face ((subFaceDimEmb sf) + 1) (subFaceDimEmb sf , b))


injCylEnd :: Bool -> SubFace -> SubFace
injCylEnd b (SubFace n mp) = (SubFace (n + 1) (Map.insert n b mp))

injCyl :: SubFace -> SubFace
injCyl (SubFace n mp) = (SubFace (n + 1) mp)

jniCyl' :: SubFace -> Maybe SubFace
jniCyl' (SubFace n mp)
  | Map.member (n - 1) mp = Nothing
  | otherwise = Just (SubFace (n - 1) mp)
                       
  

data JniCyl = JCEnd Bool SubFace | JCCyl SubFace

data DegenCase = DCEnd Bool SubFace | DCIns SubFace

degenElim :: Int -> SubFace -> DegenCase
degenElim k (SubFace n mp) =
  case Map.lookup (k) mp of
    Just b -> DCEnd b (SubFace (n - 1) $ Map.mapKeys (fromJust . punchOut k ) (Map.delete (k) mp))
    Nothing -> DCIns (SubFace (n - 1) $ Map.mapKeys (fromJust . punchOut k ) mp)


superSubFaces :: SubFace -> [SubFace]
superSubFaces sf@(SubFace m sfm) =
  [ (SubFace m (Map.fromList sfml))

    | sfml <- explode (Map.toList sfm) ]
  
    

allSuperSubFaces :: SubFace -> [SubFace]
allSuperSubFaces sf@(SubFace m sfm) =
  [ (SubFace m (Map.fromList (Set.toList sfml)))
    | sfml <- Set.toList (Set.powerSet $ Set.fromList (Map.toList sfm)) ]



-- second is Parent
jniCyl :: SubFace -> SubFace -> JniCyl
jniCyl (SubFace m sfm) sf'@(SubFace m' sfm') =
  case Map.lookup (m - 1) sfm of
    Just b -> JCEnd b (injSubFace (SubFace (m - 1) (Map.delete (m - 1) sfm)) sf')
    Nothing -> JCCyl (injSubFace (SubFace (m - 1) sfm) sf')

jniSubFaceMb :: SubFace -> SubFace -> Maybe SubFace
jniSubFaceMb sfL@(SubFace k sfLm) sf@(SubFace n m)
   | getDim sfL /= getDim sf = Nothing
   | not $ isSubFaceOf sfL sf = Nothing
   | otherwise =
       let sfLm2 =   Map.fromList
                   $ map (Bf.first fromJust)
                   $ filter (isJust . fst)
                   $ map (Bf.first $ punchOutMany (Map.keysSet m))
                   $ Map.toList sfLm
                in Just $ SubFace (subFaceDimEmb sf) sfLm2



jniSubFace :: SubFace -> SubFace -> SubFace
jniSubFace sfL@(SubFace k sfLm) sf@(SubFace n m)
   | getDim sfL /= getDim sf = error "subface dimension do not match"
   | not $ isSubFaceOf sfL sf = error "not a subface!"
   | otherwise =
       let sfLm2 =   Map.fromList
                   $ map (Bf.first fromJust)
                   $ filter (isJust . fst)
                   $ map (Bf.first $ punchOutMany (Map.keysSet m))
                   $ Map.toList sfLm
                in SubFace (subFaceDimEmb sf) sfLm2

isSubFaceOf :: SubFace -> SubFace -> Bool
isSubFaceOf (SubFace n1 m1) (SubFace n2 m2)
   | n1 /= n2 = error "cannot comapre subfaces od diferent dimension!"
   | otherwise = Map.isSubmapOf m2 m1

isProperSubFaceOf :: SubFace -> SubFace -> Bool
isProperSubFaceOf (SubFace n1 m1) (SubFace n2 m2)
   | n1 /= n2 = error "cannot comapre subfaces od diferent dimension!"
   | otherwise = Map.isProperSubmapOf m2 m1


isIncludedIn :: Set.Set SubFace -> SubFace -> Bool
isIncludedIn sfcs sf = any (isSubFaceOf sf) sfcs

isProperlyIncludedIn :: Set.Set SubFace -> SubFace -> Bool
isProperlyIncludedIn sfcs sf = isIncludedIn sfcs sf && not (Set.member sf sfcs)

isCoveredIn :: Set.Set SubFace -> SubFace -> Bool
isCoveredIn sfcs sf = any (isSubFaceOf sf) (Set.delete sf sfcs)



injFaceSide :: Face -> Face
injFaceSide (Face n (i , b)) = Face (n + 1) (i , b)

deleteButLeaveFaces :: (Face -> a -> a) -> SubFace -> Map.Map SubFace a -> Map.Map SubFace a
deleteButLeaveFaces fcs sf@(SubFace n _) m =
  let keys = Map.keysSet m

  in case Map.lookup sf m of
        Nothing -> m
        Just toDelete ->
          let shouldBeIncluded sfF =
                 not $ isIncludedIn (Set.delete sf keys) sfF
              toAdd =
                  Map.fromList
                $ filter (shouldBeIncluded . fst)
                $ map (\f -> (injFace sf f ,  fcs f toDelete))
                $ genAllLI (subFaceDimEmb sf)
              u = Map.union (Map.delete sf m) toAdd
          in --trace ((show $ (Map.keysSet (Map.delete sf m) )) ++ (show $ Map.keysSet toAdd))
             --trace ((show $ (Map.keysSet u )))
             u


missingSubFaces :: Int -> Set.Set SubFace -> Set.Set SubFace
missingSubFaces n sfcs =
   Set.filter (\x -> not (isIncludedIn sfcs x) && not (isFullSF x) ) $ Set.fromList (genAllLI n)


properlyCoveredSubFaces :: Int -> Set.Set SubFace -> Set.Set SubFace
properlyCoveredSubFaces n sfcs  =
   Set.filter (\x -> isProperlyIncludedIn sfcs x && not (isFullSF x) ) $ Set.fromList (genAllLI n)


getCenter :: FromLI SubFace a -> a
getCenter (FromLI n f) = f (fullSF n)


-- TODO : make it safer
addSubFace :: a -> SubFace -> Map.Map SubFace a -> Map.Map SubFace a
addSubFace a sf@(SubFace n _) m =
      Map.mapKeys (SubFace n)
    $ makeAntiHKeys
    $ Map.mapKeys (\(SubFace _ ma) -> ma)
    $ Map.insert sf a m
  -- let keys = Map.keysSet m

  -- in if not $ Set.member sf keys
  --    then m
  --    else           
  --         let shouldBeIncluded sfF =
  --                not $ isIncludedIn (Set.delete sf keys) sfF 
  --             toAdd =
  --                 Map.fromList
  --               $ filter (shouldBeIncluded . fst)
  --               $ map (\f -> (injFace sf f ,  fcs f))
  --               $ genAllLI (subFaceDimEmb sf)
  --             u = Map.union (Map.delete sf m) toAdd 
  --         in --trace ((show $ (Map.keysSet (Map.delete sf m) )) ++ (show $ Map.keysSet toAdd))
  --            --trace ((show $ (Map.keysSet u )))
  --            u



superFacesOutside :: Set.Set SubFace -> SubFace -> Set.Set SubFace
superFacesOutside sfs sf =
  let superSF = filter (not . isFullSF) $ allSuperSubFaces sf
  in Set.difference (Set.fromList superSF) sfs
    
-- data BdSfCase = OnBd (Set.Set SubFace) | Inside | Outside




-- -- set must be walid "boudnary set"
-- partialBdCase :: Set.Set SubFace -> SubFace -> BdSfCase
-- partialBdCase sfs sf =
--   let superSF = filter (not . isFullSF) $ allSuperSubFaces sf in
--   if (not (Set.member sf sfs))
--   then Outside
--   else if (all (flip Set.member sfs ) superSF)
--        then Inside
--        else OnBd $ Set.difference (Set.fromList superSF) sfs
