module DataExtra where

import Data.Bifunctor

import Data.List

tpl2arr (x , y) = [x , y]
trpl2arr (x , y , z) = [x , y , z]

range k = take k [0,1..]


explode :: [a] -> [[a]]
explode [] = []
explode [a] = [[]]
explode l =
  let z = zip [0..] l
  in fmap (\(i , _) -> fmap snd $ filter (\(j , _) -> j /= i) z ) z


listInsert :: Int -> a -> [a] -> [a]
listInsert 0 a l = a : l
listInsert _ a [] = [a]
listInsert k a (x : xs) = x : listInsert (k - 1) a xs 

listRemove :: Int -> [a] -> [a]
listRemove _ [] = error "bad index or empty list"
listRemove 0 (x : xs) = xs
listRemove k (x : xs) = x : listRemove (k - 1) ( xs)

listPopAt :: Int -> [a] -> (a , [a])
listPopAt _ [] = error "bad index or empty list"
listPopAt 0 (x : xs) = (x , xs) 
listPopAt k (x : xs) = second ((:) x) $ listPopAt (k - 1) ( xs)

transposeTuples ((a , b) , (c , d)) = ((a , c) , (b , d))

mapBoth f = bimap f f
