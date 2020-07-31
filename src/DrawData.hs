module DrawData where

import Combi

import Syntax

import qualified Data.Map as Map

import Data.Maybe

data CSet = CSet (Inside) |  Degen Int (Inside)

-- for each corner uniq Id, and "color" id
-- where color means id of the corner term

type Inside = (Int , Subset -> (Int , Int))

type DContext = Map.Map Int Inside


pickCornerOfExpr :: Context -> Subset -> Expr -> Int
pickCornerOfExpr = undefined

getCornerVarId :: Context -> Subset -> Int -> Int
getCornerVarId = undefined

mapIncr :: Int -> Map.Map Int Int -> (Map.Map Int Int , Int)
mapIncr k m = (Map.alter undefined k m , maybe 0 id $ Map.lookup k m)
  where
    f :: (Maybe Int) -> (Maybe Int)
    f = Just . (maybe (1) (\x -> x + 1)) 


mkDContext :: (Env , Context) -> DContext
mkDContext (env , Context l _) =
  snd $ snd (foldr f (0 , (Map.empty , Map.empty)) l )

  where
    f ::  (String,CType)
          -> (Int , ((Map.Map Int Int) , (Map.Map Int Inside)))
           -> (Int , ((Map.Map Int Int) , (Map.Map Int Inside))) 
    f (_ , CType _ []) (k , (m1 , m2)) =
         let (nm1 , j) = mapIncr 0 m1
         in (k + 1 , ( nm1 , Map.insert k (0 , const (j , _) ) m2))
