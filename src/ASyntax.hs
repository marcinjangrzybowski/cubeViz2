{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ASyntax where


import Data.List

-- import qualified Data.Tuple.Extra as DTE

import Data.Set as Set
import Data.Map as Map
-- import Data.Bifunctor
-- import Data.List.Split
-- import Data.Maybe
-- import Data.Bool
-- import Data.Either
-- import Data.Function
-- import qualified Data.List.NonEmpty as Ne

-- import qualified Text.Read as TR

-- import DataExtra

-- import Debug.Trace

-- import Combi

-- import SyntaxIExpr




data IExpr = IExpr (Set.Set ( Set.Set (Int , Bool)))
  deriving (Eq , Show , Read)

data VarIndex = VarIndex Int
  deriving (Eq , Show, Ord , Read)

type Partial = Map.Map (Map.Map Int Bool) OExpr

data ClExpr = ClExpr Int (Map.Map (Map.Map Int Bool) OExpr)
  deriving (Eq , Show , Read)

data ConstExpr = ConstExpr String 
  deriving (Eq , Show , Read)

data CellExpr = CellExpr VarIndex [IExpr]
  deriving (Eq , Show , Read)

data OExpr =
    HComp (Maybe String) Partial ClExpr
  | Cell CellExpr
  | ConstE ConstExpr
  | CHole Int
  | Appl ClExpr [ClExpr]  
  deriving (Eq , Show , Read)


data Visualisation =
    Visualisation ClExpr ([ (String,(String,Maybe [String]))])  
 deriving (Show,Read)


-- assocTest :: ClExpr
-- assocTest = read "ClExpr 2 (fromList [(fromList [],HComp Nothing (fromList [(fromList [(0,False),(1,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(0,False),(1,True)],HComp Nothing (fromList [(fromList [(0,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 1) [IExpr (fromList [fromList [(0,True)]])]))]) (ClExpr 1 (fromList [(fromList [],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,False)],Cell (CellExpr (VarIndex 5) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 4) []))]))),(fromList [(0,True),(1,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(0,True),(1,True)],Cell (CellExpr (VarIndex 1) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(1,False)],Cell (CellExpr (VarIndex 8) [])),(fromList [(1,True)],HComp Nothing (fromList [(fromList [(0,False),(1,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,False),(1,True)],Cell (CellExpr (VarIndex 1) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,True)],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(0,True),(1,True)]])])),(fromList [(0,True),(1,False)],Cell (CellExpr (VarIndex 5) [])),(fromList [(0,True),(1,True)],Cell (CellExpr (VarIndex 1) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(1,False)],Cell (CellExpr (VarIndex 3) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(1,True)],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(1,True)]])]))]) (ClExpr 2 (fromList [(fromList [],Cell (CellExpr (VarIndex 3) [IExpr (fromList [fromList [(0,True)],fromList [(1,True)]])])),(fromList [(0,False)],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,False),(1,False)],Cell (CellExpr (VarIndex 5) [])),(fromList [(0,False),(1,True)],Cell (CellExpr (VarIndex 4) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 5) [])),(fromList [(0,True),(1,False)],Cell (CellExpr (VarIndex 4) [])),(fromList [(0,True),(1,True)],Cell (CellExpr (VarIndex 4) [])),(fromList [(1,False)],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(1,True)],Cell (CellExpr (VarIndex 5) []))])))]) (ClExpr 2 (fromList [(fromList [],HComp Nothing (fromList [(fromList [(0,False)],Cell (CellExpr (VarIndex 4) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,False),(1,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(0,False),(1,True)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True),(1,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(0,True),(1,True)],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(1,False)],Cell (CellExpr (VarIndex 8) [])),(fromList [(1,True)],Cell (CellExpr (VarIndex 3) [IExpr (fromList [fromList [(0,True),(1,True)]])]))]) (ClExpr 2 (fromList [(fromList [],Cell (CellExpr (VarIndex 4) [IExpr (fromList [fromList [(1,True)]])])),(fromList [(0,False)],Cell (CellExpr (VarIndex 3) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,False),(1,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,False),(1,True)],Cell (CellExpr (VarIndex 5) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 3) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,True),(1,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True),(1,True)],Cell (CellExpr (VarIndex 5) [])),(fromList [(1,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(1,True)],Cell (CellExpr (VarIndex 6) []))]))),(fromList [(0,False)],Cell (CellExpr (VarIndex 3) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,False),(1,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,False),(1,True)],Cell (CellExpr (VarIndex 5) [])),(fromList [(0,True)],HComp Nothing (fromList [(fromList [(0,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(0,True)]])]))]) (ClExpr 1 (fromList [(fromList [],Cell (CellExpr (VarIndex 3) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 5) []))]))),(fromList [(0,True),(1,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True),(1,True)],Cell (CellExpr (VarIndex 4) [])),(fromList [(1,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(1,True)],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(0,True)]])]))]))),(fromList [(0,False)],HComp Nothing (fromList [(fromList [(0,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(0,True)],HComp Nothing (fromList [(fromList [(0,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 1) [IExpr (fromList [fromList [(0,True)]])]))]) (ClExpr 1 (fromList [(fromList [],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,False)],Cell (CellExpr (VarIndex 5) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 4) []))])))]) (ClExpr 1 (fromList [(fromList [],Cell (CellExpr (VarIndex 3) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 5) []))]))),(fromList [(0,False),(1,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,False),(1,True)],Cell (CellExpr (VarIndex 3) [])),(fromList [(0,True)],HComp Nothing (fromList [(fromList [(0,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 1) [IExpr (fromList [fromList [(0,True)]])]))]) (ClExpr 1 (fromList [(fromList [],HComp Nothing (fromList [(fromList [(0,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 2) [IExpr (fromList [fromList [(0,True)]])]))]) (ClExpr 1 (fromList [(fromList [],Cell (CellExpr (VarIndex 3) [IExpr (fromList [fromList [(0,True)]])])),(fromList [(0,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 5) []))]))),(fromList [(0,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True)],Cell (CellExpr (VarIndex 4) []))]))),(fromList [(0,True),(1,False)],Cell (CellExpr (VarIndex 6) [])),(fromList [(0,True),(1,True)],Cell (CellExpr (VarIndex 3) [])),(fromList [(1,False)],Cell (CellExpr (VarIndex 7) [])),(fromList [(1,True)],Cell (CellExpr (VarIndex 4) []))])"
