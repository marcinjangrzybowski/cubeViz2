{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module DrawExpr where

import Abstract

import Syntax

import Drawing.Base
import Drawing.Color

import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map

import Control.Arrow

import Combi

import FloatTools

import Data.Bool
import Data.List
import qualified Data.Set as Set

import Data.Tuple.Extra

import PiecesEval

import Reorientable

defaultCompPar = 0.3      

hcompDrawingsOnlyFaces :: (Drawing a) -> (Map.Map Face (Drawing a)) -> Drawing a 
hcompDrawingsOnlyFaces bot sides =
   concat
    ((sMap (map $ centerTransInv defaultCompPar) bot) :
      (map snd
       $ Map.toList
       $ Map.mapWithKey (
          \fc@(Face n (i , b)) ->
             
              (sMap
             $ ambFnOnArr
             $ (sideTransInv defaultCompPar True) fc)
             . transposeDrw i
          ) sides))

subFaceTrans :: SubFace -> Drawing a -> Drawing a
subFaceTrans sf@(SubFace n m) drw =
  case (Map.toList m) of
    [] -> mempty
    -- _ : [] -> emptyDrawing 
    (i , b)  : js ->
      let augmentedWithMissingTail =
             foldl (flip (\(i , b) -> embed (i - 1) (const $ if b then 1.0 else 0.0)))
               (drw) js

          transformed = (sMap
             $ ambFnOnArr
             $ (sideTransInv defaultCompPar True) (Face n (i , b)))
             $ transposeDrw i (augmentedWithMissingTail)

          -- extruded :: Drawing a
          -- extruded = undefined 
          --   -- foldl (flip
          --   --    (\ (k , (i , b)) -> extrude (fmap ((*) (-0.1)) (versor n i)) ))
          --   --   transformed (zip [0..] js)
      in transformed

hcompDrawings :: (Drawing a)
     -> (Map.Map SubFace (Drawing a))
     -> Drawing a 
hcompDrawings bot sides =
   concat
    (
      (sMap (map $ centerTransInv defaultCompPar) (bot)) :
      (map snd
       $ Map.toList
       $ Map.mapWithKey subFaceTrans sides)
     ) 
    


collectDrawings :: Cub (Drawing b) -> (Drawing b)
collectDrawings =
  -- foldSubFaces hcompDrawings
  foldFaces hcompDrawingsOnlyFaces
  


type CellPainter b = 
       (Int -> Address -> CellExpr -> Either String (Drawing b))




class (Colorlike b , DiaDeg c) => DrawingCtx a b c | a -> b c where
  -- a is precomputed data, config, COMMON FOR ALL CELLS,
  -- c is some abstract escriptoon
  -- b is colorlike

  fromCtx :: (Env , Context) -> a

  -- IMPORTANT : (Env , Context) everywhere bellow is global! , cant be trustet with dimensions!! 

  -- HERE PIECE ARGUMENT IS ADDITIONAL!!!, for some special application!
  -- TODO :: Recover local context in cellPainter using Address
  drawGenericTerm :: (Env , Context) -> a -> Piece -> VarIndex -> c

  --Drawing b

  drawD :: Never a -> c -> ZDrawing b

  drawCellCommon :: (Env , Context) -> a -> CellExpr -> Drawing b
  drawCellCommon _ _ _ = []
  
  drawCellPiece :: (Env , Context) -> Int -> a -> PieceExpr -> (Piece -> Drawing b)  
  drawCellPiece ee n a (PieceExpr h t) =     
     (\pc -> appLI pc (remapTL (drawD (forget a)) n t $ drawGenericTerm ee a pc h))


  cellPainter :: (Env , Context) -> Never a -> a -> CellPainter b
  cellPainter ee na dctx n adr ce =
     let zz = fmap (FromLI n . (drawCellPiece ee n dctx)) (piecesEval n ce)
     in Right $
          drawCellCommon ee dctx ce
            ++
          (evalLI zz)
            

  mkDrawExpr :: Never a  -> ((Env , Context) , Expr) -> Either String (Drawing b)
  mkDrawExpr na w@( envCtx , _ ) = 
      let  dctx = fromCtx envCtx
      in
     
      ( toCub
      >>> cubMap (cellPainter envCtx na dctx )  []
      >>> fmap (collectDrawings)) w



-- XXXX


instance DrawingCtx () Color Int where    
  fromCtx _ = ()
  drawGenericTerm (env , ctx) _ _ vI = getCTyDim env ctx (getVarType ctx vI)  

  drawD _ 0 = FromLI 0 (const [([[]]  , nthColor 4) ] )
  drawD _ 1 =
    -- FromLI 1 (bool [([[0.2],[0.3]] , nthColor 1)] [([[0.6],[0.7]] , nthColor 2)] . fst . head . toListLI)

    FromLI 1 (bool [ ([[0.3]] , nthColor 1)] [ ([[1 - 0.35]] , nthColor 2)] . fst . head . toListLI)
    -- FromLI 1 (bool [([[0.2],[0.23]] , ())] [] . fst . head . toListLI)

    -- FromLI 1 (bool [([[0.2]] , ()) , ([[0.3]] , ())]
    --                [([[0.6]] , ()) , ([[0.7]] , ())  ]
    --            . fst . head . toListLI)

  drawD _ n = FromLI n (const [])
  
  drawCellCommon ee@(env , ctx) _ _ =
     let n = getDim ee
     in
       if n > 1
       then  fmap (Bf.second $ const $ gray 0.5)
               $ translate (replicate n 0.0) $ scale 1.0 $ unitHyCubeSkel n 1
       else []



--- XXXX  
 

-- instance DrawingCtx () (Color) (Maybe ((Int , Color) , [Int])) where    
--   fromCtx _ = ()

--   drawCellCommon eee _ _ = emptyDrawing
--       -- Drawing [ ( unitHyCube (getDim eee)  , gray 0.3   ) ]

--   drawGenericTerm ((ee , ctx) , _) _ pc vi =
--     if True --(unemerate pc == 28) -- && unemerate pc < 30)
--     then Just ((getCTyDim ee ctx $ getVarType ctx vi ,
--           -- nthColor (unemerate pc)
--         gray 0.8
--           ) ,  [])
--     else Nothing

--   drawD _ (Just ( (1 , cl) , dg )) =
--      let d0 = Drawing [

--           --  ( (1 , [[0.2],[0.3]])  , cl   )
--           -- , ( (1 , [[0.7],[0.8]])  , cl   )
           

           
--             ( (1 , [[0.2],[0.25]])  , cl   )
--            , ( (1 , [[0.45],[0.55]])  , cl   )
--            ,( (1 , [[0.75],[0.8]])  , cl   )
--            ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
    
--   drawD _ (Just ( (k , cl) , dg )) =
--      let d0 = translate (replicate k 0.2)  $
--            scale 0.6 $ Drawing [ ( unitHyCube (k)  , cl   ) ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
     
--   drawD _ Nothing = emptyDrawing

-- data Stripes1 = Stripes1

-- data Field1 = Field1

-- instance DrawingCtx Stripes1 (Color) (Maybe (Either Color (Color , Color) , [Int])) where    
--   fromCtx _ = Stripes1

--   drawCellCommon eee _ _ = emptyDrawing
--       -- Drawing [ ( unitHyCube (getDim eee)  , gray 0.3   ) ]

--   drawGenericTerm ((ee , ctx) , _) _ pc vi@(VarIndex viK) =
--     let (CType _ l) = (getVarType ctx vi) in
--     case l of
--       [] -> Just (Left (nthColor viK)  , [])
--       [ (Var (VarIndex viK0) [] , Var (VarIndex viK1) []) ] ->
--           Just (Right ((nthColor viK0) , (nthColor viK1) ) , [])
--       _ -> error "this convention is not suitable for types of dim>1"
--     -- Just ((getCTyDim ee ctx $ getVarType ctx vi ,
--     --       -- nthColor (unemerate pc)
--     --     gray 0.8
--     --       ) ,  [])
    

--   drawD _ (Just ( Right (c0 , c1)  , dg )) =
--      let d0 = Drawing [

--           --  ( (1 , [[0.2],[0.3]])  , cl   )
--           -- , ( (1 , [[0.7],[0.8]])  , cl   )
           

           
--             ( (1 , [[0.2],[0.25]])  , c0   )
--            -- , ( (1 , [[0.45],[0.55]])  , gray 0.5   )
--            ,( (1 , [[0.65],[0.7]])  , c1   )
--            ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
    
--   drawD _ (Just ( Left cl , dg )) =
--      let d0 = Drawing [ ( unitHyCube 0  , cl   ) ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
     
--   drawD _ Nothing = emptyDrawing

-- instance DrawingCtx Field1 (Color) ((Either Color (Color , Color) , [Int])) where    
--   fromCtx _ = Field1

--   drawCellCommon eee _ _ = emptyDrawing
--       -- Drawing [ ( unitHyCube (getDim eee)  , gray 0.3   ) ]

--   drawGenericTerm ((ee , ctx) , _) _ pc vi@(VarIndex viK) =
--     let (CType _ l) = (getVarType ctx vi) in
--     case l of
--       [] -> (Left (nthColor viK)  , [])
--       [ (Var (VarIndex viK0) [] , Var (VarIndex viK1) []) ] ->
--            (Right ((nthColor viK0) , (nthColor viK1) ) , [])
--       _ -> error "this convention is not suitable for types of dim>1"
--     -- Just ((getCTyDim ee ctx $ getVarType ctx vi ,
--     --       -- nthColor (unemerate pc)
--     --     gray 0.8
--     --       ) ,  [])
    

--   drawD _ ( ( Right (c0 , c1)  , dg0 )) =
--      let
--          k = length dg0
--          -- d0 = translate (replicate k 0.4)  $
--          --          scale 0.2 $ Drawing [ ( unitHyCube (k)  , gray 0.3   ) ]

--          d0 = grid k 3 (gray 0.4)

--          dg :: [Int]
--          dg = [ length $ takeWhile id $ zipWith (==) dg0 [0..]  ]
--      in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
    
--   drawD _ (( Left cl , dg )) = emptyDrawing
--      -- let d0 = Drawing [ ( unitHyCube 0  , cl   ) ]
--      -- in foldl (flip $ (flip addDim) (0.0001 , 0.9999)) d0 dg
     
  

drawExpr = mkDrawExpr (forget ())
  
-- drawExpr = mkDrawExpr (forget Stripes1)
          
