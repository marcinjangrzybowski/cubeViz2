module DrawExpr where

import Abstract

import Syntax

import Drawing.Base

import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map

import Control.Arrow

unTabulate :: a -> [a] -> Int -> a      
unTabulate a la i =
    case (i , la) of
        (_ , []) -> a
        (0 , _) -> a
        (_ , (x : xs)) -> unTabulate x xs (i - 1)
                        
    
ambFnOnArr :: ((Int -> a) -> (Int -> a)) -> [a] -> [a]
ambFnOnArr f l =
    case l of
        [] -> []
        (x : xs) -> map
                   (f (unTabulate x xs ))
                   (take (length l) [0,1..])   



centerW compPar = (1.0 - (compPar * 2.0))     

tanAlpha compPar = 0.5 / ( 1.0 + (hhh compPar))

interp x0 x1 t = (x0 * (1.0 - t)) + (x1 * t)

negF x = 1.0 - x

hhh compPar = ( (1.0 - 2.0 * compPar) / (2.0 * compPar) )

vertSideFixInv :: Float -> Float -> Float
vertSideFixInv compPar y =

    
    let cp = compPar
        uu = (hhh cp) * y * (tanAlpha cp)
        dd = (hhh cp) + 1.0 - y
    in         
    ((uu / dd) * (1.0 / cp))         

centerTransInv :: Float -> Float -> Float
centerTransInv compPar x = ((x - 0.5) * ((1.0 -(compPar * 2.0)))) + 0.5      

-- sideTransInv :: Float ->  Bool -> Face -> ( (Int -> Float)) -> (Int -> Float)
-- sideTransInv compPar sk (i , b) f k =
--     let
--         q = f i
--         zz = ( vertSideFixInv compPar q )    
--         qq = compPar * (negF ( zz ))    
--         z = (if b
--              then (1.0 - qq)
--              else qq)    
--     in             
--     if (k == i)
--     then (z) 
--     else
--       ( if sk
--         then (((f k) - 0.5) / (1.0 / (interp ( centerW compPar) 1.0 zz ))) + 0.5
--         else (((f k) - 0.5)) + 0.5 )

sideTransInv :: Float ->  Bool -> Face -> ( (Int -> Float)) -> (Int -> Float)
sideTransInv compPar sk (i , b0) f k =
    let
        b = b0
        q = f i
        zz = ( vertSideFixInv compPar q )    
        qq = compPar * (negF ( zz ))    
        z = (if b
             then (1.0 - qq)
             else qq)    
    in             
    if (k == i)
    then z 
    else
      ( if sk
        then (((f k) - 0.5) / (1.0 / (interp ( centerW compPar) 1.0 (zz) ))) + 0.5
        else (((f k) - 0.5)) + 0.5 )


defaultCompPar = 0.3      

hcompDrawings :: (Drawing a) -> (Map.Map Face (Drawing a)) -> Drawing a 
hcompDrawings bot sides =
   combineDrawings
    ((mapCoords (map $ centerTransInv defaultCompPar) bot) :
      (map snd $ Map.toList $ Map.mapWithKey (mapCoords . ambFnOnArr. (sideTransInv defaultCompPar True)) sides)
     ) 




-- fixSfacesIndexes :: Cub a b -> Cub a b
-- fixSfacesIndexes = 

collectDrawings :: Cub a DrawingGL -> DrawingGL
collectDrawings = foldFaces hcompDrawings



drawExpr0 :: ((Env , Context) , Expr) -> DrawingGL 
drawExpr0 x =
     debugRainbow $
       collectDrawings
     $ Bf.second
       -- (const ())
       (const (
              translate [0.01 , 0.01] $
              scale 0.98 $
              Drawing [ ( unitHyCube 2  , SShape [ Rgba 0.0 1.0 1.0 1.0 ]  ) ])  )
       -- (makeGrid 2 3)
       ((toCub x) :: (Cub ((Env , Context) , Expr) CellExpr))


-- type Step1 = 

drawCell :: Int -> ((Env , Context) , Expr) -> Address -> CellExpr -> Either String DrawingGL
drawCell _ _ _ _ =
  Right $ Drawing [ ( unitHyCube 2  , SShape [ Rgba 0.0 1.0 1.0 1.0 ]  ) ]

drawExpr :: ((Env , Context) , Expr) -> Either String DrawingGL 
drawExpr =
          toCub
      >>> cubMap 2 drawCell []
      >>> fmap (collectDrawings)
      >>> fmap (debugRainbow)
