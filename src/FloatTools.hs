module FloatTools where

import Abstract

import Syntax

import Drawing.Base

import qualified Data.Bifunctor as Bf

import qualified Data.Map as Map

import Control.Arrow

import Combi

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

sideTransInv :: Float ->  Bool -> Face -> ( (Int -> Float)) -> (Int -> Float)
sideTransInv compPar sk (Face n (i , b0)) f k =
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

