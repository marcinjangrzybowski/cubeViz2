{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Drawing.Base where

import Data.Bifunctor
import Data.Tuple
-- import Data.Colour
import Data.Maybe

import Data.List
import Data.Bool
-- import Data.Floating.Classes

import Combi

import Drawing.Color

import DataExtra

-- type NPoint = [ Float ]

-- data Seg = Pt Float | Seg Float Float


class InSpace a where
  sMap :: ([Float] -> [Float]) -> a -> a

  scale :: Float -> a -> a
  scale x = sMap (fmap ((*) x))

  translate :: [Float] -> a -> a
  translate x = sMap (zipWith (+) x)
  

instance {-# OVERLAPPING #-} InSpace [Float] where
  sMap = id

instance (InSpace b)  => InSpace [b] where
  sMap = fmap . sMap

instance InSpace a  => InSpace (a , b) where
  sMap = first . sMap 

-- instance Bi a => InSpace (a [ Float ]) where
--   sMap = fmap   




type Smplx = [[ Float ]]     


-- type MetaColor = [Maybe Color]


type Drawing a = [(Smplx , a)]

type DrawingGL = Drawing Color

type Pt3D = (Float , Float , Float)


data Renderable = Point Pt3D | Line (Pt3D , Pt3D) | Triangle (Pt3D , Pt3D , Pt3D)

type Renderables = [(Renderable,Color)]  


toRenderable :: Smplx -> Maybe Renderable
toRenderable ([([ x0 , y0 , z0 ]) , ([ x1 , y1 , z1]) , ([ x2 , y2 , z2])]) =
    Just $ Triangle ((x0 , y0 , z0) , (x1 , y1 , z1) , (x2 , y2 , z2)) 
toRenderable ([([ x0 , y0 , z0 ]) , ([ x1 , y1 , z1])]) = Just $ Line ((x0 , y0 , z0) , (x1 , y1 , z1))
toRenderable ([[ x0 , y0 , z0 ]]) = Just $ Point (x0 , y0 , z0)
toRenderable _ = Nothing


-- ignores when aproching unrenderable
toRenderableForce :: Smplx -> [Renderable]
toRenderableForce l =
  maybe (explode l >>= toRenderableForce) pure $ (toRenderable l) 


-- throws error when aproching unrenderable
toRenderables :: Colorlike a => Drawing a -> Renderables
toRenderables = fmap $ bimap (fromJust . toRenderable) toColor

toRenderablesForce :: Colorlike a => Drawing a -> Renderables
toRenderablesForce l =
  fmap (bimap toRenderableForce toColor) l >>= (\(l , a) -> fmap (flip (,) a) l)

transposeDrw :: Int -> Drawing a -> Drawing a
transposeDrw k = sMap (\l ->
                         case l of
                           [] -> error "attempt to tranpose 0 dimensional drawing"
                           _ -> listInsert k (last l) (init l)) 



versor :: Int -> Int -> [Float]
versor n k = fmap (\i -> if i == k then 1.0 else 0.0) $ range n
       
embed :: Int -> ([ Float ] -> Float) -> Drawing a -> Drawing a
embed k f = sMap (\l -> listInsert k (f l) l)  


-- extrudeNotMeta :: [Float] -> Drawing a -> Drawing a
-- extrudeNotMeta v (Drawing l) =
--   Drawing $
--     ( fmap (first $ extrudePrll v) l )



extrude :: Int -> ([Float] -> Float , [Float] -> Float) -> Drawing a -> Drawing a
extrude k (f0 , f1) =
    (=<<) (\(s , a) ->
     let ss = map
               (\i ->
                 let s0 = fmap (\pt -> listInsert k (f0 pt) pt) $ take (i + 1) s
                     s1 = fmap (\pt -> listInsert k (f1 pt) pt) $ drop i s
                 in s0 ++ s1
                  )
               $ fmap fst $ zip [0..] s
         
     in map (flip (,) a) (ss))
  -- Drawing $
  --   ( fmap
  --     (\(p , c) ->
  --        case c of
  --          Mask -> undefined
  --          MShape a -> undefined
  --          SShape a -> (extrudePrll v p , SShape a)
  --            )
  --     l )


-- data Renderable

type ZDrawing a = FromLI Piece (Drawing a)


-- dim is dimention of input,
pieceHyperPlanes :: (Bool , Int) -> Piece -> ([Float] -> Float , [Float] -> Float)
pieceHyperPlanes (b , j) pc@( crnr@(Subset dim _) , pmr) =
   if b
   then mapBoth ((.) (\x -> 1.0 - x)) hyperPlane
   else hyperPlane
  where
    crnrB = toListLI crnr 
    
    hyperPlanes :: [[Float] -> Float]
    hyperPlanes =
       [const 0]
         ++
       (fmap (flip (!!)) $ (toListLI pmr))
         ++
       [const 0.5]

    hyperPlane = ( hyperPlanes !! j , hyperPlanes !! (j + 1) ) 

degen :: Int -> ZDrawing a -> ZDrawing a
degen k (FromLI n f) =
   (FromLI (n + 1)
     (\pc ->
        let ((crnr , side) , pcPrj ) = projectSplit k pc              
        in extrude k (pieceHyperPlanes (crnr , side) pcPrj) (f pcPrj) )
   )

degenAll :: [Int] -> ZDrawing a -> ZDrawing a
degenAll ds x = foldl (flip degen) x ds


extrudeSeg :: Drawing a -> Drawing a
extrudeSeg =
  (=<<) (\(s , a) ->
     let ss = map
               (\i ->
                 let s0 = fmap ((:) 0) $ take (i + 1) s
                     s1 = fmap ((:) 1) $ drop i s
                 in s0 ++ s1
                  )
               $ fmap fst $ zip [0..] s
         
     in map (flip (,) a) (ss))

-- todo inline!
unitHyCube :: Int -> Drawing ()
unitHyCube 0 = [([[]] , ())]
unitHyCube n = extrudeSeg (unitHyCube (n - 1))


unitHyCubeSkel :: Int -> Int -> Drawing ()
unitHyCubeSkel _ k | k < 0
                   = []
unitHyCubeSkel 0 _ = [([[]] , ())]
unitHyCubeSkel n k =
  if k >= n
  then unitHyCube n
  else
        embed 0 (const 0) (unitHyCubeSkel (n - 1) k)
          ++
        embed 0 (const 1) (unitHyCubeSkel (n - 1) k)
          ++
        extrudeSeg (unitHyCubeSkel (n - 1) (k - 1))
