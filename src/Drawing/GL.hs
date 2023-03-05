{-# LANGUAGE DeriveGeneric #-}
module Drawing.GL where


import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue
                                  , newTVarIO)
-- import Graphics.Rendering.OpenGL as GL
-- import Graphics.UI.GLFW as GLFW
import Control.Monad
import Control.Monad.Writer (Writer , tell , mapWriterT , listen, runWriter ,execWriter)
-- import Control.Monad.Trans
import System.Exit ( exitWith, ExitCode(..) )
import System.IO  
import System.Environment

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Drawing.Color
import Drawing.Base as D
import Drawing.Example

import Data.List

import Data.Aeson

import Data.Maybe

import Data.Bifunctor
import GHC.Generics

import DataExtra

import Debug.Trace

-- data Descriptor = Descriptor
--   { dPrimitiveMode :: PrimitiveMode
--   , dVertexArrayObject :: VertexArrayObject
--   , dArrayIndex :: ArrayIndex
--   , dNumArrayIndices :: NumArrayIndices
--   , dLineWidth :: Int
--   }
--   deriving Show

data PrimitiveMode =
     Points
     -- ^ Treats each vertex as a single point. Vertex /n/ defines point /n/.
     -- /N/ points are drawn.
   | Lines
     -- ^ Treats each pair of vertices as an independent line segment. Vertices
     -- 2/n/-1 and 2/n/ define line /n/. /N/\/2 lines are drawn.
   | LineLoop
     -- ^ Draws a connected group of line segments from the first vertex to the
     -- last, then back to the first. Vertices /n/ and /n/+1 define line /n/.
     -- The last line, however, is defined by vertices /N/ and 1. /N/ lines
     -- are drawn.
   | LineStrip
     -- ^ Draws a connected group of line  segments from the first vertex to the
     -- last. Vertices /n/ and /n/+1 define line /n/. /N/-1 lines are drawn.
   | Triangles
     -- ^ Treats each triplet of vertices as an independent triangle. Vertices
     -- /3n-2/, /3n-1/, and /3n/ define triangle /n/. /N\/3/ triangles are drawn.
   | TriangleStrip
     -- ^ Draws a connected group of triangles. One triangle is defined for each
     -- vertex presented after the first two vertices. For odd /n/, vertices
     -- /n/, /n/+1, and /n/+2 define triangle /n/. For even /n/, vertices /n/+1,
     -- /n/, and /n/+2 define triangle /n/. /N/-2 triangles are drawn.
   | TriangleFan
     -- ^ Draws a connected group of triangles. One triangle is defined for each
     -- vertex presented after the first two vertices. Vertices 1, /n/+1, and
     -- /n/+2 define triangle /n/. /N/-2 triangles are drawn.
   | Quads
     -- ^ Treats each group of four vertices as an independent quadrilateral.
     -- Vertices 4/n/-3, 4/n/-2, 4/n/-1, and 4/n/ define quadrilateral /n/.
     -- /N/\/4 quadrilaterals are drawn.
   | QuadStrip
     -- ^ Draws a connected group of quadrilaterals. One quadrilateral is
     --defined for each pair of vertices presented after the first pair.
     -- Vertices 2/n/-1, 2/n/, 2/n/+2, and 2/n/+1 define quadrilateral /n/.
     -- /N/\/2-1 quadrilaterals are drawn. Note that the order in which vertices
     -- are used to construct a quadrilateral from strip data is different from
     -- that used with independent data.
   | Polygon
     -- ^ Draws a single, convex polygon. Vertices 1 through /N/ define this
     -- polygon.
   | Patches
     -- ^ Only used in conjunction with tessellation. The number of vertices per
     -- patch can be set with 'patchVertices'.
   deriving ( Generic,Eq, Ord, Show )

instance FromJSON PrimitiveMode
instance ToJSON PrimitiveMode


data WebGlDescriptor = WebGlDescriptor
  { dPrimitiveMode :: PrimitiveMode
  , dvertexData :: [Float]
  , dStartIndex :: Int
  , dElemNum :: Int
  , dLineWidth :: Int
  , dInitCommands :: String
  , dDrawCommands :: String
  }
  deriving (Generic,Show)

instance FromJSON WebGlDescriptor
instance ToJSON WebGlDescriptor

data CombinedVertexData =
  CombinedVertexData { pointVs :: [Float] , lineVs :: [Float] , triangleVs :: [Float] }

emptyCVD = CombinedVertexData { pointVs = [] , lineVs = [] , triangleVs = [] }


defaultLineWidth = 2

perVert :: [[a]] -> [a] -> [a]
perVert lv lt = concat $ fmap (\x -> x ++ lt) lv 


elemsPerVert = 14

renderables2CVD :: Renderables -> CombinedVertexData
renderables2CVD =
  foldl mkVs emptyCVD

  where

    calcNormal :: [[Float]] -> [Float] 
    calcNormal [ v0 , v1 , v2 ] =
        let [ uX , uY , uZ , _] = zipWith (-) v1 v0
            [ vX , vY , vZ , _] = zipWith (-) v2 v0
        in [ uY * vZ - uZ * vY , uZ * vX - uX * vZ , uX * vY - uY * vX ]
    calcNormal [ v0 , v1 ] = [0.0 , 0.0 , 0.0 ] 
    calcNormal _ = [0.0 , 0.0 , 0.0 ] 
    
    mkVs x (D.Triangle pts , shade) =
      let pos = fmap (\x -> x ++ [0.0]) $ fmap trpl2arr $ trpl2arr pts          
      in x { triangleVs = (perVert pos (calcNormal pos ++ color2arr (shadeColor shade)
                                                     ++ [ fromIntegral (shadeMode shade)
                                                        , fromIntegral ( shadeModeVFG shade)
                                                        , fromIntegral 0]))
                ++ triangleVs x }

    mkVs x (D.Line pts , shade) =
      let pos = fmap (\x -> x ++ [0.0]) $ fmap trpl2arr $ tpl2arr pts          
      in x { lineVs = (perVert pos (calcNormal pos ++ color2arr (shadeColor shade)
                                                     ++ [ fromIntegral (shadeMode shade)
                                                        , fromIntegral ( shadeModeVFG shade)
                                                        , fromIntegral 0]))
                 ++ lineVs x }

    mkVs x (D.Point pt , shade) =
      let pos = fmap (\x -> x ++ [0.0]) $ fmap trpl2arr [pt]          
      in x { pointVs = (perVert pos (calcNormal pos ++ color2arr (shadeColor shade)
                                      ++ [ fromIntegral ( shadeMode shade)
                                         , fromIntegral (shadeModeVFG shade)
                                         , fromIntegral 0 ]))
                 ++ pointVs x}  
      
    
-- type Descriptors = (Descriptor,Descriptor,Descriptor) 

initResources :: Renderables -> [WebGlDescriptor]
initResources rs =
  let de0 = makeTrianglesResources Points (pointVs (renderables2CVD rs))
      de1 = makeTrianglesResources Lines (lineVs (renderables2CVD rs))
      de2 = makeTrianglesResources Triangles (triangleVs (renderables2CVD rs))

  in reverse [de0 , de1 , de2]
     
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


makeInitJSFun :: [String] -> String
makeInitJSFun l = unlines l

makeTrianglesResources ::  PrimitiveMode -> [Float] -> WebGlDescriptor
makeTrianglesResources pm vertices = 

     let numVertices = (length vertices) 
         firstIndex = 0


         ofst = (1 * 3 * 4 + 2 * 4 * 4 + 3 * 1 * 4)
         -- size = fromIntegral (numVertices * sizeOf (head vertices))

         arrayCmd :: String -> Int -> Int -> Int -> Writer [String] ()
         arrayCmd vName size stride offset = do
             tell ["var arrId = webgl.getAttribLocation(program, \""++vName++"\");"]
             tell ["webgl.vertexAttribPointer(arrId, "++
                     show size ++", webgl.FLOAT, false ,"++
                     show stride ++","++
                     show offset ++")"]
             tell ["webgl.enableVertexAttribArray(arrId);"]


         traingleCommands :: Writer [String] ()
         traingleCommands = do
           arrayCmd "vPosition" 4 ofst firstIndex
           arrayCmd "Normal" 3 ofst (firstIndex + 4 * 4 * 1 )
           arrayCmd "Color" 4 ofst (firstIndex + 3 * 4 * 1 + 4 * 4 * 1)           
           arrayCmd "Mode" 1 ofst (firstIndex + 3 * 4 * 1 + 4 * 4 * 2 )                      
           arrayCmd "VisFlagF" 1 ofst (firstIndex + 3 * 4 * 1 + 4 * 4 * 2 +  1 * 4 * 1)
           arrayCmd "ObjGroup" 1 ofst (firstIndex + 3 * 4 * 1 + 4 * 4 * 2 +  2 * 4 * 1)


     in WebGlDescriptor pm
          vertices
          firstIndex
          (fromIntegral (div (length vertices) elemsPerVert) )
          defaultLineWidth
          (makeInitJSFun (execWriter traingleCommands)) ""



updateAttribValues :: IO ()
updateAttribValues = undefined

data Viewport = Viewport
   { vpAlpha :: Float
   , vpBeta :: Float
   , vpGamma :: Float
   , vpScale :: Float
   , vpScreenDelta :: (Float , Float)
   }
  deriving Show


makeDrawJSFun :: [String] -> String
makeDrawJSFun l = unlines l

onDisplay :: VizGroupFlag -> Viewport -> WebGlDescriptor -> WebGlDescriptor
onDisplay vgf vp ds = 
 let dCmds :: Writer [String] ()

     -- passUnifrom :: String -> [ Float ] -> Writer [String] ()
     -- passUnifrom uName val = do
     --   tell  ["webgl.uniform4fv(offsetLoc, [1, 0, 0, 0]);"]
     --   return ()
  
     dCmds = do

      tell ["webgl.disable(webgl.BLEND);"]
      tell ["webgl.clearDepth(1);"]
      tell ["webgl.depthFunc(webgl.LEQUAL)"]

      let vMat = [(vpAlpha vp * 360)
                 ,(vpBeta vp * 360)
                 ,(vpGamma vp * 360)
                 ,(vpScale vp)]
      
      tell ["webgl.uniform4fv(uLoc[\"euler\"], "++ show vMat ++");"]
      tell ["webgl.uniform2fv(uLoc[\"screen\"], [w , h]);"]
      case dPrimitiveMode ds of
         Lines -> tell ["webgl.uniform1fv(uLoc[\"shade\"], [0]);"]
         Points -> tell ["webgl.uniform1fv(uLoc[\"shade\"], [0]);"]
         _ -> tell ["webgl.uniform1fv(uLoc[\"shade\"], [1]);"]
      tell ["webgl.uniform1fv(uLoc[\"scaleG\"], [1]);"]
      let (vpX , vpY) = vpScreenDelta vp
      tell ["webgl.uniform2fv(uLoc[\"screenDelta\"], "++show [vpX , vpY]++");"]
      tell ["webgl.uniform1fv(uLoc[\"VisF\"], ["++ show (fromIntegral vgf)++"]);"] 
      return ()       

 in ds { dDrawCommands = makeDrawJSFun (execWriter dCmds) }

  
--   let vMat = Vector4 (vpAlpha vp * 360) (vpBeta vp * 360) (vpGamma vp * 360) (vpScale vp)  --75.0 0.0 $ 1.0 * (-35.0 + 1.0 * 50.0 * sin (0.7 * (realToFrac $ fromJust now)))

  
--   uniform (UniformLocation 0 ) $= (vMat :: Vector4 Float)
--   uniform (UniformLocation 1 ) $= (Vector2 (fromIntegral w) (fromIntegral h) :: Vector2 Float)
--   case now of
--     Just nowD -> uniform (UniformLocation 2 ) $= nowD
--     Nothing -> return ()


--   let (Descriptor pm verts firstIndex numVertices _) = ds
--   case pm of
--     Lines -> uniform (UniformLocation 3 ) $= (0 :: Float)
--     Points -> uniform (UniformLocation 3 ) $= (0 :: Float)
--     _ -> uniform (UniformLocation 3 ) $= (1 :: Float)

--   uniform (UniformLocation 4 ) $= (1.0 :: Float)
--   uniform (UniformLocation 5 ) $= ((uncurry Vector2 (vpScreenDelta vp))  :: Vector2 Float)
--   uniform (UniformLocation 6 ) $= ((fromIntegral vgf) :: Float)
    
--   bindVertexArrayObject $= Just verts
--   drawArrays pm firstIndex numVertices


onDisplayAll :: VizGroupFlag -> Viewport -> [WebGlDescriptor] -> [WebGlDescriptor]
onDisplayAll vfg vp = map (onDisplay vfg vp) 

mouseToModel1d :: Int -> Int -> (Double , Double) -> (Float)
mouseToModel1d w h (x , y) =
  let p = 
            if w > h
            then (((x - (fromIntegral (w - h) * 0.5)) / fromIntegral h))
            else ((x / fromIntegral w))
  in realToFrac p

mouseToModel2d :: Int -> Int -> (Double , Double) -> (Float , Float)
mouseToModel2d w h (x , y) =
  let p = 
            if w > h
            then (((x - (fromIntegral (w - h) * 0.5)) / fromIntegral h)  , (y / fromIntegral h)  )
            else ((x / fromIntegral w) , (y - (fromIntegral (h - w) * 0.5)) / fromIntegral w)
  in second (\y -> 1.0 - y) $ (bimap realToFrac realToFrac) $ p


anglesToAxes :: (Double , Double , Double) ->
                     ((Double , Double , Double , Double)
                     ,(Double , Double , Double , Double)
                     ,(Double , Double , Double , Double)
                     ,(Double , Double , Double , Double)
                     )
anglesToAxes (anglesx , anglesy , anglesz) =
   let dEG2RAD = acos (-1) / 180.0;  -- PI/180

    -- rotation angle about X-axis (pitch)
       thetaX = anglesx * dEG2RAD;
       sx = sin(thetaX);
       cx = cos(thetaX);

    -- rotation angle about Y-axis (yaw)
       thetaY = anglesy * dEG2RAD;
       sy = sin(thetaY);
       cy = cos(thetaY);

    -- rotation angle about Z-axis (roll)
       thetaZ = anglesz * dEG2RAD;
       sz = sin(thetaZ);
       cz = cos(thetaZ);

    -- determine left axis
       left = ( cy*cz
              , sx*sy*cz + cx*sz 
              , -cx*sy*cz + sx*sz , 0.0 )

    -- determine up axis
       up = ( -cy*sz
            , -sx*sy*sz + cx*cz
            , cx*sy*sz + sx*cz , 0.0 )

    -- determine forward axis
       forward   = ( sy
                   , -sx*cy
                   , cx*cy , 0.0 )

   in -- (left , up , forward , (0.0,0.0,0.0,1.0))
      ((cy*cz             , -cy*sz            , sy     , 0.0)
      ,(sx*sy*cz + cx*sz  , -sx*sy*sz + cx*cz , -sx*cy , 0.0)
      ,(-cx*sy*cz + sx*sz , cx*sy*sz + sx*cz  , cx*cy  , 0.0)
      ,(0.0               , 0.0               , 0.0    , 1.0)
      )

frustum :: ((Double , Double , Double , Double)
                     ,(Double , Double , Double , Double)
                     ,(Double , Double , Double , Double)
                     ,(Double , Double , Double , Double)
                     )
frustum =
   let r = 1.0;
       t = 1.0;
       n = 0.1;
       f = 3;
       
   in (( n/r , 0.0 , 0.0 , 0.0)
      ,(0.0 , n/t , 0.0 , 0.0)
      ,(0.0 , 0.0 , (-1.0 * (f + n)) /( f - n) , (-2.0 * f * n) /( f - n))
      ,(0.0 , 0.0 , -1.0 , 0.0)
      )


vec4TimesMat4 :: 
                 ((Double , Double , Double , Double)
                     ,(Double , Double , Double , Double)
                     ,(Double , Double , Double , Double)
                     ,(Double , Double , Double , Double)
                     ) -> (Double , Double , Double , Double) -> (Double , Double , Double , Double)
vec4TimesMat4 (xr , yr , zr , wr) (x , y , z , w) =
   (f xr , f yr , f zr , f wr)

   where
     f (xc , yc , zc , wc) = xc * x +  yc * y + zc * z + wc * w
  

-- (
-- 		      (
-- 			vec4(sx , sy , 0.1 , 1.0)  *
-- 		       ( anglesToAxes(euler.xyz) *
-- 		      (vec4(2.0 , 2.0 , 2.0 , 1.0) * ((vec4(vPosition.x , vPosition.y , vPosition.z , 1.0)
-- 			 - vec4(0.5 , 0.5 , 0.5 , 0.0)))))))






model2Screen :: Int -> Int -> Viewport -> [Float] -> (Double , Double ,Double)  
model2Screen w h vp [pX , pY , pZ] = 
  let p :: (Double , Double , Double) -> (Double , Double , Double)
      p (x' , y' , z') =
            let y = 1.0 - ( (y' / 2.0) + 0.5 )
                x = ( (x' / 2.0) + 0.5 )
                (xx,yy) = if w > h
                          then ((((x * fromIntegral h) + (fromIntegral (w - h) * 0.5)))  , (y * fromIntegral h)  )
                          else ((x * fromIntegral w) , ((y * fromIntegral w) + (fromIntegral (h - w) * 0.5))  )
            in (xx , yy , z')


      vpCal :: (Double , Double , Double , Double) -> (Double , Double , Double , Double)
      vpCal =
            vec4TimesMat4
            (anglesToAxes ((realToFrac $ (vpAlpha vp) * 360) , (realToFrac $ (vpBeta vp) * 360)  , (realToFrac $ (vpGamma vp) * 360)))

  in   p
     $ (\(x,y,z,w) -> ( (realToFrac $ vpScale vp) * x/w
                      , (realToFrac $ vpScale vp) * y/w
                      , (realToFrac $ vpScale vp) * z/w
                      ))
     $ vpCal
     $ (2.0 * (realToFrac pX - 0.5), 2.0 * (realToFrac pY - 0.5) , 2.0 * (realToFrac pZ - 0.5), 1.0)

  
model2Screen _ _ _ x = error (show $ length x) 



  -- let (Descriptor pm1 verts1 firstIndex1 numVertices1) = ds1  
  -- bindVertexArrayObject $= Just verts1
  -- drawArrays pm1 firstIndex1 numVertices1

  -- let (Descriptor pm2 verts2 firstIndex2 numVertices2) = ds2  
  -- bindVertexArrayObject $= Just verts2

  -- drawArrays pm2 firstIndex2 numVertices2


-- testRen :: IO ()
-- testRen =
--   do putStr "drawing test"
--      render ex1ren


-- testSf :: IO ()
-- testSf =
--   do putStr "drawing test"
--      render (toRenderables ex2drw)

-- testForce :: IO ()
-- testForce =
--   do putStr "drawing test force"
--      render (toRenderablesForce ex2drw)


-- testHycu :: IO ()
-- testHycu =
--   do putStr "drawing test force"
--      render (toRenderables (D.scale 0.5 $ (unitHyCubeSkel 3 2)))


-- test = testHycu

data RenderMode = Raw

-- renderAs :: (Colorlike a) => Drawing.GL.RenderMode -> Drawing a -> IO () 
-- renderAs Raw dw = render (toRenderablesForce dw)
  
  



