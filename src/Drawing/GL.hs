module Drawing.GL where


import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue
                                  , newTVarIO)
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
-- import Control.Monad.Trans
import System.Exit ( exitWith, ExitCode(..) )
import System.IO  
import System.Environment

import LoadShaders
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Drawing.Color
import Drawing.Base as D
import Drawing.Example

import Data.List

import Data.Maybe

import Data.Bifunctor

import DataExtra

import Debug.Trace

data Descriptor = Descriptor
  { dPrimitiveMode :: PrimitiveMode
  , dVertexArrayObject :: VertexArrayObject
  , dArrayIndex :: ArrayIndex
  , dNumArrayIndices :: NumArrayIndices
  , dLineWidth :: Int
  }
  deriving Show


data CombinedVertexData =
  CombinedVertexData { pointVs :: [GLfloat] , lineVs :: [GLfloat] , triangleVs :: [GLfloat] }

emptyCVD = CombinedVertexData { pointVs = [] , lineVs = [] , triangleVs = [] }


defaultLineWidth = 2

perVert :: [[a]] -> [a] -> [a]
perVert lv lt = concat $ fmap (\x -> x ++ lt) lv 


elemsPerVert = 11

renderables2CVD :: Renderables -> CombinedVertexData
renderables2CVD =
  foldl mkVs emptyCVD

  where

    calcNormal :: [[GLfloat]] -> [GLfloat] 
    calcNormal [ v0 , v1 , v2 ] =
        let [ uX , uY , uZ ] = zipWith (-) v1 v0
            [ vX , vY , vZ ] = zipWith (-) v2 v0
        in [ uY * vZ - uZ * vY , uZ * vX - uX * vZ , uX * vY - uY * vX ]
    calcNormal [ v0 , v1 ] = [0.0 , 0.0 , 0.0 ] 
    calcNormal _ = [0.0 , 0.0 , 0.0 ] 
    
    mkVs x (D.Triangle pts , shade) =
      let pos = fmap trpl2arr $ trpl2arr pts          
      in x { triangleVs = (perVert pos (calcNormal pos ++ color2arr (shadeColor shade)
                                                     ++ [ fromIntegral (shadeMode shade) , fromIntegral ( shadeModeVFG shade) ]))
                ++ triangleVs x }

    mkVs x (D.Line pts , shade) =
      let pos = fmap trpl2arr $ tpl2arr pts          
      in x { lineVs = (perVert pos (calcNormal pos ++ color2arr (shadeColor shade)
                                                     ++ [ fromIntegral (shadeMode shade) , fromIntegral ( shadeModeVFG shade)  ]))
                 ++ lineVs x }

    mkVs x (D.Point pt , shade) =
      let pos = fmap trpl2arr [pt]          
      in x { pointVs = (perVert pos (calcNormal pos ++ color2arr (shadeColor shade)
                                                     ++ [ fromIntegral ( shadeMode shade)  , fromIntegral (shadeModeVFG shade) ]))
                 ++ pointVs x}  
      
    
type Descriptors = (Descriptor,Descriptor,Descriptor) 

initResources :: Renderables -> IO [Descriptor]
initResources rs =
  do de0 <- initTrianglesResources Points (pointVs (renderables2CVD rs))
     de1 <- initTrianglesResources Lines (lineVs (renderables2CVD rs))
     de2 <- initTrianglesResources Triangles (triangleVs (renderables2CVD rs))
     -- putStr (show de2)
     return $ reverse [de0 , de1 , de2]
     
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initTrianglesResources ::  PrimitiveMode -> [GLfloat] -> IO Descriptor
initTrianglesResources pm vertices = 

  do triangles <- genObjectName
     bindVertexArrayObject $= Just triangles

     -- putStr (show $ length vertices)
     -- let vertices = [

     let numVertices = (length vertices) 

         -- maskAttr = concat $ replicate numVertices mask0


     program <- loadShaders [
        ShaderInfo VertexShader (FileSource "data/shaders/shader3d.vert"),
        ShaderInfo FragmentShader (FileSource "data/shaders/shader3d.frag")]
     currentProgram $= Just program

     let firstIndex = 0
         vPosition = AttribLocation 0


     let ofst = (2 * 3 * 4 + 1 * 4 * 4 + 2 * 1 * 4)
     let size = fromIntegral (numVertices * sizeOf (head vertices))


     withArray vertices $ \ptr -> do
       arrayBuffer <- genObjectName
       bindBuffer ArrayBuffer $= Just arrayBuffer

       bufferData ArrayBuffer $= (size, ptr, StaticDraw)

       vertexAttribPointer vPosition $=
         (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset firstIndex))
       vertexAttribArray vPosition $= Enabled



       normalBuffer <- genObjectName
       bindBuffer ArrayBuffer $= Just normalBuffer
       
       bufferData ArrayBuffer $= (size, ptr, StaticDraw)
       let normalPosition = AttribLocation 1

       vertexAttribPointer normalPosition $=
         (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset (firstIndex + 3 * 4 * 1 )))
       vertexAttribArray normalPosition $= Enabled




       colorBuffer <- genObjectName
       bindBuffer ArrayBuffer $= Just colorBuffer
       
       bufferData ArrayBuffer $= (size, ptr, StaticDraw)

       let colorPosition = AttribLocation 2

       vertexAttribPointer colorPosition $=
         (ToFloat, VertexArrayDescriptor 4 Float ofst (bufferOffset (firstIndex + 3 * 4 * 2)))
       vertexAttribArray colorPosition $= Enabled




       modeBuffer <- genObjectName
       bindBuffer ArrayBuffer $= Just modeBuffer
       
       bufferData ArrayBuffer $= (size, ptr, StaticDraw)
       let modePosition = AttribLocation 3

       vertexAttribPointer modePosition $=
         (ToFloat, VertexArrayDescriptor 1 Float ofst (bufferOffset (firstIndex + 3 * 4 * 2 + 4 * 4 * 1)))
       vertexAttribArray modePosition $= Enabled



       visFlagBuffer <- genObjectName
       bindBuffer ArrayBuffer $= Just visFlagBuffer
       
       bufferData ArrayBuffer $= (size, ptr, DynamicDraw)
       let visFlagPosition = AttribLocation 4

       vertexAttribPointer visFlagPosition $=
         (ToFloat, VertexArrayDescriptor 1 Float ofst (bufferOffset (firstIndex + 3 * 4 * 2 + 4 * 4 * 1 +  1 * 4 * 1)))
       vertexAttribArray visFlagPosition $= Enabled





       return $ Descriptor pm triangles firstIndex (fromIntegral (div (length vertices) elemsPerVert) ) defaultLineWidth



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

onDisplay :: Window -> Int -> Int -> VizGroupFlag -> Viewport -> Descriptor -> IO ()
onDisplay win w h vgf vp ds = do

  -- GL.clear [GL.DepthBuffer]
  blend $= Disabled
  vertexProgramPointSize $= Enabled
  clearDepth $= 1
  depthFunc $= Just Lequal
  cullFace $= Nothing
  lineWidth $= 2
  now <- GLFW.getTime
  -- blendFunc $= (SrcAlpha , OneMinusSrcAlpha)
  polygonSmooth $= Disabled

  
  let vMat = Vector4 (vpAlpha vp * 360) (vpBeta vp * 360) (vpGamma vp * 360) (vpScale vp)  --75.0 0.0 $ 1.0 * (-35.0 + 1.0 * 50.0 * sin (0.7 * (realToFrac $ fromJust now)))

  
  uniform (UniformLocation 0 ) $= (vMat :: Vector4 GLfloat)
  uniform (UniformLocation 1 ) $= (Vector2 (fromIntegral w) (fromIntegral h) :: Vector2 GLfloat)
  case now of
    Just nowD -> uniform (UniformLocation 2 ) $= nowD
    Nothing -> return ()


  let (Descriptor pm verts firstIndex numVertices _) = ds
  case pm of
    Lines -> uniform (UniformLocation 3 ) $= (0 :: GLfloat)
    Points -> uniform (UniformLocation 3 ) $= (0 :: GLfloat)
    _ -> uniform (UniformLocation 3 ) $= (1 :: GLfloat)

  uniform (UniformLocation 4 ) $= (1.0 :: GLfloat)
  uniform (UniformLocation 5 ) $= ((uncurry Vector2 (vpScreenDelta vp))  :: Vector2 GLfloat)
  uniform (UniformLocation 6 ) $= ((fromIntegral vgf) :: GLfloat)
    
  bindVertexArrayObject $= Just verts
  drawArrays pm firstIndex numVertices


onDisplayAll :: Window -> Int -> Int -> VizGroupFlag -> Viewport -> [Descriptor] -> IO ()
onDisplayAll win w h vfg vp = void . sequence . map (onDisplay win w h vfg vp) 

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
  
  



