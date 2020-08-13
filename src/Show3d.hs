module Show3d where

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

import Drawing.Base
import Drawing.Example

import Data.List

import Data.Maybe

import Syntax
import InteractiveParse

import Data.Bifunctor

import DrawExpr

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices







bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

brownish = [Vertex2 0.2 0.2 , Vertex2 0.9 1.0];


asTuples :: [a] -> Maybe (a , a)
asTuples [x , y] = Just (x , y)
asTuples _ = Nothing

asTruples :: [a] -> Maybe (a , a , a)
asTruples [x , y , z] = Just (x , y , z)
asTruples _ = Nothing

mmHelp :: Maybe a -> (a -> Maybe b) -> Maybe (Maybe b)
mmHelp Nothing _ = Just Nothing
mmHelp (Just a) f =
  case (f a) of
    Nothing -> Nothing
    Just b -> Just (Just b) 


quad2tris [x00 , x01 , x10 , x11] = [x00 , x01 , x10 , x11 , x10 , x01]
quad2tris _ = []


oct2tris [x000 , x001 , x010 , x011 , x100 , x101 , x110 , x111 ] =
  concat
   [
    quad2tris [x000 , x001 , x010 , x011]
   ,quad2tris [x100 , x101 , x110 , x111]
   ,quad2tris [x000 , x001 , x100 , x101]
   ,quad2tris [x010 , x011 , x110 , x111]
   ,quad2tris [x000 , x010 , x100 , x110]
   ,quad2tris [x001 , x011 , x101 , x111]
   ]
oct2tris _ = []

groupPer3 :: [a] -> [[a]]
groupPer3 (x1 : x2 : x3 : xs) = [ x1 , x2 , x3 ] : (groupPer3 xs) 
groupPer3 _ = []

drawing2vertex :: DrawingGL -> IO [GLfloat]
drawing2vertex drw =
  (do let flts = (concat $ map shpVertexes $ drawingSplitBase drw)
      -- putStr $ show flts
      return flts)

  where

    tpl2Arr (x , y) = [x , y]
    trpl2Arr (x , y , z) = [x , y , z]
    
    shpVertexes ((3 , l ) , mbm , ((Rgba r g b a))) =
      fromMaybe []
        (do tl <- oct2tris <$> (sequence $ (map asTruples l))
            mbm2 <- mmHelp mbm (sequence . map asTruples . snd)
            
            let color = [r , g , b , a]
                mask =
                  case mbm2 of
                    Just mTpls -> [0 , 1] ++ (concat $ (map trpl2Arr mTpls))  
                    Nothing -> [0 , 0] ++ replicate (4 * 3) 0

                tailData :: [GLfloat]
                tailData = mask ++ color

                verts :: [[GLfloat]]
                verts = map (trpl2Arr) tl

                triangles :: [[[GLfloat]]]
                triangles = groupPer3 verts

                calcNormal :: [[GLfloat]] -> [GLfloat] 
                calcNormal [ v0 , v1 , v2 ] =
                   let [ uX , uY , uZ ] = zipWith (-) v1 v0
                       [ vX , vY , vZ ] = zipWith (-) v2 v0
                   in [ uY * vZ - uZ * vY , uZ * vX - uX * vZ , uX * vY - uY * vX ]

                pitl :: [GLfloat]
                pitl =
                  concat $
                  (map (\t ->
                          concat $ map (\pt -> pt ++ tailData ++ (calcNormal t)) t  )
                   triangles)
                
                -- pitl :: [GLfloat]
                -- pitl = (intercalate tailData (verts ++ [[]]) )

            
            return pitl
         )

    shpVertexes _ = []
    
initResources :: DrawingGL -> IO Descriptor
initResources dgl = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles



  vertices <- drawing2vertex dgl
  putStr (show $ length vertices)
  -- let vertices = [
     
  let numVertices = (length vertices)

      -- maskAttr = concat $ replicate numVertices mask0
  
  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  program <- loadShaders [
     ShaderInfo VertexShader (FileSource "data/shaders/shader3d.vert"),
     ShaderInfo FragmentShader (FileSource "data/shaders/shader3d.frag")]
  currentProgram $= Just program

  let firstIndex = 0
      vPosition = AttribLocation 0


  let ofst = (6 * 3 * 4 + 1 * 2 * 4 + 1 * 4 * 4 )
  
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset firstIndex))
  vertexAttribArray vPosition $= Enabled

  ctrlBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just ctrlBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let ctrlPosition = AttribLocation 1
  
  vertexAttribPointer ctrlPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float ofst (bufferOffset (firstIndex + 3 * 4 * 1 + 2 * 4 * 0)))
  vertexAttribArray ctrlPosition $= Enabled


  m0Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m0Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m0Position = AttribLocation 2
  
  vertexAttribPointer m0Position $=
    (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset (firstIndex + 3 * 4 * 1 + 2 * 4 * 1)))
  vertexAttribArray m0Position $= Enabled


  m1Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m1Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m1Position = AttribLocation 3
  
  vertexAttribPointer m1Position $=
    (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset (firstIndex + 3 * 4 * 2 + 2 * 4 * 1)))
  vertexAttribArray m1Position $= Enabled


  m2Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m2Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m2Position = AttribLocation 4
  
  vertexAttribPointer m2Position $=
    (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset (firstIndex + 3 * 4 * 3 + 2 * 4 * 1)))
  vertexAttribArray m2Position $= Enabled

  m3Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m3Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m3Position = AttribLocation 5
  
  vertexAttribPointer m3Position $=
    (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset (firstIndex + 3 * 4 * 4 + 2 * 4 * 1)))
  vertexAttribArray m3Position $= Enabled


  colorBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just colorBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let colorPosition = AttribLocation 6
  
  vertexAttribPointer colorPosition $=
    (ToFloat, VertexArrayDescriptor 4 Float ofst (bufferOffset (firstIndex + 3 * 4 * 5 + 2 * 4 * 1)))
  vertexAttribArray colorPosition $= Enabled

  normalBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just normalBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let normalPosition = AttribLocation 7
  
  vertexAttribPointer normalPosition $=
    (ToFloat, VertexArrayDescriptor 3 Float ofst (bufferOffset (firstIndex + 3 * 4 * 5 + 2 * 4 * 1 + 4 * 4 * 1 )))
  vertexAttribArray normalPosition $= Enabled



  return $ Descriptor triangles firstIndex (fromIntegral numVertices)


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
    do
      GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      uniform (UniformLocation 1 ) $= (Vector2 (fromIntegral w) (fromIntegral h) :: Vector2 GLfloat) 

--       let ww = (realToFrac w)
--           hh = (realToFrac h)
-- --      GL.ortho2D (- (ww/2)) (ww/2) (hh/2) (- (hh/2))
      -- GL.ortho2D (- (ww/2)) (ww/2) (hh/2) (- (hh/2))

keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed win GLFW.Key'Right _ GLFW.KeyState'Pressed _ = putStr "xx"
keyPressed _   _               _ _                     _ = return ()


shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()


showDrawing :: Colorlike a => Drawing (MetaColor a) -> IO ()
showDrawing drw0 =
  do
     let drw = toDrawingGL drw0
         -- toPr :: Drawing () 
         -- toPr = show ((fmap $ const ()) drw0)
     -- putStr (show ((fmap $ const ()) drw0) )
     GLFW.init
     GLFW.defaultWindowHints
     Just win <- GLFW.createWindow 640 480 "CubeViz2" Nothing Nothing
     GLFW.makeContextCurrent (Just win)
     GLFW.setWindowSizeCallback win (Just resizeWindow)
     GLFW.setKeyCallback win (Just keyPressed)
     GLFW.setWindowCloseCallback win (Just shutdown)
     descriptor <- initResources drw
     onDisplay win descriptor
     GLFW.destroyWindow win
     GLFW.terminate

showTerm :: SessionState -> IO ()
showTerm ss =
   either putStr showDrawing $ drawExpr $ ssEnvExpr $ ss

mainShowTerm :: String -> IO ()
mainShowTerm fname =
  do let list = []
     handle <- openFile fname ReadMode
     contents <- hGetContents handle
     let parseResult = parseInteractive contents
     putStr $ either id (show) parseResult
     either putStr showTerm parseResult
     hClose handle   


main :: IO ()
-- main = mainShowTerm "data/input-to-viz/penta-lhs"
main =
  do args <- getArgs
     putStr (show args)
     mainShowTerm ("data/input-to-viz/" ++ head args)



-- main  :: IO ()
-- main =
--   do args <- getArgs
--      showDrawing example3d



onDisplay :: Window -> Descriptor -> IO ()
onDisplay win descriptor@(Descriptor triangles firstIndex numVertices) = do
  GL.clearColor $= Color4 1 1 1 1
  GL.clear [ColorBuffer , DepthBuffer]
  bindVertexArrayObject $= Just triangles
  blend $= Disabled
  clearDepth $= 1
  depthFunc $= Just Lequal
  cullFace $= Nothing
  now <- GLFW.getTime
  blendFunc $= (SrcAlpha , OneMinusSrcAlpha)
  polygonSmooth $= Disabled
  let vMat =  Vector3 75.0 0.0 (-35.0 + 1.0 * 40.0 * sin (0.7 * (realToFrac $ fromJust now)))
  uniform (UniformLocation 0 ) $= (vMat :: Vector3 GLfloat)
  drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers win
  
  forever $ do
     GLFW.pollEvents
     onDisplay win descriptor


