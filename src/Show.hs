module Show where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
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


mmHelp :: Maybe a -> (a -> Maybe b) -> Maybe (Maybe b)
mmHelp Nothing _ = Just Nothing
mmHelp (Just a) f =
  case (f a) of
    Nothing -> Nothing
    Just b -> Just (Just b) 


quad2tris [x00 , x01 , x10 , x11] = [x00 , x01 , x10 , x11 , x10 , x01]
quad2tris _ = []

drawing2vertex :: DrawingGL -> IO [GLfloat]
drawing2vertex drw = 
  return (concat $ map shpVertexes $ drawingSplitBase drw)

  where

    tpl2Arr (x , y) = [x , y]
    
    shpVertexes ((2 , l ) , mbm , ((Rgba r g b a))) =
      fromMaybe []
        (do tl <- quad2tris <$> (sequence $ (map asTuples l))
            mbm2 <- mmHelp mbm (sequence . map asTuples . snd)
            
            let color = [r , g , b , a]
                mask =
                  case mbm2 of
                    Just mTpls -> [0 , 1] ++ (concat $ (tpl2Arr (head mTpls)) : (map tpl2Arr mTpls))  
                    Nothing -> [0 , 0] ++ replicate 8 0

                tailData :: [GLfloat]
                tailData = mask ++ color

                verts :: [[GLfloat]]
                verts = map (tpl2Arr) tl

                pitl :: [GLfloat]
                pitl = (intercalate tailData (verts ++ [[]]) )
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
     ShaderInfo VertexShader (FileSource "data/shaders/shader.vert"),
     ShaderInfo FragmentShader (FileSource "data/shaders/shader.frag")]
  currentProgram $= Just program

  let firstIndex = 0
      vPosition = AttribLocation 0


  let ofst = (6 * 2 * 4 + 4 * 4 )
  
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float ofst (bufferOffset firstIndex))
  vertexAttribArray vPosition $= Enabled

  ctrlBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just ctrlBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let ctrlPosition = AttribLocation 1
  
  vertexAttribPointer ctrlPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float ofst (bufferOffset (firstIndex + 2 * 4 * 1)))
  vertexAttribArray ctrlPosition $= Enabled


  m0Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m0Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m0Position = AttribLocation 2
  
  vertexAttribPointer m0Position $=
    (ToFloat, VertexArrayDescriptor 2 Float ofst (bufferOffset (firstIndex + 2 * 4 * 2)))
  vertexAttribArray m0Position $= Enabled


  m1Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m1Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m1Position = AttribLocation 3
  
  vertexAttribPointer m1Position $=
    (ToFloat, VertexArrayDescriptor 2 Float ofst (bufferOffset (firstIndex + 2 * 4 * 3)))
  vertexAttribArray m1Position $= Enabled


  m2Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m2Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m2Position = AttribLocation 4
  
  vertexAttribPointer m2Position $=
    (ToFloat, VertexArrayDescriptor 2 Float ofst (bufferOffset (firstIndex + 2 * 4 * 4)))
  vertexAttribArray m2Position $= Enabled

  m3Buffer <- genObjectName
  bindBuffer ArrayBuffer $= Just m3Buffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let m3Position = AttribLocation 5
  
  vertexAttribPointer m3Position $=
    (ToFloat, VertexArrayDescriptor 2 Float ofst (bufferOffset (firstIndex + 2 * 4 * 5)))
  vertexAttribArray m3Position $= Enabled


  colorBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just colorBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let colorPosition = AttribLocation 6
  
  vertexAttribPointer colorPosition $=
    (ToFloat, VertexArrayDescriptor 4 Float ofst (bufferOffset (firstIndex + 2 * 4 * 6)))
  vertexAttribArray colorPosition $= Enabled



  return $ Descriptor triangles firstIndex (fromIntegral numVertices)


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
    do
      GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
--       let ww = (realToFrac w)
--           hh = (realToFrac h)
-- --      GL.ortho2D (- (ww/2)) (ww/2) (hh/2) (- (hh/2))
      -- GL.ortho2D (- (ww/2)) (ww/2) (hh/2) (- (hh/2))

keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
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



onDisplay :: Window -> Descriptor -> IO ()
onDisplay win descriptor@(Descriptor triangles firstIndex numVertices) = do
  GL.clearColor $= Color4 1 1 1 1
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  blend $= Enabled
  blendFunc $= (SrcAlpha , OneMinusSrcAlpha)
  polygonSmooth $= Disabled
  drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers win
  
  forever $ do
     GLFW.pollEvents
     onDisplay win descriptor


