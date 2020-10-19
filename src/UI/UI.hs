module UI.UI where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.Reader      (ReaderT , ask , runReaderT, reader)
import Control.Monad.Writer      (WriterT, tell , runWriterT, listen)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, gets, liftIO, modify, put, mapRWST)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List                 (intercalate)
import Data.Maybe                (catMaybes)
import Text.PrettyPrint   hiding ((<>))

import qualified Data.Set as Set

import Data.Function

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import DataExtra

--------------------------------------------------------------------------------

data Env = Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    }

data State uiState glDesc uiMsg = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , stateLogEvents       :: !Bool
    , stateMouseDown       :: !Bool
    , stateDragging        :: !Bool
    , stateDragStartX      :: !Double
    , stateDragStartY      :: !Double
    , stateUI              :: uiState
    , stateGLDesc          :: Maybe glDesc
    , stateMsgsQueue       :: [uiMsg]
    , statePressedKeys     :: Set.Set GLFW.Key
    }

--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !Bool
  | EventWindowIconify   !GLFW.Window !Bool
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

type UI uiState glDesc uiMsg = RWST Env [uiMsg] (State uiState glDesc uiMsg) IO

type UIWithInfo uiState glDesc uiMsg = RWST Env (String , [uiMsg]) (State uiState glDesc uiMsg) IO

data UIDesc uiState glDesc uiMsg = UIDescription
  { uiDescInit                   :: WriterT [uiMsg] IO (Maybe uiState)
  -- , uiDescRenderInit             :: IO glDesc
  , uiDescRenderFrame            :: GLFW.Window -> Int -> Int -> glDesc -> UI uiState glDesc uiMsg () -- -> glDesc -> IO ()
  , uiDescUpdate                 :: uiMsg -> UI uiState glDesc uiMsg ()
  , uiListeners                  :: Event -> UI uiState glDesc uiMsg ()
  , uiUpdateGLDescriptor         :: UI uiState glDesc uiMsg glDesc
  }


main :: UIDesc uiState glDesc uiMsg -> IO ()
main uiDesc =
  do
    let width  = 640
        height = 480
    
    eventsChan <- newTQueueIO :: IO (TQueue Event)

    -- GLFW.windowHint $ GLFW.WindowHint'Resizable False

    withWindow width height "cubeViz2" $ \win -> do
        GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
        GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
        GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
        GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
        GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
        GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
        GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
        GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
        GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
        GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
        GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
        GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
        GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
        GLFW.setCharCallback            win $ Just $ charCallback            eventsChan


        GLFW.swapInterval 1

        (fbWidth, fbHeight) <- GLFW.getFramebufferSize win


        -- GLFW.setWindowSizeLimits win (Just fbWidth) (Just fbHeight) Nothing Nothing --(Just fbWidth) (Just fbHeight)


        -- GLFW.setWindowAttrib win GLFW.WindowAttrib'Floating True
        -- GLFW.setWindowAttrib win GLFW.WindowAttrib'Resizable False


        (mbInitialState , msgs) <- runWriterT (uiDescInit uiDesc)
        -- descriptors <- uiDescRenderInit uiDesc
        (mbInitialState) & (maybe (putStrLn "unable to initialize - exiting...")
         (\initialState -> do

          let env = Env
                      { envEventsChan    = eventsChan
                      , envWindow        = win
                      }
          let state = State
                      { stateWindowWidth     = fbWidth
                      , stateWindowHeight    = fbHeight
                      , stateLogEvents       = False
                      , stateMouseDown       = False
                      , stateDragging        = False
                      , stateDragStartX      = 0
                      , stateDragStartY      = 0
                      , stateUI              = initialState
                      , stateGLDesc          = Nothing --descriptors
                      , stateMsgsQueue       = msgs
                      , statePressedKeys     = Set.empty
                      }
          runUI uiDesc env state))

    putStrLn "ended!"

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    -- GLFW.defaultWindowHints
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

modifyAppState :: (uiState -> uiState) -> UI uiState glDesc uiMsg ()
modifyAppState f =
  modify $ \s -> s { stateUI = f (stateUI s) }

getsAppState :: (uiState -> a) -> UI uiState glDesc uiMsg a
getsAppState f =
  gets (f . stateUI)

getAppState :: Monoid b => RWST a b (State uiState glDesc uiMsg) IO uiState
getAppState =
  gets (stateUI)

setAppState :: uiState -> UI uiState glDesc uiMsg ()
setAppState s = modifyAppState (const s)
  

sendMsg :: uiMsg -> UI uiState glDesc uiMsg ()
sendMsg msg =
  do modify $ \s -> s { stateMsgsQueue = msg : stateMsgsQueue s }
     

runUI :: UIDesc uiState glDesc uiMsg ->  Env -> State uiState glDesc uiMsg -> IO ()
runUI uiDesc env state = do
    printInstructions
    void $ evalRWST (adjustWindow >> run uiDesc) env state

run :: UIDesc uiState glDesc uiMsg -> UI uiState glDesc uiMsg ()
run uiDesc =
  
  do


      msgs <- gets $ stateMsgsQueue

      modify $ \s -> s { stateMsgsQueue = []}

      sequence $ fmap (uiDescUpdate uiDesc) msgs
      

      state <- get
      
      win <- asks envWindow




      let width  = stateWindowWidth  state
          height = stateWindowHeight state

      liftIO $ do GL.clearColor GL.$= GL.Color4 1 1 1 1
                  -- GLFW.swapInterval 0
                  GL.clear [GL.ColorBuffer , GL.DepthBuffer]

      mbDescriptors <- gets stateGLDesc 

      descriptors <-             
          case mbDescriptors of
              Just dsc ->
                 return dsc
              Nothing -> do  newDesc <- uiUpdateGLDescriptor uiDesc
                             modify $ \s -> s { stateGLDesc = Just newDesc}
                             return newDesc

      (uiDescRenderFrame uiDesc) win width height descriptors
          
      -- draw
      liftIO $ do
          GLFW.swapBuffers win
          GL.flush  -- not necessary, but someone recommended it
          GLFW.pollEvents

          
      processEvents (uiListeners uiDesc)

      -- state <- get
      -- if stateDragging state
      --   then do
      --       let sodx  = stateDragStartX      state
      --           sody  = stateDragStartY      state
      --           sodxa = stateDragStartXAngle state
      --           sodya = stateDragStartYAngle state
      --       (x, y) <- liftIO $ GLFW.getCursorPos win
      --       let myrot = (x - sodx) / 2
      --           mxrot = (y - sody) / 2
      --       put $ state
      --         { stateXAngle = sodxa + mxrot
      --         , stateYAngle = sodya + myrot
      --         }
      --   else do
      --       (kxrot, kyrot) <- liftIO $ getCursorKeyDirections win
      --       (jxrot, jyrot) <- liftIO $ getJoystickDirections GLFW.Joystick'1
      --       put $ state
      --         { stateXAngle = stateXAngle state + (2 * kxrot) + (2 * jxrot)
      --         , stateYAngle = stateYAngle state + (2 * kyrot) + (2 * jyrot)
      --         }

      mt <- liftIO GLFW.getTime
      -- modify $ \s -> s
      --   { stateGearZAngle = maybe 0 (realToFrac . (100*)) mt
      --   }

      q <- liftIO $ GLFW.windowShouldClose win
      unless q (run uiDesc)

flushDisplay :: UI uiState glDesc uiMsg ()  
flushDisplay =
  modify $ \s -> s { stateGLDesc = Nothing}

processEvents :: (Event -> UI uiState glDesc uiMsg ()) -> UI uiState glDesc uiMsg ()
processEvents appListeners = pe

  where pe =
            do
              tc <- asks envEventsChan
              me <- liftIO $ atomically $ tryReadTQueue tc
              case me of
                Just e -> do
                    processEvent e
                    appListeners e
                    pe
                Nothing -> return ()
   
      
processEvent :: Event -> UI uiState glDesc uiMsg ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowPos _ x y) ->
          printEvent "window pos" [show x, show y]

      (EventWindowSize _ width height) ->
          printEvent "window size" [show width, show height]

      (EventWindowClose _) ->
          printEvent "window close" []

      (EventWindowRefresh _) ->
          printEvent "window refresh" []

      (EventWindowFocus _ fs) ->
          printEvent "window focus" [show fs]

      (EventWindowIconify _ is) ->
          printEvent "window iconify" [show is]

      (EventFramebufferSize _ width height) -> do
          printEvent "framebuffer size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventMouseButton _ mb mbs mk) -> do
          printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
          when (mb == GLFW.MouseButton'1) $ do
              let pressed = mbs == GLFW.MouseButtonState'Pressed
              modify $ \s -> s
                { stateMouseDown = pressed
                }
              unless pressed $
                modify $ \s -> s
                  { stateDragging = False
                  }

      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "cursor pos" [show x', show y']
          state <- get
          when (stateMouseDown state && not (stateDragging state)) $
            put $ state
              { stateDragging        = True
              , stateDragStartX      = x
              , stateDragStartY      = y
              }

      -- (EventCursorEnter _ cs) ->
      --     printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']
          -- env <- ask
          -- modify $ \s -> s
          --   { stateZDist =
          --       let zDist' = stateZDist s + realToFrac (negate $ y / 2)
          --       in curb (envZDistClosest env) (envZDistFarthest env) zDist'
          --   }
          
          adjustWindow

      (EventKey win k scancode ks mk) -> do
            s <- get
            let pKeys = statePressedKeys s
            when ((ks == GLFW.KeyState'Pressed || ks == GLFW.KeyState'Repeating) && (not $ Set.member k pKeys)) $ do
               put $ s
                  { statePressedKeys = Set.insert k $ pKeys
                  }
               flushDisplay
            when (ks == GLFW.KeyState'Released && (Set.member k pKeys)) $ do
               put $ s
                  { statePressedKeys = Set.delete k $ pKeys
                  }
               flushDisplay
          


      -- (EventChar _ c) ->
      --     printEvent "char" [show c]
      _ -> liftIO $ return ()

pressedKeys :: UI uiState glDesc uiMsg (Set.Set GLFW.Key)
pressedKeys = gets statePressedKeys
      
adjustWindow :: UI uiState glDesc uiMsg ()
adjustWindow = do
    state <- get
    let width  = stateWindowWidth  state
        height = stateWindowHeight state

    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    
    -- let pos   = GL.Position 0 0
    --     size  = GL.Size (fromIntegral width) (fromIntegral height)
    --     h     = fromIntegral height / fromIntegral width :: Double
    --     znear = 1           :: Double
    --     zfar  = 40          :: Double
    --     xmax  = znear * 0.5 :: Double
    -- liftIO $ do
    --     GL.viewport   GL.$= (pos, size)
    --     GL.matrixMode GL.$= GL.Projection
    --     GL.loadIdentity
    --     GL.frustum (realToFrac $ -xmax)
    --                (realToFrac    xmax)
    --                (realToFrac $ -xmax * realToFrac h)
    --                (realToFrac $  xmax * realToFrac h)
    --                (realToFrac    znear)
    --                (realToFrac    zfar)
    --     GL.matrixMode GL.$= GL.Modelview 0
    --     GL.loadIdentity
    --     GL.translate (GL.Vector3 0 0 (negate $ realToFrac zDist) :: GL.Vector3 GL.GLfloat)

    liftIO $ return ()

-- draw :: UI ()
-- draw = do
--     env   <- ask
--     state <- get
--     liftIO (return ())

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
    x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
    x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
    y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
    y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
    let x0n = if x0 then (-1) else 0
        x1n = if x1 then   1  else 0
        y0n = if y0 then (-1) else 0
        y1n = if y1 then   1  else 0
    return (x0n + x1n, y0n + y1n)

getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)
getJoystickDirections js = do
    maxes <- GLFW.getJoystickAxes js
    return $ case maxes of
      (Just (x:y:_)) -> (-y, x)
      _              -> ( 0, 0)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

--------------------------------------------------------------------------------

printInstructions :: IO ()
printInstructions =
    putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "'?': Print these instructions"                                $+$
        text "'i': Print GLFW information"                                  $+$
        text ""                                                             $+$
        text "* Mouse cursor, keyboard cursor keys, and/or joystick"        $+$
        text "  control rotation."                                          $+$
        text "* Mouse scroll wheel controls distance from scene."           $+$
        text "------------------------------------------------------------"
      )

printInformation :: GLFW.Window -> IO ()
printInformation win = do
    version       <- GLFW.getVersion
    versionString <- GLFW.getVersionString
    monitorInfos  <- runMaybeT getMonitorInfos
    clientAPI     <- GLFW.getWindowClientAPI              win
    cv0           <- GLFW.getWindowContextVersionMajor    win
    cv1           <- GLFW.getWindowContextVersionMinor    win
    cv2           <- GLFW.getWindowContextVersionRevision win
    robustness    <- GLFW.getWindowContextRobustness      win
    forwardCompat <- GLFW.getWindowOpenGLForwardCompat    win
    debug         <- GLFW.getWindowOpenGLDebugContext     win
    profile       <- GLFW.getWindowOpenGLProfile          win

    putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "GLFW C library:" $+$
        nest 4 (
          text "Version:"        <+> renderVersion version $+$
          text "Version string:" <+> renderVersionString versionString
        ) $+$
        text "Monitors:" $+$
        nest 4 (
          renderMonitorInfos monitorInfos
        ) $+$
        text "OpenGL context:" $+$
        nest 4 (
          text "Client API:"            <+> renderClientAPI clientAPI $+$
          text "Version:"               <+> renderContextVersion cv0 cv1 cv2 $+$
          text "Robustness:"            <+> renderContextRobustness robustness $+$
          text "Forward compatibility:" <+> renderForwardCompat forwardCompat $+$
          text "Debug:"                 <+> renderDebug debug $+$
          text "Profile:"               <+> renderProfile profile
        ) $+$
        text "------------------------------------------------------------"
      )
  where
    renderVersion (GLFW.Version v0 v1 v2) =
        text $ intercalate "." $ map show [v0, v1, v2]

    renderVersionString =
        text . show

    renderMonitorInfos =
        maybe (text "(error)") (vcat . map renderMonitorInfo)

    renderMonitorInfo (name, (x,y), (w,h), vms) =
        text (show name) $+$
        nest 4 (
          location <+> size $+$
          fsep (map renderVideoMode vms)
        )
      where
        location = int x <> text "," <> int y
        size     = int w <> text "x" <> int h <> text "mm"

    renderVideoMode (GLFW.VideoMode w h r g b rr) =
        brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz  = int rr <> text "Hz"

    renderContextVersion v0 v1 v2 =
        hcat [int v0, text ".", int v1, text ".", int v2]

    renderClientAPI         = text . show
    renderContextRobustness = text . show
    renderForwardCompat     = text . show
    renderDebug             = text . show
    renderProfile           = text . show

type MonitorInfo = (String, (Int,Int), (Int,Int), [GLFW.VideoMode])

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos =
    getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors

    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
        name <- getMonitorName mon
        vms  <- getVideoModes mon
        MaybeT $ do
            pos  <- liftIO $ GLFW.getMonitorPos mon
            size <- liftIO $ GLFW.getMonitorPhysicalSize mon
            return $ Just (name, pos, size, vms)

    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon

    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon


--------------------------------------------------------------------------------

printEvent :: String -> [String] -> UI uiState glDesc uiMsg ()
printEvent cbname fields =
  do printEnabled <- gets stateLogEvents   
     when printEnabled $ liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

-----------------------------

collect :: (Monad m , Monoid c , Monoid e) => [a] ->
             (a -> RWST b c d m e) ->
               RWST b c d m [(a , e)]
collect l f = sequence (fmap (\a -> (fmap ((,) a)) (f a)) l)
