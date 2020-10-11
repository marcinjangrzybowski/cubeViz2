{-# LANGUAGE FlexibleContexts #-}
module ShowExp where


import Control.Monad             (unless, when, void)
import Control.Monad.Reader      (ReaderT , ask)
import Control.Monad.Writer      (WriterT , tell)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, gets, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Control.Monad
-- import Control.Monad.Trans
import System.Exit ( exitWith, ExitCode(..) )
import System.IO  
import System.Environment
import Data.Time.Clock

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Drawing.Color
import Drawing.Base
import Drawing.GL
import Drawing.Example
import Data.List

import qualified System.Console.Pretty as SCP
import qualified System.Console.ANSI as SCA

import Data.Tuple.Extra


import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Maybe

import Syntax
import InteractiveParse

import Data.Bifunctor

import DrawExpr

import Data.Either

import DataExtra

import Combi

import qualified UI.UI as UI

import Abstract
import ConsolePrint

import ExprTransform

import Debug.Trace

import ExprParser (parseExpr)

data Msg =
    LoadFile String
  | LoadGrid
  | TermInput
 deriving (Show)


data DisplayPreferences = DisplayPreferences
   { dpShowFilling       :: Bool
   }

defaultDisplayPreferences = DisplayPreferences
   { dpShowFilling        = True
   }
   
data AppState = AppState
   { fileName             :: Maybe String
   , asSession            :: SessionState
   , asDrawMode           :: DrawExprMode
   , asViewport           :: Viewport 
   , asDragStartVP        :: Maybe Viewport
   , asUserMode           :: UserMode
   , asKeyPressed         :: Bool
   , asMessage            :: String
   , asDisplayPreferences :: DisplayPreferences
   , asTime               :: DiffTime
   }

asCursorAddress :: AppState -> Maybe Address
asCursorAddress as = 
  case (asUserMode as) of
    UMNavigation x -> Just x
    _ -> Nothing

asSubFaceToAdd :: AppState -> Maybe (Address , SubFace)
asSubFaceToAdd as =
    case (asUserMode as) of
      UMAddSubFace x -> Just x
      _ -> Nothing

asExpression = ssEnvExpr . asSession

asCub :: AppState -> (Cub () (Either Int CellExpr))
asCub = toCub . asExpression


data UserMode =
    Idle
  | UMNavigation { umCoursorAddress :: Address }
  | UMAddSubFace { umSubFaceToAdd :: (Address , SubFace) }
  | UMEditCell { umEditedCell :: Address }

data DrawExprMode = Stripes | StripesNoFill | Scaffold | Scaffold2
  deriving (Show , Eq)

drawExprModes = [
  -- Stripes ,
  Scaffold  ]

drawExpr :: AppState -> DrawExprMode -> ((Env , Context) , Expr)
                  -> Either String ([Drawing (([String] , ExtrudeMode) , Color)])
-- drawExpr _ Stripes = fmap pure . mkDrawExprFill DefaultPT
-- drawExpr _ StripesNoFill = fmap pure . mkDrawExpr DefaultPT 
-- drawExpr _ Scaffold = \e ->
--    sequence $  [
--                 mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True , sptCursorAddress = Nothing })
--               , mkDrawExprFill (ScaffoldPT { sptDrawFillSkelet = False  , sptCursorAddress = Nothing })              
--               ] <*> pure e
   
drawExpr as Scaffold = \e ->

   let (sptCA , sptMSFC) =
          case (asSubFaceToAdd as) of
             Nothing -> (asCursorAddress as , Nothing)
             Just x -> (Nothing , Just x)
  
   in sequence $ (

              [ mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True
                                           , sptCursorAddress = sptCA
                                           , sptMissingSubFaceCursor = sptMSFC
                                           , sptScaffDim = 1})
              , mkDrawExprFill (DefaultPT { dptCursorAddress = sptCA
                                          , dptShowFill = dpShowFilling $ asDisplayPreferences as
                                          , dptFillFactor = 1.0
                                              -- 0.5 * (sin (realToFrac $ asTime as) + 1)

                                          })
              ]
               -- ++
               -- maybe [] (\cAddr -> [mkDrawExpr (CursorPT { cursorAddress = cAddr })]) (asCursorAddress as)
              ) <*> pure e 

type UIApp = UI.UI AppState [Descriptor] Msg


main :: IO ()
main =
    UI.main $ UI.UIDescription
       { UI.uiDescInit                   = initialize 
       , UI.uiDescRenderFrame            = render
       , UI.uiDescUpdate                 = update
       , UI.uiListeners                  = processEvent
       , UI.uiUpdateGLDescriptor         = updateGL
       }

render win w h descriptors =
   do
      viewport <- UI.getsAppState asViewport
      liftIO $ onDisplayAll win w h viewport descriptors 
      currTime <- fmap utctDayTime $ liftIO $ getCurrentTime
      UI.modifyAppState (\s -> s { asTime = currTime })
      -- UI.flushDisplay
      return ()
      
initialize =
  do args <- liftIO getArgs
     let fName = head args

     mbInitialSessionState <- liftIO (loadFile fName)

     currTime <- fmap utctDayTime $ liftIO $ getCurrentTime
     
     case mbInitialSessionState of
       Right initialSessionState -> return $ Just $ 
             AppState
              { fileName          = Just fName
              , asViewport        = Viewport { vpAlpha = 1/5 , vpBeta = 0 , vpGamma = -0.1 }
              , asDrawMode        = Scaffold -- Stripes --   --head drawExprModes
              , asDragStartVP     = Nothing
              , asSession         = initialSessionState
              , asUserMode        = UMNavigation []
              , asKeyPressed      = False
              , asMessage         = ""
              , asDisplayPreferences = defaultDisplayPreferences
              , asTime            = currTime
              }
       Left errorMsg -> do
          liftIO $ putStrLn errorMsg
          return Nothing
             

updateView :: UIApp ()     
updateView = return ()

update msg = return ()


inputTerm :: (Env , Context) ->  UIApp (Maybe CellExpr)
inputTerm (env , ctx) =
  do let dimsInCtx = unConstrainedDimsOnly ctx
     consolePrint $ "inputTerm: " ++ "dims in context : " ++ intercalate "," dimsInCtx
     
     exprR <- fmap (parseExpr ctx) $ liftIO (getLine)
     case exprR of
       Right expr -> do let ce = toCellExpr (env , ctx) expr
                        consolePrint $ toString (env , ctx) expr
                        return $ Just ce
       Left err -> do consolePrint $ show err
                      return Nothing    
     
     
setUserMode :: UserMode -> UIApp ()
setUserMode um = do
   UI.modifyAppState (\s ->
    s { asUserMode = um })
   UI.flushDisplay

processEvent :: UI.Event -> UIApp ()         
processEvent ev = do
  eventsGlobal ev
  mode <- UI.getsAppState asUserMode
  modesEvents mode ev
   
consolePrint :: String -> UIApp ()
consolePrint str = do
  UI.modifyAppState (\s ->
    s { asMessage = str })
  printConsoleView


printExpr :: UIApp ()
printExpr =
  do cub <- UI.getsAppState asCub
     (ee , expr) <- UI.getsAppState asExpression
     addrs <- UI.getsAppState asCursorAddress

     liftIO
          $ do SCA.clearScreen
               (putStrLn $ 
                 (printCub ee (CubPrintData {cpdCursorAddress = addrs}) [] cub))


printConsoleView :: UIApp ()
printConsoleView = do
    printExpr
    liftIO $ putStrLn ""
    uMsg <- UI.getsAppState asMessage
    liftIO $ putStrLn uMsg


modifyDrawingPreferences :: (DisplayPreferences -> DisplayPreferences) -> UIApp ()
modifyDrawingPreferences f = do
    UI.modifyAppState (\s ->
               let ss = (asDisplayPreferences s)
               in s {
                       asDisplayPreferences = 
                       f ss
                                })
    UI.flushDisplay

  

setFromCub :: Cub () (Either Int CellExpr) -> UIApp () 
setFromCub newCub = do
    UI.modifyAppState (\s ->
               let ss = (asSession s)
               in s {
                       asSession = 
                       ssSetExpression ss (fromCub (fst (ssEnvExpr ss) ) newCub)
                                })
    UI.flushDisplay

transformExpr :: CubTransformation (Either Int CellExpr) -> UIApp ()
transformExpr trns =
   do
      appS <- UI.getAppState
      case (applyTransform trns) (asCub appS) of
        (Left err) -> consolePrint $ "transform error: " ++ err
        (Right newCub) -> setFromCub newCub
        

printExprCode :: UIApp ()
printExprCode =
  do (ee , expr) <- UI.getsAppState asExpression
     liftIO $ do SCA.clearScreen
                 (putStrLn 
                    $ fromRight "error while printing expr" (toCode ee expr))

setAddSubFaceMode :: Address -> UIApp ()
setAddSubFaceMode addr = do
            appS <- UI.getAppState
            let cub = asCub appS
            let vsf = Set.toList $ vaccantSubFaces cub addr
            case vsf of
                 [] -> return ()
                 (x : _) -> setUserMode (UMAddSubFace (addr , x))

               -- Just _ -> do UI.modifyAppState $ \s -> s { asSubFaceToAdd = Nothing }
               --              updateView

setNavMode :: Address -> UIApp ()
setNavMode addr = setUserMode (UMNavigation addr)

eventsGlobal :: UI.Event -> UIApp ()
eventsGlobal ev = 
      case ev of
        (UI.EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          -- printEvent "cursor pos" [show x', show y']
          mouseD <- gets UI.stateMouseDown
          x0 <- gets UI.stateDragStartX
          y0 <- gets UI.stateDragStartY

          when mouseD
            (do s <- UI.getsAppState id 
                case asDragStartVP s of
                   Nothing -> return ()
                   Just vp0 -> do
                     let newV  = (asViewport s)
                             { vpAlpha = vpAlpha vp0 - ((realToFrac (y - y0)) / 2000.0)
                             , vpGamma = vpGamma vp0 + ((realToFrac (x - x0)) / 2000.0) }
                     -- liftIO $ putStrLn $ show newV 
                     UI.setAppState $ s { asViewport = newV } 
            )
        (UI.EventMouseButton _ mb mbs mk) -> do
           when (mb == GLFW.MouseButton'1) $ do
               let pressed = mbs == GLFW.MouseButtonState'Pressed
               when pressed $
                 do UI.modifyAppState $ \s -> s
                      { asDragStartVP = Just (asViewport s)
                      }

               unless pressed $
                 UI.modifyAppState $ \s -> s
                   { asDragStartVP = Nothing
                   }
        (UI.EventKey win k scancode ks mk) -> do
              when (ks == GLFW.KeyState'Pressed) $ do
                                -- Q, Esc: exit
                  when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                    liftIO $ GLFW.setWindowShouldClose win True
                  -- ?: print instructions
                  -- when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
                  --   liftIO printInstructions
                  -- -- i: print GLFW information
                  -- when (k == GLFW.Key'I) $
                  --   liftIO $ putStrLn "x"
                  -- when (k == GLFW.Key'E) $
                    -- modify $ \s -> s { stateLogEvents = not ( stateLogEvents s) }
                  -- when (k == GLFW.Key'F) $
                    -- liftIO $ GLFW.setWindowAttrib win GLFW.WindowAttrib'Resizable False


                  -- when (k == GLFW.Key'O) $
                  --   UI.sendMsg $ LoadFile "data/input-to-viz/expr3d1-fill"
                  -- when (k == GLFW.Key'G) $
                  --   UI.sendMsg $ LoadGrid 
                  -- when (k == GLFW.Key'D) $ do
                  --   UI.sendMsg $ CycleDrawMode
                  when (k == GLFW.Key'P) $ 
                    printExprCode
                  when (k == GLFW.Key'F) $
                    modifyDrawingPreferences
                      (\dp ->
                         dp { dpShowFilling = not $ dpShowFilling dp })
        _ -> return ()    





modesEvents :: UserMode -> UI.Event -> UIApp ()         

modesEvents Idle ev = return ()

modesEvents um@(UMNavigation { umCoursorAddress = addr }) ev = do 
     appS <- UI.getAppState
     let cub = asCub appS     
     case ev of
        (UI.EventKey win k scancode ks mk) -> do
             when (ks == GLFW.KeyState'Pressed) $ do
                  when (k == GLFW.Key'T) $ do
                    UI.sendMsg $ TermInput

                  when (k == GLFW.Key'Enter) $ do
                     let ((env , ctx) , _) = asExpression appS
                     mbTerm <- inputTerm (env , contextAt ctx addr cub)
                     let toPut = maybe (Left 0) Right mbTerm
                     transformExpr (ReplaceAt addr toPut)

                    
                  when (k == GLFW.Key'A) $ do
                    case (uncons addr) of
                      Just (_ , addrTail) -> setAddSubFaceMode addrTail
                      Nothing -> return ()
                    
                    
                  when (k == GLFW.Key'C) $ do
                    if (GLFW.modifierKeysControl mk)
                    then transformExpr (RemoveCell addr)
                            
                    else transformExpr (RemoveCellLeaveFaces addr)
                    setNavMode (fullSF (getDim (head addr)) : (tailAlways addr))

                  when (k == GLFW.Key'S) $ do
                    transformExpr (SplitCell addr)
                    
                  when (k == GLFW.Key'H) $ do
                    transformExpr (ReplaceAt addr (Left 0))
                    

                  when (isArrowKey k) $ do                                        
                    UI.modifyAppState (\s ->
                       let nav = fromJust (arrowKey2nav k)
                           jna = fromRight (addr) (cubNav (asCub appS) addr nav)  

                       in s { asUserMode = UMNavigation jna })
                    UI.flushDisplay

       
        _ -> return ()


modesEvents um@(UMEditCell addr  ) ev = undefined

modesEvents um@(UMAddSubFace (addr , sf) ) ev = do
     appS <- UI.getAppState
     let cub = asCub appS
     
     case ev of
        (UI.EventKey win k scancode ks mk) -> do
            when (ks == GLFW.KeyState'Pressed) $ do
                  when (k == GLFW.Key'Enter) $ do
                     transformExpr (AddSubFace (addr , sf))
                     setUserMode (UMNavigation { umCoursorAddress = sf : addr })

                  when (k == GLFW.Key'A) $ do
                    setUserMode $ UMNavigation addr


                  when (isArrowKey k) $ do
                
                     let nav = fromJust (arrowKey2nav k)
                         vcnt = Set.toList $ vaccantSubFaces cub addr
                         sfNext =
                            case nav of
                               DNext -> rotateFrom sf vcnt
                               DPrev -> rotateFrom sf $ reverse vcnt
                               _ -> sf
                                         
                     setUserMode $ UMAddSubFace (addr , sfNext)
                
       
        _ -> return ()

updateGL :: UIApp [Descriptor]
updateGL = 
   do appState <- UI.getAppState
      let dRes = drawExpr appState (asDrawMode appState) $ (asExpression appState)
      printConsoleView
      case dRes of
        (Left errMsg) -> do
             liftIO $ putStrLn errMsg
             return []
        (Right drawings) -> do
             desc <- liftIO $ initResources $ concat (map toRenderablesForce drawings)
             return desc
      
      
----

loadFile :: String -> IO (Either String SessionState) 
loadFile fName =   
   liftIO $ do          
       handle <- (openFile fName ReadMode)
       contents <- hGetContents handle
       return $ parseInteractive contents 

   


--
              
  -- let (init , render) = renderTask $ toRenderablesForce ex1drw

  -- in  

-- main  :: IO ()
-- main =
--   do args <- getArgs
--      showDrawing example3d


---- helpers

isArrowKey GLFW.Key'Down = True
isArrowKey GLFW.Key'Up = True
isArrowKey GLFW.Key'Left = True
isArrowKey GLFW.Key'Right = True
isArrowKey _ = False

arrowKey2nav k =
  case k of
    GLFW.Key'Up -> Just DParent
    GLFW.Key'Down -> Just DChild
    GLFW.Key'Left -> Just DNext
    GLFW.Key'Right -> Just DPrev



-- TODO :
-- show context
-- add curent goal to context
-- type ITail
-- for ITail defined

-- "zoom" into cell
