{-# LANGUAGE FlexibleContexts #-}
module ShowExpOld where


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

-- asExpression = fmap ssEnvExpr . asSession

-- asCub :: AppState -> Maybe (Cub () (Either Int CellExpr))
-- asCub = fmap toCub . asExpression

-- termDrawing :: UIApp (Maybe (Either String [Drawing ColorType]))
-- termDrawing =
--    do appState <- UI.getAppState
--       let drawings = fmap (drawExpr appState (asDrawMode appState)) $ (asExpression appState)
--       return drawings
-- --           dim = getDim ee
      
-- --       in bimap
-- --             id
-- --             (case dim of
-- --                2 -> map (embed 1 (const 0))
-- --                3 -> map (id)
-- --             )
-- --             drawings

-- -- data EditCommand = ClearCell

-- data Msg =
--     SetDescriptor [Descriptor]
--   | LoadFile String
--   | LoadGrid
--   | CycleDrawMode
--   | EditCub (CubTransformation (Either Int CellExpr))
--   | TermInput
--   | ToggleSubfaceAdding Address

-- data AppState = AppState
--    { fileName          :: Maybe String
--    , asSession         :: SessionState
--    , asDrawMode        :: DrawExprMode
--    , asViewport        :: Viewport 
--    , asDragStartVP     :: Maybe Viewport
--    , asUserMode        :: UserMode
--    }

-- asCursorAddress :: AppState -> Maybe Address
-- asCursorAddress as = 
--   case (asUserMode as) of
--     UMNavigaton x -> Just x
--     _ -> Nothing

-- asSubFaceToAdd :: AppState -> Maybe (Address , SubFace)
-- asSubFaceToAdd as =
--     case (asUserMode as) of
--       UMAddSubFace x -> Just x
--       _ -> Nothing


-- data UserMode =
--     Idle
--   | UMNavigaton { umCoursorAddress :: Address }
--   | UMAddSubFace { umSubFaceToAdd :: (Address , SubFace) }
--   | UMEditCell { umEditedCell :: (Address , SubFace) }

-- data DrawExprMode = Stripes | StripesNoFill | Scaffold | Scaffold2
--   deriving (Show , Eq)

-- drawExprModes = [
--   -- Stripes ,
--   Scaffold  ]

-- drawExpr :: AppState -> DrawExprMode -> ((Env , Context) , Expr)
--                   -> Either String ([Drawing (([String] , ExtrudeMode) , Color)])
-- -- drawExpr _ Stripes = fmap pure . mkDrawExprFill DefaultPT
-- -- drawExpr _ StripesNoFill = fmap pure . mkDrawExpr DefaultPT 
-- -- drawExpr _ Scaffold = \e ->
-- --    sequence $  [
-- --                 mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True , sptCursorAddress = Nothing })
-- --               , mkDrawExprFill (ScaffoldPT { sptDrawFillSkelet = False  , sptCursorAddress = Nothing })              
-- --               ] <*> pure e
   
-- drawExpr as Scaffold = \e ->

--    let (sptCA , sptMSFC) =
--           case (asSubFaceToAdd as) of
--              Nothing -> (asCursorAddress as , Nothing)
--              Just x -> (Nothing , Just x)
  
--    in sequence $ (

--               [ mkDrawExpr (ScaffoldPT { sptDrawFillSkelet = True
--                                            , sptCursorAddress = sptCA
--                                            , sptMissingSubFaceCursor = sptMSFC
--                                            , sptScaffDim = 1})
--               , mkDrawExprFill (DefaultPT { dptCursorAddress = sptCA
--                                            })
--               ]
--                -- ++
--                -- maybe [] (\cAddr -> [mkDrawExpr (CursorPT { cursorAddress = cAddr })]) (asCursorAddress as)
--               ) <*> pure e 

-- type UIApp = UI.UI AppState [Descriptor] Msg

-- printExpr :: UIApp ()
-- printExpr =
--   do mbCub <- UI.getsAppState asCub
--      mbExpr <- UI.getsAppState asExpression
--      addrs <- UI.getsAppState asCursorAddress
--      case (mbCub , mbExpr) of
--        (Just cub , Just (ee , expr)) ->
--              liftIO
--            $ do SCA.clearScreen
--                 (putStrLn 
--                   $ 
--                       (printCub ee (CubPrintData {cpdCursorAddress = addrs}) [] cub))
--        _ -> return ()

-- printExprCode :: UIApp ()
-- printExprCode =
--   do mbExpr <- UI.getsAppState asExpression
--      case mbExpr of
--        Just (ee , expr) ->
--              liftIO
--            $ do SCA.clearScreen
--                 (putStrLn 
--                   $ fromRight "error while printing expr" (toCode ee expr))
--        _ -> return ()

-- main :: IO ()
-- main =
--     UI.main $ UI.UIDescription
--        { UI.uiDescInit                   = init 
--        , UI.uiDescRenderFrame            = render
--        , UI.uiDescUpdate                 = update
--        , UI.uiListeners                  = processEvent
--        }

--     where

--       render win w h descriptors =
--          do
--             viewport <- UI.getsAppState asViewport
--             liftIO $ onDisplayAll win w h viewport descriptors 
        
      
--       init =
--         do args <- liftIO getArgs
--            let fName = head args
           
--            -- dsc <- liftIO $ initResources $ toRenderablesForce ex1drw
--            tell $ pure (LoadFile fName)

--            -- testB <- liftIO getLine
--            -- liftIO $ putStr $ show testB
             
--            return $ AppState
--             { fileName          = Nothing
--             , asViewport        = Viewport { vpAlpha = 1/5 , vpBeta = 0 , vpGamma = -0.1 }
--             , asDrawMode        = Scaffold -- Stripes --   --head drawExprModes
--             , asDragStartVP     = Nothing
--             , asSession         = Nothing
--             , asUserMode        = Idle
--             }

--       updateView = 
--                 do tdRes <- termDrawing
--                    case tdRes of
--                         Nothing -> UI.sendMsg $ SetDescriptor []
--                         Just (Left errMsg) -> liftIO $ putStrLn errMsg
--                         Just (Right drawings) ->
--                           do
--                              desc <- liftIO $ initResources $ concat (map toRenderablesForce drawings)
--                              UI.sendMsg $ SetDescriptor desc
--                    printExpr
                             
--       printAddress :: UIApp ()
--       printAddress = do
--            aSt <- UI.getAppState
--            liftIO $ putStrLn (show (asCursorAddress aSt))


--       consolePrint :: String -> UIApp ()
--       consolePrint str = do
--            liftIO $ putStrLn (str)

--       setFromCub :: Cub () (Either Int CellExpr) -> UIApp () 
--       setFromCub newCub = do
--            UI.modifyAppState (\s ->
--                     case (asSession s) of
--                       Nothing -> s
--                       Just (ss) -> s {
--                             asSession = Just $
--                               ssSetExpression ss (fromCub (fst (ssEnvExpr ss) ) newCub)
--                                        }

--                    )
--            updateView
--            return ()

 
       
      
--       update msg =
--         case msg of
--           SetDescriptor dsc -> 
--             do modify $ \s -> s
--                   { UI.stateGLDesc  = Just dsc 
--                   }
--           LoadFile fName ->
--             do UI.modifyAppState $ \s -> s
--                   { fileName = Just fName 
--                   }

--                fileLoadingResult <-
--                     liftIO $
--                       do
--                          handle <- (openFile fName ReadMode)
--                          contents <- hGetContents handle
--                          return $ parseInteractive contents 
                         
--                case fileLoadingResult of
--                   Left errMsg -> liftIO $ putStrLn errMsg
--                   Right sessionState -> do
--                          UI.modifyAppState $ (\s -> s
--                              { asSession = Just sessionState
--                              , asCursorAddress = Nothing
--                               })
--                          liftIO $ putStrLn (show sessionState)
--                          updateView
--                          return ()

--           LoadGrid ->
--             do 
--                UI.modifyAppState $ (\s -> s
--                    { asSession =
--                       let (ee , expr) = (mkExprGrid 3 2)
--                       in Just $ SessionState ee [] (BType 0) expr

--                      , asCursorAddress = Just []
--                     })
--                updateView
                         
--           CycleDrawMode -> do
--                UI.modifyAppState $ \s -> s
--                   { asDrawMode = rotateFrom (asDrawMode s) drawExprModes 
--                   }                
--                updateView
--           EditCub trns -> do
--                appS <- UI.getAppState
--                case fmap (applyTransform trns) (asCub appS) of
--                  Just (Left err) -> consolePrint $ "transform error: " ++ err
--                  Just (Right newCub) -> setFromCub newCub
--                  Nothing -> return ()

--           TermInput -> do
--             s <- liftIO getLine 
--             consolePrint s

--           ToggleSubfaceAdding addr -> do
--             appS <- UI.getAppState
--             case (asSubFaceToAdd appS) of
--                Nothing ->
--                   case (asCub appS) of
--                      Nothing -> return ()
--                      Just cub -> do 
--                         let vsf = Set.toList $ vaccantSubFaces cub addr
--                         case vsf of
--                           [] -> return ()
--                           (x : _) -> do UI.modifyAppState $ \s -> s { asSubFaceToAdd = Just (addr , x) }
--                                         updateView
--                Just _ -> do UI.modifyAppState $ \s -> s { asSubFaceToAdd = Nothing }
--                             updateView

--       processEvent :: UI.Event -> UIApp ()         
--       processEvent ev = do
--            eventsGlobal ev


--       eventsGlobal :: UI.Event -> UIApp ()
--       eventsGlobal ev = 
--             case ev of
--               (UI.EventCursorPos _ x y) -> do
--                 let x' = round x :: Int
--                     y' = round y :: Int
--                 -- printEvent "cursor pos" [show x', show y']
--                 mouseD <- gets UI.stateMouseDown
--                 x0 <- gets UI.stateDragStartX
--                 y0 <- gets UI.stateDragStartY
                
--                 when mouseD
--                   (do s <- UI.getsAppState id 
--                       case asDragStartVP s of
--                          Nothing -> return ()
--                          Just vp0 -> do
--                            let newV  = (asViewport s)
--                                    { vpAlpha = vpAlpha vp0 - ((realToFrac (y - y0)) / 2000.0)
--                                    , vpGamma = vpGamma vp0 + ((realToFrac (x - x0)) / 2000.0) }
--                            -- liftIO $ putStrLn $ show newV 
--                            UI.setAppState $ s { asViewport = newV } 
--                   )
--               (UI.EventMouseButton _ mb mbs mk) -> do
--                  when (mb == GLFW.MouseButton'1) $ do
--                      let pressed = mbs == GLFW.MouseButtonState'Pressed
--                      when pressed $
--                        do UI.modifyAppState $ \s -> s
--                             { asDragStartVP = Just (asViewport s)
--                             }
                          
--                      unless pressed $
--                        UI.modifyAppState $ \s -> s
--                          { asDragStartVP = Nothing
--                          }
--               (UI.EventKey win k scancode ks mk) -> do
--                     when (ks == GLFW.KeyState'Pressed) $ do
--                                       -- Q, Esc: exit
--                         when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
--                           liftIO $ GLFW.setWindowShouldClose win True
--                         -- ?: print instructions
--                         -- when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $
--                         --   liftIO printInstructions
--                         -- -- i: print GLFW information
--                         -- when (k == GLFW.Key'I) $
--                         --   liftIO $ printInformation win
--                         -- when (k == GLFW.Key'E) $
--                           -- modify $ \s -> s { stateLogEvents = not ( stateLogEvents s) }
--                         -- when (k == GLFW.Key'F) $
--                           -- liftIO $ GLFW.setWindowAttrib win GLFW.WindowAttrib'Resizable False

                      
--                         when (k == GLFW.Key'O) $
--                           UI.sendMsg $ LoadFile "data/input-to-viz/expr3d1-fill"
--                         when (k == GLFW.Key'G) $
--                           UI.sendMsg $ LoadGrid 
--                         when (k == GLFW.Key'D) $ do
--                           UI.sendMsg $ CycleDrawMode
--                         when (k == GLFW.Key'P) $ 
--                           printExprCode

--       -- eventsMode


--       modesEvents :: UserMode -> UI.Event -> UIApp ()         
--       modesEvents um@(UMNavigaton { umCoursorAddress = ca }) ev = do
--            case ev of
--               -- (UI.EventKey win k scancode ks mk) -> do


--               --           when (k == GLFW.Key'T) $ do
--               --             UI.sendMsg $ TermInput
                          
--               --           -- when (k == GLFW.Key'Enter) $ do
--               --           --   appS <- UI.getAppState
--               --           --   case (asSubFaceToAdd appS)  of
--               --           --     (Just (addr , sf)) -> do
--               --           --          UI.setAppState (appS { asSubFaceToAdd = Nothing , asCursorAddress = Just (sf : addr)})
--               --           --          UI.sendMsg $ EditCub (AddSubFace (addr , sf)) 
--               --           --     _ -> return ()

--               --           when (k == GLFW.Key'A) $ do
--               --             appS <- UI.getAppState
--               --             case ((asCursorAddress appS) >>= uncons , asCub appS)  of
--               --               (Just ( _ , addr) , Just cub) -> do
--               --                    UI.sendMsg $ ToggleSubfaceAdding addr
--               --               _ -> return ()
                          
  

--               --           when (k == GLFW.Key'C) $ do
--               --             appS <- UI.getAppState
--               --             case (asCursorAddress appS , asCub appS)  of
--               --               (Just addr , Just cub) -> do
--               --                    if (GLFW.modifierKeysControl mk)
--               --                    then UI.sendMsg $ EditCub (RemoveCell addr)
--               --                    else UI.sendMsg $ EditCub (RemoveCellLeaveFaces addr)
--               --                    UI.modifyAppState (\s ->                               
--               --                      s { asUserMode = Just (tailAlways addr) })
--               --               _ -> return ()
                            
--               --           when (k == GLFW.Key'S) $ do
--               --             appS <- UI.getAppState
--               --             case (asCursorAddress appS , asCub appS)  of
--               --               (Just addr , Just cub) -> 
--               --                    UI.sendMsg $ EditCub (SplitCell addr)
--               --               _ -> return ()

--               --           when (k == GLFW.Key'H) $ do
--               --             appS <- UI.getAppState
--               --             case (asCursorAddress appS , asCub appS)  of
--               --               (Just addr , Just cub) -> 
--               --                    UI.sendMsg $ EditCub (ReplaceAt addr (Left 0))
--               --               _ -> return ()
                            
--               --           when (isArrowKey k) $ do
--               --             appS <- UI.getAppState
--               --             case (asSubFaceToAdd appS , asCub appS) of
--               --                (Nothing , _) ->
--               --                  case (asCursorAddress appS , asCub appS)  of                            
--               --                    (Just addr , Just cub) -> do
--               --                       UI.modifyAppState (\s ->
--               --                          let nav = fromJust (arrowKey2nav k)
--               --                              jna = fromRight (addr) (cubNav cub addr nav)  

--               --                          in s { asCursorAddress = Just jna })
--               --                       -- printAddress
--               --                       updateView
--               --                    (Nothing , Just cub) -> do
--               --                       UI.modifyAppState (\s ->
--               --                           s { asCursorAddress = Just [] })
--               --                       -- printAddress
--               --                       updateView
--               --                    _ -> return ()

--               --                (Just (addr , sf) , Just cub) -> do
--               --                          (UI.modifyAppState (\s ->
--               --                               let nav = fromJust (arrowKey2nav k)
--               --                                   vcnt = Set.toList $ vaccantSubFaces cub addr
--               --                                   sfNext =
--               --                                      case nav of
--               --                                         DNext -> rotateFrom sf vcnt
--               --                                         DPrev -> rotateFrom sf $ reverse vcnt
--               --                                         _ -> sf

--               --                               in s { asSubFaceToAdd = Just (addr , sfNext) }))                                       
--               --                          updateView
--               --                _ -> return ()

--               --               -- fullSF
--               --             -- modify $ \s -> s { stateLogEvents = not ( stateLogEvents s) }           
--               _ -> return ()
              
              
--   -- let (init , render) = renderTask $ toRenderablesForce ex1drw

--   -- in  

-- -- main  :: IO ()
-- -- main =
-- --   do args <- getArgs
-- --      showDrawing example3d


-- ---- helpers

-- isArrowKey GLFW.Key'Down = True
-- isArrowKey GLFW.Key'Up = True
-- isArrowKey GLFW.Key'Left = True
-- isArrowKey GLFW.Key'Right = True
-- isArrowKey _ = False

-- arrowKey2nav k =
--   case k of
--     GLFW.Key'Up -> Just DParent
--     GLFW.Key'Down -> Just DChild
--     GLFW.Key'Left -> Just DNext
--     GLFW.Key'Right -> Just DPrev



-- -- TODO :
-- -- show context
-- -- add curent goal to context
-- -- type ITail
-- -- for ITail defined

-- -- "zoom" into cell
