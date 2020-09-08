module ShowExp where




import Control.Monad
-- import Control.Monad.Trans
import System.Exit ( exitWith, ExitCode(..) )
import System.IO  
import System.Environment

import Drawing.Base
import Drawing.GL
import Drawing.Example
import Data.List

import Data.Maybe

import Syntax
import InteractiveParse

import Data.Bifunctor

import DrawExpr

import Combi

import qualified UI.UI as UI

-- showTerm :: SessionState -> IO ()
-- showTerm ss@(SessionState ee _ _ e) =
   
--       let drawing = drawExpr $ ssEnvExpr $ ss
--           dim = getDim ee
      
--       in either
--             putStr
--             (case dim of
--                2 -> (renderAs Raw . embed 1 (const 0))
--                3 -> (renderAs Raw)
--             )
--             drawing

-- mainShowTerm :: String -> IO ()
-- mainShowTerm fname =
--   do let list = []
--      handle <- openFile fname ReadMode
--      contents <- hGetContents handle
--      let parseResult = parseInteractive contents
--      putStr $ either id (show) parseResult
--      either putStr showTerm parseResult
--      hClose handle   


-- main :: IO ()
-- -- main = mainShowTerm "data/input-to-viz/penta-lhs"
-- main =
--   do args <- getArgs
--      putStr (show args)
--      mainShowTerm (head args)


main :: IO ()
main =

  let (init , render) = renderTask $ toRenderablesForce ex1drw

  in   UI.main $ UI.UIDescription
       { UI.uiDescInitialState           = ()
       , UI.uiDescRenderInit             = init
       , UI.uiDescRenderFrame            = render
       }



-- main  :: IO ()
-- main =
--   do args <- getArgs
--      showDrawing example3d




