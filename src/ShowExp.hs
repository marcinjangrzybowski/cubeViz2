module ShowExp where

import Control.Monad
-- import Control.Monad.Trans
import System.Exit ( exitWith, ExitCode(..) )
import System.IO  
import System.Environment

import Drawing.Base
import Drawing.GL
import Data.List

import Data.Maybe

import Syntax
import InteractiveParse

import Data.Bifunctor

import DrawExpr



showTerm :: SessionState -> IO ()
showTerm ss =
   either putStr (renderAs Raw) $ drawExpr $ ssEnvExpr $ ss

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
     mainShowTerm (head args)



-- main  :: IO ()
-- main =
--   do args <- getArgs
--      showDrawing example3d




