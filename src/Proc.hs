module Proc where

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


mainProcTerm :: String -> IO ()
mainProcTerm fname =
  do let list = []
     handle <- openFile fname ReadMode
     contents <- hGetContents handle
     let parseResult = parseInteractive contents
     putStr $ either id (show) parseResult
     putStr $ either id
       (\(SessionState (env , ctx) _ bType expr) -> toStringEE ctx expr)
       parseResult
     
     hClose handle   


main :: IO ()
-- main = mainShowTerm "data/input-to-viz/penta-lhs"
main =
  do args <- getArgs
     putStr (show args)
     mainProcTerm ("data/input-to-viz/" ++ head args)


