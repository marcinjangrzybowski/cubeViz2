module Reorientable where


import Drawing.Base

import Syntax

import Combi

import Abstract

class OfDim a => Reorientable a where
  

class OfDim a => Diagonable a where  

instance Reorientable (Drawing b) where
  
