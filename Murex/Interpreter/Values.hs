module Murex.Interpreter.Values where

import Import
import Murex.Syntax.Typeless
import Murex.Data
import Control.Monad.Environment


type MurexEnv = MEnv IO Symbol Value


data Value = Data MurexData
           | Closure [Symbol] AST MurexEnv
instance Show Value where
    show (Data x) = show x
    show (Closure _ _ _) = "<closure>"

