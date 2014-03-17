module Import (
      module X
    , chr, ord, digitToInt
    , Text, pack, unpack
    , AList
    , SourcePos, Pos, SourcePos'(..), Pos'
    ) where

import Data.Maybe as X
import Data.Either as X
import Data.List as X
import Data.Symbol as X
import Data.Ratio as X

import Control.Applicative as X
import Control.Monad as X
import Control.Monad.Trans.Class as X
import Control.Monad.IO.Class as X
import Control.Monad.Either as X

import Data.Char (chr, ord, digitToInt)
import Data.Text (Text, pack, unpack)
import Text.Parsec (SourcePos)

type AList k v = [(k, v)]

type Pos a = (SourcePos, a)
data SourcePos' = SimplePos SourcePos
                | ImplicitPos
type Pos' a = (SourcePos', a)
instance Show SourcePos' where
	show (SimplePos pos) = show pos
	show _ = error "TODO: show my SourcePos"

