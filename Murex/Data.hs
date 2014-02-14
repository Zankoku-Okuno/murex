module Murex.Data (
      MurexData(..)

    , addNum

    , seqIndex
    ) where

import Import
import Data.Ratio
import Data.Sequence ( Seq, (|>), (<|), (><)
                     , ViewL(..), ViewR(..), viewl, viewr)
import qualified Data.Sequence as S


data MurexData = MurexUnit
               | MurexNum Rational
--               | MurexInf | MurexNegZ | MurexNegInf | MurexNaN
               | MurexChar Char
               | MurexSeq (Seq MurexData)
               | MurexRecord (AList (Either Int Symbol) MurexData)
               | MurexVariant (Either Int Symbol) MurexData


addNum :: MurexData -> MurexData -> MurexData
addNum (MurexNum a) (MurexNum b) = MurexNum (a + b)
----TODO verify these choice against floating point standards
----computations involving NaN
--addNum MurexNaN _ = MurexNaN
--addNum _ MurexNaN = MurexNaN
----computations involving infinities
--addNum MurexInf MurexNegInf = MurexNaN
--addNum MurexNegInf MurexInf = MurexNaN
--addNum MurexInf _ = MurexInf
--addNum _ MurexInf = MurexInf
--addNum MurexNegInf _ = MurexNegInf
--addNum _ MurexNegInf = MurexNegInf
----computations involving negative zero
--addNum MurexNegZ MurexNegZ = MurexNegZ
--addNum MurexNegZ (MurexNum a) = MurexNum a
--addNum (MurexNum a) MurexNegZ = MurexNum a

--TODO sub, mul, div, maybe pow, sqrt, log(e,2,10)
--TODO char/num conversion

-- turns out, floating point arithmetic is hard. error conditions have to be put in sticky flags, which means I need access to some stateful flags
seqIndex :: MurexData -> Int -> Maybe MurexData
seqIndex (MurexSeq xs) i = if i < 0 || i >= S.length xs then Nothing else Just (S.index xs i)
--TODO cons, snoc, cat

--TODO projections for murex prod/sum
--TODO product modify/set

instance Show MurexData where
    show MurexUnit = "()"
    show (MurexNum x) = if denominator x == 1
                            then show (numerator x)
                            else show (numerator x) ++ "/" ++ show (denominator x)
    show (MurexChar c) = show c --TODO show like murex parses
    show (MurexSeq s) = show s --TODO show strings better
    show (MurexRecord xs) = "{" ++ intercalate ", " (map showRecordItem xs) ++ "}"
    show (MurexVariant l x) = "{" ++ showRecordItem (l, x) ++ "}"
showRecordItem (l, x) = "`" ++ show l ++ " " ++ show x

