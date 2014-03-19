module Murex.Interpreter.Builtin where

import Import
import Data.Sequence ( Seq, (|>), (<|), (><)
                     , ViewL(..), ViewR(..), viewl, viewr)
import qualified Data.Sequence as S
import Murex.Interpreter.Values


------ Booleans ------
notBool (MurexBool a) = MurexBool $ not a
eqBool (MurexBool a) (MurexBool b) = MurexBool $ a == b
andBool (MurexBool a) (MurexBool b) = MurexBool $ a && b
orBool (MurexBool a) (MurexBool b) = MurexBool $ a || b
xorBool (MurexBool a) (MurexBool b) = MurexBool $ a /= b


------ Arithmetic ------
negNum (MurexNum a) = MurexNum (negate a)
-- turns out, floating point arithmetic is hard. error conditions have to be put in sticky flags, which means I need access to some stateful flags
addNum :: Value -> Value -> Value
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

subNum (MurexNum a) (MurexNum b) = MurexNum (a - b)
mulNum (MurexNum a) (MurexNum b) = MurexNum (a * b)
divNum (MurexNum a) (MurexNum b) = if numerator b == 0 then Nothing else Just (MurexNum (a / b))


------ Relations ------
eqNum (MurexNum a) (MurexNum b) = MurexBool (a == b)
neqNum (MurexNum a) (MurexNum b) = MurexBool (a /= b)
ltNum (MurexNum a) (MurexNum b) = MurexBool (a < b)
lteNum (MurexNum a) (MurexNum b) = MurexBool (a > b)
gtNum (MurexNum a) (MurexNum b) = MurexBool (a <= b)
gteNum (MurexNum a) (MurexNum b) = MurexBool (a >= b)


------ Characters ------
eqChr (MurexChar a) (MurexChar b) = MurexBool (a == b)


------ Conversion ------

--numFloor (MurexNum a) = MurexInt (numerator a `div` denominator a)
--numCeil (MurexNum a) = MurexInt $ if denominator a == 1 then numerator a else (numerator a `div` denominator a) + 1

--numNumer (MurexNum a) = MurexInt $ numerator a
--numDenom (MurexNum a) = MurexInt $ denominator a

numToChr :: Value -> Maybe Value
numToChr (MurexNum a) = if valid then Just (MurexChar $ chr int) else Nothing
    where
    valid = denominator a == 1 && 0 <= int && int <= 0x10FFFF
    int = fromInteger $ numerator a
chrToNum :: Value -> Value
chrToNum (MurexChar c) = MurexNum (fromIntegral (ord c) % 1)


------ Sequences ------
lenSeq :: Value -> Value
lenSeq (MurexSeq xs) = MurexNum (fromIntegral (S.length xs) % 1)
ixSeq :: Value -> Int -> Maybe Value
ixSeq (MurexSeq xs) i = if inBounds i xs then Just (S.index xs i) else Nothing
setSeq :: Value -> Int -> Value -> Maybe Value
setSeq (MurexSeq xs) i x = if inBounds i xs then Just (MurexSeq $ S.update i x xs) else Nothing
inBounds i xs = 0 <= i && i < S.length xs

consSeq :: Value -> Value -> Value
consSeq x (MurexSeq xs) = MurexSeq $ x <| xs
snocSeq :: Value -> Value -> Value
snocSeq (MurexSeq xs) x = MurexSeq $ xs |> x
catSeq :: Value -> Value -> Value
catSeq (MurexSeq a) (MurexSeq b) = MurexSeq $ a >< b
--TODO split seq at index

headSeq (MurexSeq xs) = case viewl xs of { EmptyL -> Nothing; x :< _ -> Just x }
lastSeq (MurexSeq xs) = case viewr xs of { EmptyR -> Nothing; _ :> x -> Just x }
tailSeq (MurexSeq xs) = case viewl xs of { EmptyL -> Nothing; _ :< xs -> Just (MurexSeq xs) }
initSeq (MurexSeq xs) = case viewr xs of { EmptyR -> Nothing; xs :> _ -> Just (MurexSeq xs) }


------ Finite Types ------
project :: Label -> Value -> Maybe Value
project i (MurexRecord xs) = lookup i xs
project i (MurexVariant i0 x) = if i == i0 then Just x else Nothing

update :: Label -> Value -> Value -> Maybe Value
update i (MurexRecord xs) v = case break ((==i) . fst) xs of
	(_, []) -> Nothing
	(before, (_:after)) -> Just . MurexRecord $ before ++ ((i,v):after)

setField :: Label -> Value -> Value -> Value
setField i (MurexRecord xs) x = MurexRecord $ (i, x) : filter ((i /=) . fst) xs

