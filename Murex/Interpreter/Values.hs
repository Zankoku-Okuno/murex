module Murex.Interpreter.Values where

import Import
import Data.Foldable (toList)
import Data.Sequence ( Seq, (|>), (<|), (><)
                     , ViewL(..), ViewR(..), viewl, viewr)
import qualified Data.Sequence as S
import Murex.Syntax.Abstract (AST, Literal, Builtin(..))
import qualified Murex.Syntax.Abstract as A
import Control.Monad.Environment


type MurexEnv = MEnv IO Symbol Value

data Value = MurexUnit
           | MurexBool Bool
           | MurexNum Rational
           | MurexInf | MurexNegZ | MurexNegInf | MurexNaN
--           | MurexNat Natural --package: natural-numbers
--           | MurexInt Integer
--           | MurexRat Rational
--           | MurexI64 Int64 | MurexI32 Int32 | MurexI16 Int16 | MurexI8 Int8
--           | MurexW64 Word64 | MurexW32 Word32 | MurexW16 Word16 | MurexW8 Word8
--           | MurexF64 Double | Murex32 Float
           | MurexChar Char
           | MurexSeq (Seq Value)
           | MurexRecord (AList Label Value)
           | MurexVariant Label Value
--           | MurexBits TODO
--           | MurexRef (MVar Literal) | MurexSharedRef (IORef Literal)
--           | MurexArray (IOArray Int Literal)
--           | MurexChan Chan
--           | MurexHandle Handle
--           | MurexHandler TODO
           | Closure [Symbol] AST MurexEnv
           | Prim Builtin


{-| Map syntactic literals to machine values -}
toValue :: Literal -> Value
toValue A.MurexUnit = MurexUnit
toValue (A.MurexBool x) = MurexBool x
toValue (A.MurexNum x) = MurexNum x
toValue A.MurexInf = MurexInf
toValue A.MurexNegZ = MurexNegZ
toValue A.MurexNegInf = MurexNegInf
toValue A.MurexNaN = MurexNaN
toValue (A.MurexChar c) = MurexChar c
toValue (A.MurexSeq xs) = MurexSeq (toValue <$> xs)
toValue (A.MurexRecord xs) = MurexRecord (fmap toValue <$> xs)
toValue (A.MurexVariant l x) = MurexVariant l (toValue x)


------ Haskell <-> Murex  ------
fromMurexChar :: Value -> Char
fromMurexChar (MurexChar c) = c
toMurexChar :: Char -> Value
toMurexChar c = MurexChar c

toMurexString :: String -> Value
toMurexString = MurexSeq . S.fromList . map MurexChar
fromMurexString :: Seq Value -> Maybe String
fromMurexString s = let s' = toList s in if all isChar s'
                                    then Just (map fromMurexChar s')
                                    else Nothing
    where
    isChar (MurexChar _) = True
    isChar _ = False

instance Show Value where
    show MurexUnit = "()"
    show (MurexBool True) = "True"
    show (MurexBool False) = "False"
    show (MurexNum x) = if denominator x == 1
                            then show (numerator x)
                            else show (numerator x) ++ "/" ++ show (denominator x)
    show (MurexChar c) = show c --TODO show like murex parses
    show (MurexSeq s) | S.null s = "[]"
    show (MurexSeq s) = maybe (show $ toList s) show (fromMurexString s)
    show (MurexRecord xs) = "{" ++ intercalate ", " (map showRecordItem xs) ++ "}"
    show (MurexVariant l x) = "{" ++ showRecordItem (l, x) ++ "}"
    show (Closure _ _ _) = "<closure>"
    show (Prim (ProjFn (Left l))) = "<project: " ++ show l ++ ">"
    show (Prim (ProjFn (Right l))) = "<project: " ++ l ++ ">"
showRecordItem (Left l, x) = "`" ++ show l ++ " " ++ show x
showRecordItem (Right l, x) = "`" ++ l ++ " " ++ show x
