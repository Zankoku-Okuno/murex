{-# LANGUAGE FlexibleInstances #-}
module Murex.Syntax.Concrete where

import Import
import Data.Hierarchy
import Data.Hexpr
import qualified Murex.Syntax.Abstract as A
import Data.Symbol


data Atom = Lit A.Literal
          | Name [String]
          | Bind String
          | Label Label
          | Kw Keyword
          | Prim Primitive
    deriving (Eq)

data Keyword = Lambda
             | Let | In | Block
             | Def
             | At | InfixDot | Ellipsis
  deriving (Eq)
data Primitive = List | Nil
               | Xons | Xil
               | Tuple
               | Interpolate
               | Project | Update | Modify
    deriving (Eq)

type Tree = Quasihexpr SourcePos Atom


--FIXME DELME
toAST :: Tree -> A.AST
toAST (QLeaf _ (Lit x)) = A.Lit x
toAST (QLeaf _ (Name [name])) = A.Var (intern name)
toAST (QBranch p (QLeaf _ (Kw Lambda) : QLeaf _ (Name [name]) : body)) = A.Lambda [intern name] (toAST (adjoins p body))
toAST (QBranch p (QLeaf _ (Kw Lambda) : QBranch _ names : body)) = A.Lambda (transName <$> names) (toAST (adjoins p body))
    where
    transName (QLeaf _ (Name [name])) = intern name
toAST (QBranch _ [QLeaf _ (Kw Def), bind, body]) = A.Define (toAST bind) (toAST body)
toAST (QBranch _ [QLeaf _ (Kw Block), x]) = toAST x
toAST (QBranch _ (QLeaf _ (Kw Block) : xs)) = A.Block (toAST <$> xs)
toAST (QBranch _ [QLeaf _ (Kw Let), defs, QLeaf _ (Kw In), body]) = A.LetIn (toAST defs) (toAST body)
toAST (QBranch _ (QLeaf _ (Prim List) : xs)) = A.Sequence $ toAST <$> filter notNil xs
    where
    notNil (QLeaf _ (Prim Nil)) = False
    notNil _ = True
toAST (QBranch _ (QLeaf _ (Prim Xons) : xs)) = case filter notXil xs of
        [] -> error $ "can't form empty finite data"
        [x] -> uncurry A.Variant (toAssoc x)
        xs -> A.Record (toAssoc <$> xs)
    where
    notXil (QLeaf _ (Prim Xil)) = False
    notXil _ = True
    toAssoc (QBranch _ [QLeaf _ (Label l), x]) = (l, toAST x)
toAST (QBranch _ (QLeaf _ (Prim Tuple) : xs)) = A.Record $ loop 1 xs []
    where
    loop i [] acc = reverse acc
    loop i (x:xs) acc = loop (i + 1) xs $ (Left i, toAST x) : acc
toAST            (QBranch _ [QLeaf _ (Prim Project), QLeaf _ (Label l)])     = A.Project l
toAST (QBranch _ [QBranch _ [QLeaf _ (Prim Modify),  QLeaf _ (Label l)], f]) = A.Modify  l (toAST f)
toAST (QBranch _ [QBranch _ [QLeaf _ (Prim Update),  QLeaf _ (Label l)], f]) = A.Update  l (toAST f)
toAST (QBranch _ xs) = A.Apply (toAST <$> xs)
toAST tree = error $ "toAST: " ++ show tree


instance Show Atom where
    show (Lit x) = show x
    show (Name parts) = intercalate "." parts
    show (Bind name) = '?':name
    show (Label (Left i)) = '`':show i
    show (Label (Right name)) = '`':name
    show (Kw At) = "@"
    show (Kw Ellipsis) = ".."
    show (Kw x) = '#':show x
    show (Prim x) = show x
instance Show Primitive where
    show List = "#list"
    show Nil = "#nil"
    show Xons = "#xons"
    show Xil = "#xil"
    show Tuple = "#tuple"
    show Interpolate = "#str"
    show Project = "#project"
    show Modify = "#modify"
    show Update = "#update"
instance Show Keyword where
    show Lambda = "λ"
    show Block = "block"
    show Let = "let"
    show In = "in"
    show Def = "def"

instance Show (Hexpr SourcePos Atom) where
    show (Leaf _ x) = show x
    show (Branch _ [Leaf _ (Kw InfixDot), expr]) = '.':show expr
    show (Branch _ xs) = "(" ++ intercalate " " (map show xs) ++ ")"

instance Show (Quasihexpr SourcePos Atom) where
    show (QLeaf _ x) = show x
    show (QBranch _ [QLeaf _ (Kw InfixDot), expr]) = '.':show expr
    show (QBranch _ xs) = "(" ++ intercalate " " (map show xs) ++ ")"
    --TODO show quotation
