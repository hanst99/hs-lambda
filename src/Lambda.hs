{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}

module Lambda where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String(IsString, fromString)

newtype Name = Name String
  deriving (Eq, Ord)
  deriving Show via String
  deriving Read via String

instance IsString Name where
  fromString = Name

data Expr = Variable Name | Apply Expr Expr | Lambda Name Expr
  deriving (Eq, Show)

instance IsString Expr where
  fromString = Variable . fromString

canonicalise :: Expr -> Expr
canonicalise = go M.empty where
  go :: M.Map Name Expr -> Expr -> Expr
  go env = \case
    v@(Variable name) -> fromMaybe v (M.lookup name env)
    Apply fun arg ->
      let canonicalFun = go env fun
          canonicalArg = go env arg
      in case canonicalFun of
        Lambda name body ->
          go (M.insert name canonicalArg env) body
        _ -> Apply canonicalFun canonicalArg
    Lambda name body -> Lambda name (go env body)


pretty :: Expr -> String
pretty e = go 0 e "" where
  go _ (Variable (Name s)) = (s++)
  go n (Apply fun arg) = showParen (n >= 10) $ go 10 fun . (' ':). go 10 arg
  go n (Lambda (Name name) body) = showParen (n > 5) $ ("λ" ++).  (name ++) . (". " ++) . go 5 body

chainApply :: [Expr] -> Expr
chainApply = foldl1 Apply
