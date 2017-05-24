{-# LANGUAGE GADTs #-}

module Main where

import Protolude


data Expr a where
  IntVal    :: Int    -> Expr Int
  FloatVal  :: Float  -> Expr Float
  DoubleVal :: Double -> Expr Double
  BoolVal   :: Bool   -> Expr Bool

  IntAdd :: Expr Int -> Expr Int -> Expr Int
  IntSub :: Expr Int -> Expr Int -> Expr Int
  IntMul :: Expr Int -> Expr Int -> Expr Int
  IntDiv :: Expr Int -> Expr Int -> Expr Int

  FloatAdd :: Expr Float -> Expr Float -> Expr Float
  FloatSub :: Expr Float -> Expr Float -> Expr Float
  FloatMul :: Expr Float -> Expr Float -> Expr Float
  FloatDiv :: Expr Float -> Expr Float -> Expr Float

  DoubleAdd :: Expr Double -> Expr Double -> Expr Double
  DoubleSub :: Expr Double -> Expr Double -> Expr Double
  DoubleMul :: Expr Double -> Expr Double -> Expr Double
  DoubleDiv :: Expr Double -> Expr Double -> Expr Double

  BoolAnd :: Expr Bool -> Expr Bool -> Expr Bool
  BoolOr  :: Expr Bool -> Expr Bool -> Expr Bool
  BoolNot :: Expr Bool -> Expr Bool

  IntBitAnd :: Expr Int -> Expr Int -> Expr Int
  IntBitOr  :: Expr Int -> Expr Int -> Expr Int
  IntBitNot :: Expr Int -> Expr Int

  IntUnaryPlus    :: Expr Int    -> Expr Int
  FloatUnaryPlus  :: Expr Float  -> Expr Float
  DoubleUnaryPlus :: Expr Double -> Expr Double

  IntUnaryMinus    :: Expr Int    -> Expr Int
  FloatUnaryMinus  :: Expr Float  -> Expr Float
  DoubleUnaryMinus :: Expr Double -> Expr Double

  FloatRound :: Expr Float -> Expr Float


reify :: Expr a -> Text
reify (IntVal  v)          = show v
reify (FloatVal v)         = show v
reify (DoubleVal v)        = show v
reify (BoolVal  v)         = if v then "true" else "false"
reify (IntAdd l r)         = "(" <> reify l <> "+"  <> reify r <> ")"
reify (FloatAdd l r)       = "(" <> reify l <> "+"  <> reify r <> ")"
reify (DoubleAdd l r)      = "(" <> reify l <> "+"  <> reify r <> ")"
reify (IntSub l r)         = "(" <> reify l <> "-"  <> reify r <> ")"
reify (FloatSub l r)       = "(" <> reify l <> "-"  <> reify r <> ")"
reify (DoubleSub l r)      = "(" <> reify l <> "-"  <> reify r <> ")"
reify (IntMul l r)         = "(" <> reify l <> "*"  <> reify r <> ")"
reify (FloatMul l r)       = "(" <> reify l <> "*"  <> reify r <> ")"
reify (DoubleMul l r)      = "(" <> reify l <> "*"  <> reify r <> ")"
reify (IntDiv l r)         = "(" <> reify l <> "/"  <> reify r <> ")"
reify (FloatDiv l r)       = "(" <> reify l <> "/"  <> reify r <> ")"
reify (DoubleDiv l r)      = "(" <> reify l <> "/"  <> reify r <> ")"
reify (BoolAnd l r)        = "(" <> reify l <> "&&" <> reify r <> ")"
reify (BoolOr l r)         = "(" <> reify l <> "||" <> reify r <> ")"
reify (BoolNot e)          = "(" <>            "!"  <> reify e <> ")"
reify (IntBitAnd l r)      = "(" <> reify l <> "&"  <> reify r <> ")"
reify (IntBitOr l r)       = "(" <> reify l <> "|"  <> reify r <> ")"
reify (IntBitNot e)        = "(" <>            "~"  <> reify e <> ")"
reify (IntUnaryPlus e)     = "+"  <> reify e
reify (FloatUnaryPlus e)   = "+"  <> reify e
reify (DoubleUnaryPlus e)  = "+"  <> reify e
reify (IntUnaryMinus e)    = "-"  <> reify e
reify (FloatUnaryMinus e)  = "-"  <> reify e
reify (DoubleUnaryMinus e) = "-"  <> reify e
reify (FloatRound e)       = "Math.fround("  <> reify e <> ")"


coerce :: Expr a -> Expr a
coerce (IntAdd l r) = IntBitOr (IntAdd (coerce l) (coerce r)) (IntVal 0)
coerce (IntSub l r) = IntBitOr (IntSub (coerce l) (coerce r)) (IntVal 0)
coerce (IntMul l r) = IntBitOr (IntMul (coerce l) (coerce r)) (IntVal 0)
coerce (IntDiv l r) = IntBitOr (IntDiv (coerce l) (coerce r)) (IntVal 0)
coerce (FloatAdd l r) = FloatRound (FloatAdd (coerce l) (coerce r))
coerce (FloatSub l r) = FloatRound (FloatSub (coerce l) (coerce r))
coerce (FloatMul l r) = FloatRound (FloatMul (coerce l) (coerce r))
coerce (FloatDiv l r) = FloatRound (FloatDiv (coerce l) (coerce r))
coerce (DoubleAdd l r) = DoubleUnaryPlus (DoubleAdd (coerce l) (coerce r))
coerce (DoubleSub l r) = DoubleUnaryPlus (DoubleSub (coerce l) (coerce r))
coerce (DoubleMul l r) = DoubleUnaryPlus (DoubleMul (coerce l) (coerce r))
coerce (DoubleDiv l r) = DoubleUnaryPlus (DoubleDiv (coerce l) (coerce r))
coerce v = v


data Fn a = Fn
  { fnName :: Text
  , fnExpr :: Expr a }

mkFn :: Fn a -> Text
mkFn f = "function " <> fnName f <> "() { return " <> (reify . coerce $ fnExpr f) <> "}"


data Module a = Module
  { moduleName :: Text
  , moduleFn  :: Fn a }

mkModule :: Module a -> Text
mkModule m = header <> "{" <> "\"use asm\"; return {" <> exports <> "}; }"
  where
    header = "function " <> moduleName m <> "(stdlib, foreign, heap)"
    exports = (\ f -> fnName f <> ":" <> mkFn f) $ moduleFn m


e0 :: Expr Int
e0 = IntAdd (IntVal 3) (IntMul (IntVal 2) (IntVal 5))

e1 :: Expr Float
e1 = FloatMul (FloatDiv (FloatVal 2) (FloatVal 3)) (FloatDiv (FloatVal 3) (FloatVal 2))

f0 :: Fn Int
f0 = Fn { fnName = "f0", fnExpr = e0 }

f1 :: Fn Float
f1 = Fn { fnName = "f1", fnExpr = e1 }

m0 :: Module Int
m0 = Module { moduleName = "m0", moduleFn = f0 }

m1 :: Module Float
m1 = Module { moduleName = "m1", moduleFn = f1 }


main :: IO ()
main = do
  putText $ mkModule m0
  putText $ mkModule m1
