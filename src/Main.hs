module Main where

import Protolude

data JsExpr a
  = Val a
  | Add (JsExpr a) (JsExpr a)
  | Sub (JsExpr a) (JsExpr a)
  | Mul (JsExpr a) (JsExpr a)
  | Div (JsExpr a) (JsExpr a)
  | And (JsExpr a) (JsExpr a)
  | Or  (JsExpr a) (JsExpr a)
  | Not (JsExpr a)
  | BitAnd (JsExpr a) (JsExpr a)
  | BitOr  (JsExpr a) (JsExpr a)
  | BitNot (JsExpr a)
  | UnaryPlus  (JsExpr a)
  | UnaryMinus (JsExpr a)
  | Round (JsExpr a)


-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b 
instance Functor JsExpr where
  fmap f (Val v)        = Val (f v)
  fmap f (Add l r)      = Add (fmap f l) (fmap f r)
  fmap f (Sub l r)      = Sub (fmap f l) (fmap f r)
  fmap f (Mul l r)      = Mul (fmap f l) (fmap f r)
  fmap f (Div l r)      = Div (fmap f l) (fmap f r)
  fmap f (And l r)      = And (fmap f l) (fmap f r)
  fmap f (Or l r)       = Or  (fmap f l) (fmap f r)
  fmap f (Not e)        = Not (fmap f e)
  fmap f (BitAnd l r)   = BitAnd (fmap f l) (fmap f r)
  fmap f (BitOr l r)    = BitOr  (fmap f l) (fmap f r)
  fmap f (BitNot e)     = BitNot (fmap f e)
  fmap f (UnaryPlus e)  = UnaryPlus  (fmap f e)
  fmap f (UnaryMinus e) = UnaryMinus (fmap f e)
  fmap f (Round e)      = Round (fmap f e)


-- class Foldable t where
--   foldMap :: Monoid m => (a -> m) -> t a -> m 
instance Foldable JsExpr where
  foldMap f (Val v)        = f v
  foldMap f (Add l r)      = foldMap f l `mappend` foldMap f r
  foldMap f (Sub l r)      = foldMap f l `mappend` foldMap f r
  foldMap f (Mul l r)      = foldMap f l `mappend` foldMap f r
  foldMap f (Div l r)      = foldMap f l `mappend` foldMap f r
  foldMap f (And l r)      = foldMap f l `mappend` foldMap f r
  foldMap f (Or l r)       = foldMap f l `mappend` foldMap f r
  foldMap f (Not e)        = foldMap f e
  foldMap f (BitAnd l r)   = foldMap f l `mappend` foldMap f r
  foldMap f (BitOr l r)    = foldMap f l `mappend` foldMap f r
  foldMap f (BitNot e)     = foldMap f e
  foldMap f (UnaryPlus e)  = foldMap f e
  foldMap f (UnaryMinus e) = foldMap f e
  foldMap f (Round e)      = foldMap f e


-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 
instance Traversable JsExpr where
  traverse f (Val v)        = Val <$> f v
  traverse f (Add l r)      = Add <$> traverse f l <*> traverse f r
  traverse f (Sub l r)      = Sub <$> traverse f l <*> traverse f r
  traverse f (Mul l r)      = Mul <$> traverse f l <*> traverse f r
  traverse f (Div l r)      = Div <$> traverse f l <*> traverse f r
  traverse f (And l r)      = And <$> traverse f l <*> traverse f r
  traverse f (Or l r)       = Or  <$> traverse f l <*> traverse f r
  traverse f (Not e)        = Not <$> traverse f e
  traverse f (BitAnd l r)   = BitAnd <$> traverse f l <*> traverse f r
  traverse f (BitOr l r)    = BitOr  <$> traverse f l <*> traverse f r
  traverse f (BitNot e)     = BitNot <$> traverse f e
  traverse f (UnaryPlus e)  = UnaryPlus  <$> traverse f e
  traverse f (UnaryMinus e) = UnaryMinus <$> traverse f e
  traverse f (Round e)      = Round <$> traverse f e


class JsShow a where
  jsShow :: a -> Text

instance JsShow Int where
  jsShow = show

instance JsShow Float where
  jsShow = show

instance JsShow Double where
  jsShow = show

instance JsShow Bool where
  jsShow (True)  = "true"
  jsShow (False) = "false"


jsReify :: (JsShow a) => JsExpr a -> Text
jsReify (Val v)        = jsShow v
jsReify (Add l r)      = "(" <> jsReify l <> "+"  <> jsReify r <> ")"
jsReify (Sub l r)      = "(" <> jsReify l <> "-"  <> jsReify r <> ")"
jsReify (Mul l r)      = "(" <> jsReify l <> "*"  <> jsReify r <> ")"
jsReify (Div l r)      = "(" <> jsReify l <> "/"  <> jsReify r <> ")"
jsReify (And l r)      = "(" <> jsReify l <> "&&" <> jsReify r <> ")"
jsReify (Or l r)       = "(" <> jsReify l <> "||" <> jsReify r <> ")"
jsReify (Not e)        = "(" <>              "!"  <> jsReify e <> ")"
jsReify (BitAnd l r)   = "(" <> jsReify l <> "&"  <> jsReify r <> ")"
jsReify (BitOr l r)    = "(" <> jsReify l <> "|"  <> jsReify r <> ")"
jsReify (BitNot e)     = "(" <>              "~"  <> jsReify e <> ")"
jsReify (UnaryPlus e)  = "+" <> jsReify e
jsReify (UnaryMinus e) = "-" <> jsReify e
jsReify (Round e)      = "Math.fround("  <> jsReify e <> ")"


class JsCoerce a where
  jsCoerce :: JsExpr a -> JsExpr a

instance JsCoerce Int where
  jsCoerce (Add l r) = BitOr (Add (jsCoerce l) (jsCoerce r)) (Val 0)
  jsCoerce (Sub l r) = BitOr (Sub (jsCoerce l) (jsCoerce r)) (Val 0)
  jsCoerce (Mul l r) = BitOr (Mul (jsCoerce l) (jsCoerce r)) (Val 0)
  jsCoerce (Div l r) = BitOr (Div (jsCoerce l) (jsCoerce r)) (Val 0)
  jsCoerce v = v

instance JsCoerce Float where
  jsCoerce (Add l r) = Round (Add (jsCoerce l) (jsCoerce r))
  jsCoerce (Sub l r) = Round (Sub (jsCoerce l) (jsCoerce r))
  jsCoerce (Mul l r) = Round (Mul (jsCoerce l) (jsCoerce r))
  jsCoerce (Div l r) = Round (Div (jsCoerce l) (jsCoerce r))
  jsCoerce v = v

instance JsCoerce Double where
  jsCoerce (Add l r) = UnaryPlus (Add (jsCoerce l) (jsCoerce r))
  jsCoerce (Sub l r) = UnaryPlus (Sub (jsCoerce l) (jsCoerce r))
  jsCoerce (Mul l r) = UnaryPlus (Mul (jsCoerce l) (jsCoerce r))
  jsCoerce (Div l r) = UnaryPlus (Div (jsCoerce l) (jsCoerce r))
  jsCoerce v = v


data JsFunction a = JsFunction
  { jsFunctionName :: Text
  , jsFunctionExpr :: JsExpr a }

data JsModule a = JsModule
  { jsModuleName     :: Text
  , jsModuleFunction :: JsFunction a }


mkJsFunction :: (JsShow a, JsCoerce a) => JsFunction a -> Text
mkJsFunction f = "function " <> jsFunctionName f <> "() { return "
                             <> (jsReify . jsCoerce $ jsFunctionExpr f)
                             <> "}"

mkJsModule :: (JsShow a, JsCoerce a) => JsModule a -> Text
mkJsModule m = header <> "{" <> "\"use asm\"; return {"
                      <> exports
                      <> "}; }"
  where
    header = "function " <> jsModuleName m <> "(stdlib, foreign, heap)"
    exports = (\ f -> jsFunctionName f <> ":" <> mkJsFunction f) $ jsModuleFunction m


e0 :: JsExpr Int
e0 = Add (Val 3) (Mul (Val 2) (Val 5))

e1 :: JsExpr Int
e1 = Mul (Div (Val 2) (Val 3)) (Div (Val 3) (Val 2))

f0 :: JsFunction Int
f0 = JsFunction "f0" e0

f1 :: JsFunction Int
f1 = JsFunction "f1" e1

m0 :: JsModule Int
m0 = JsModule "m0" f0

m1 :: JsModule Int
m1 = JsModule "m1" f1


main :: IO ()
main = do
  putText $ mkJsModule m0
  putText $ mkJsModule m1
