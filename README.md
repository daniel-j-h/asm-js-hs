# asm-js-hs

Playground. Nothing to see here. Move along.

Tasks:
- Write internal AST for expressions and code generation from this AST targeting asm.js.
- Write internal asm.js parser for generating an internal AST from asm.js.

# asm.js modules

```javascript
function SimpleModule(stdlib, foreign, heap) {
  "use asm";

  function simpleFunction() {
    return 1;
  }


  return {
    simpleFunction: simpleFunction
  };
}
```

# Resources

- [asm.js Working Draft — 18 August 2014](http://asmjs.org/spec/latest/)

# Notes

There are a couple of decisions we can make in the AST design space.
Here is a small run-down of what I tried and its pros and cons.

I started out with GADTs to explicitly specify the allowed constructor signatures:

```haskell
data Expr a where
  Val :: a -> Expr a
  Add :: Expr a -> Expr a -> Expr a
```

This prevents invalid ASTs to be constructed already at the type level.
Unfortunately constructors such as `Add :: Expr a -> Expr a -> Expr a` prevent us from making `Expr a` even a `Functor` due to `fmap :: (a -> b) -> f a -> f b`.
The GADT constructors are restricted to `b~a`.
For this reason GADTs constrain the AST types too much for intermediate transformations.
In addition if we ever want to parse from text into our AST, GADTs are also too restrictive.

The second design decision is to either put type constraints (think `Show`, `Num`) already into the AST (with GADTs):

```haskell
data Expr a where
  Val :: (Show a, Num a) => a -> Expr a
```

or to have them placed on interpreter functions only:

```haskell
reify :: (Show a) => Expr a -> Text
reify (Val v) = show v
```

without GADTs we have to put constraints on the interpreter functions.

The third design decision is how to dispatch on type `a` in `Expr a`.
We need this for example for coercion (rewriting the AST based on types):
with `Expr a` we need to dispatch on `a~Int`, `a~Float`, `a~Double` and rewrite the AST accordingly.
With GADTs we could make type constructors explicit in the AST

```haskell
IntVal :: Int -> Expr Int
```

or we provide a type class for rewriting based on types

```haskell
class Rewrite a where
  rewrite :: Expr a -> Expr a

instance Rewrite Int where
  rewrite e = rewriteInt e

instance Rewrite Float where
  rewrite e = rewriteFloat e
```

without GADTs the type class approach is what I went with.

If we ever need to change the type during rewriting we can make use of the `MultiParamTypeClasses` extension:

```haskell
class Rewrite2 a b where
  rewrite :: Expr a -> Expr b
```

Note: we then have to be careful for `Rewrite a` and `Rewrite a b` instances not to overlap.
