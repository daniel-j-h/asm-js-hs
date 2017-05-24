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

GADTs allow us to explicitly specify the allowed constructor signatures.
Still constructors such as `Add :: Expr a -> Expr a -> Expr a` prevent us from making `Expr a` even a `Functor` because `fmap :: (a -> b) -> f a -> f b` but we are restricted to `b~a`.
`Functor`, `Foldable` and `Traversable` would allow for easy tree walking.

Coercion needs to dispatch on the tree's leaf types (with `Expr a` we need to dispatch e.g. on `a~Int`, `a~Float`, `a~Double`).
The easiest way to allow for this - but certainly not the most beautiful - is making typed constructors explicit in the AST: `IntVal :: Int -> Expr Int`.
