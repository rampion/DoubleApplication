What is the type of `\f x -> f (f x)`?
--------------------------------------

<!--
# Setup

This is a literate haskell file, so we need to specify all our `LANGUAGE` pragma and `import`s up front. But just because we *need* to doesn't mean we need to show them to the reader.

Thus, an HTML comment.

```haskell
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeInType, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module README where
import Data.Functor.Identity (Identity(..))
import GHC.Types (Type)

twice :: (t -> t) -> t -> t
twice f x = f (f x)
```
-->

In the blog post "[Limits and Colimits](https://bartoszmilewski.com/2015/04/15/limits-and-colimits)", Bartosz Milewski defines the category theoretic concept of a "pullback", and states that a pullback can be used to infer the type of `\f x -> f (f x)`:

> For instance, suppose that the compiler wants to infer the type of a function:
>
>     twice f x = f (f x)
>
> **[...]**
>
> The most general substitution is obtained using a pullback. I won't go into the details, because they are beyond the scope of this book, but you can convince yourself that the result should be:
>
>     twice :: (t -> t) -> t -> t
>
> with `t` a free type variable.

While the given type of `twice` is certainly the most general type for normal Hasekll, the `Rank2Types` language extension makes it possible to give many other different types to `\f x -> f (f x)`.

As a non-exhaustive list, consider:

```haskell
{-# LANGUAGE Rank2Types #-}
-- ...
doubleWrap :: (forall a. a -> m a) -> t -> m (m t)
doubleWrap f x = f (f x)

doubleUnwrap :: (forall a. w a -> a) -> w (w t) -> t
doubleUnwrap f x = f (f x)

pairAndRepair :: (forall a. a -> (a,a)) -> t -> ((t,t),(t,t))
pairAndRepair f x = f (f x)

lookupBoth :: (forall a b. c a => (e, a -> b) -> (e, b)) -> (c i, c j) => (e, i -> j -> k) -> (e, k)
lookupBoth f x = f (f x)

doubleRotation :: (forall a0 a1 a2. m a0 a1 a2 -> m a1 a2 a0) -> m t0 t1 t2 -> m t2 t0 t1
doubleRotation f x = f (f x)
```

Enabling `ConstraintKinds` and `AllowAmbiguousTypes` allows even more types for `\f x -> f (f x)`, for example:

```haskell
{-# LANGUAGE ConstraintKinds, AllowAmbiguousTypes #-}
-- ...
constrainedTwice :: (forall a. c a => a -> a) -> c t => t -> t
constrainedTwice f x = f (f x)
```

All of these functions have the same implementation, `\f x -> f (f x)`, yet their types look very different.

What then is a more general type for `\f x -> f (f x)`?

### Type unification and specialization

Before we get to that question, there's a more basic question that needs to be answered. What does it mean for two types to be different, or, for that matter, the same?

In haskell, we compare non-polymorphic types using *nominal equality*; if two types have the same name, they are nominally equal. If not, they're not. So `Int` is nominally equal to `Int`, but not to `Char`.

Parametric types are a little different since some parts of the name (the parameters) are missing, so they can't necessarily be tested for nominal equality. However, the compiler can still test if two parametric types are *unifiable*; that is, determine if there's a substitution of parameters that makes the two types equal. For example, `(Int, a)` and `m (Char -> b)` are unifiable using the substitutions `m ~ (,) Int` and `a ~ (Char -> b)`, but there's no way to unify `(Int, a)` and `[b]`.

Type *specialization* is a specific sort of type unification where given a type **a** and **b**, on esays that type **a** specializes to type **b** if there exists a substitution of parameters to type **a** that makes it equal to type **b**. In the prior example, though `(Int, a)` and `m (Char -> b)` are unifiable, neither type can specialize to the other. Both, though, can specialize to `(Int, Char -> b)`.

The basic question then becomes "What is a type for `\f x -> f (f x)` that can specialize to any other type for `\f x -> f (f x)`?

### Comparing types using GHC

GHC is pretty good at type unification and specialization. For example, the type of `twice` can be specialized to the type of `constrainedTwice`:

```haskell
constrainedTwice_using_twice :: (forall a. c a => a -> a) -> c t => t -> t
constrainedTwice_using_twice = twice
```

However, GHC can't specialize the type of `constrainedTwice` to that of `twice` without assistance:

```
ghci> :{
  ... twice_using_constrainedTwice :: (t -> t) -> t -> t
  ... twice_using_constrainedTwice = constrainedTwice
  ... :}

<interactive>:152:32: error:
    * Could not deduce: c0 t arising from a use of `constrainedTwice` ##FIXME *, quotes
...
<interactive>:152:32: error:
    * Could not deduce: t ~ a
...
```

Part of the problem is that GHC can not automatically infer what the type variable `c` in `constrainedTwice`'s type should be (this is the ambiguity `AllowAmbiguousTypes` allowed), and so doesn't know whether the outer type parameter `t` satisfies it. Another part is that it doesn't know how to unify `forall a. c a => a -> a` and `t -> t` without assuming `t ~ a`.

The constraint `t ~ a` can also be written in prefix form as `(~) t a`, which gives an elegant solution to both problems. If GHC can be told to substitute `(~) t` for `c`, then not only is the ambiguity resolved, but `forall a. (t ~ a) => a -> a` can be unified with `t -> t`.

GHC's `TypeApplications` language extension adds an `@` operator to perform such substitutions. This can be used to give GHC the hint it needs to specialize the type of `constrainedTwice` to that of `twice`:

```haskell
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
-- ...
twice_using_constrainedTwice :: forall t. (t -> t) -> t -> t
twice_using_constrainedTwice = constrainedTwice @((~) t)
```

### A more restricted function

As a step towards discovering a more general type, consider a more specific one. `\f x -> f (f x)` can also be applied to a family of functions `forall a. a -> a`. 

```haskell
restrictedTwice :: (forall a. a -> a) -> t -> t
restrictedTwice f x = f (f x)
```

The only value of type `forall a. a -> a` that doesn't involve bottom is `id`, so `restrictedTwice` isn't a very useful function, but the type of `restrictedTwice` *is* unifiable with that of `twice`:

```haskell
restrictedTwice_using_twice :: (forall a. a -> a) -> t -> t
restrictedTwice_using_twice = twice
```

The type of `restrictedTwice` is also almost identical to that of `constrainedTwice`, but GHC again needs some help to specialize the type of the latter to that of the former.

```haskell
{-# LANGUAGE FlexibleInstances #-}
-- ...
class Any a
instance Any a

restrictedTwice_using_constrainedTwice :: (forall a. a -> a) -> t -> t
restrictedTwice_using_constrainedTwice = constrainedTwice @Any
```

Even though the type of `restrictedTwice` can't be specialized from the types of `doubleWrap` or `doubleUnwrap`, it can be implemented in terms of either of them using the `Identity` functor.

```haskell
-- import Data.Functor.Identity
-- ...
restrictedTwice_using_doubleWrap :: (forall a. a -> a) -> t -> t
restrictedTwice_using_doubleWrap f = runIdentity . runIdentity . doubleWrap (Identity . f)
-- restrictedTwice_using_doubleWrap = coerce . doubleWrap @Identity . coerce
-- restrictedTwice_using_doubleWrap = coerce (doubleWrap @Identity)

restrictedTwice_using_doubleUnwrap :: (forall a. a -> a) -> t -> t
restrictedTwice_using_doubleUnwrap f = doubleUnwrap (f . runIdentity) . Identity . Identity
-- restrictedTwice_using_doubleUnwrap f = coerce . doubleUnwrap @Identity . coerce
```

Here `Identity` is used to substitute for the type constructor `m` in `doubleWrap`'s type `(forall a. a -> m a) -> t -> m (m t)` and for the type constructor `w` in `doubleUnwrap`'s type `(forall a. w a -> a) -> w (w t) -> t`, yet the wrapping and unwrapping it introduces obscures the relation between the types of `restrictedTwice` and `doubleWrap` or `doubleUnwrap`.

Consider the type synonym `Is`:

```haskell
type Is a = a
```

If it were possible to use the unsaturated type synonym `Is` as a substitute for `m` in `doubleWrap`'s type or `w` in `doubleUnwrap`'s type, then `Is (Is a) ~ a` would follow for free, without all that wrapping and unwrapping. However, unsaturated type synonyms like `Is` can't be passed arround in Haskell. Attempting to do so results in a compile error:

```
ghci> :t doubleWrap @Is

<interactive>:1:1: error:
    * The type synonym 'Is' should have 1 argument, but has been given none #FIXME
```

This limitation can be overcome using [**defunctionalization**](https://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/).

### Defunctionalization

In broad strokes, there are two sorts of type-level functions in Haskell:

- *type synonyms* defined by `type` (like `Is` above). These can't be passed around unsaturated, but saturated type synonyms can evaluate to arbitrary types of any kind.

- *type constructors* defined by `data` or `newtype` (e.g. `IO`, `Maybe` or `Either`). These may be passed around unsaturated, but type constructors must eventually evaluate to parameterized types of kind `Type` (aka `*`).

Defunctionalization uses the best of both varieties of type-level functions while bypassing the limitations of each.

- Define a type constructor to use as an *identifier* for a type-level function; allowing the type-level function to be passed around unsaturated.

- Use a type family to match type-functions' identifiers with their implementations; allowing the type-level function to evaluate to types of any kind.

In Haskell, strongly-typed defunctionalization can be implemented with only a slight syntatic cost.

First, define a kind `a --> b` for identifier types for type-functions that map a type of kind `a` to a type of kind `b`:

```haskell
{-# LANGUAGE TypeOperators #-}
-- ...
type a --> b = (b -> Type) -> a -> Type
infixr 0 -->
```

The choice of implementation of `a --> b` here is thoroughly arbitrary. `(a,b,b) -> Type` or `Either b a -> Type` or `a -> b -> Type` all work just as well, as do infinitely many others. As we will never be saturating the type, all that matters on the right-hand side of the definition is:

- it mentions `a` and `b`, so they're still in scope for typechecking the implementation, and

- it's a function to kind `Type`, so `a --> b` can be used as a kind for unsaturated type constructors.

An open type family can be used to connect an identifier type with its implementation. Since types and values have different namespaces in Haskell, this type family is here named `$`, punning off the use of `$` at the value level for application.

```haskell
{-# LANGUAGE PolyKinds, DataKinds, TypeInType, TypeFamilies #-}
-- ...
type family ($) (f :: a --> b) (x :: a) :: b
infixr 0 $
```

Defining type-level functions is now very similar to defining value-level functions.

- Declare the type-level function's identifier type and its kind on one line.

- Define the type-level function's implementation on the next line.

For instance, compare

```haskell
data ID :: a --> a
type instance ID $ a = a
```

with

```haskell
id :: a -> a
id a = a
```

or

```haskell
data CONST :: c -> a --> c
type instance (CONST c) $ _ = c
```

with

```haskell
const :: c -> a -> c
const c _ = c
```

Another useful identifier is `LIFT f`, which lifts a type constructor `f :: a -> b` to a type-level function identifier of kind `a --> b`.

```haskell
data LIFT :: (a -> b) -> a --> b
type instance (LIFT f) $ a = f a
```

(More discussion of these type-level function identifiers can be found in Appendix C)

### Unifying the types of `doubleWrap`, `doubleUnwrap`, and `constrainedTwice`

Applying defunctionalization to the types of `doubleWrap`, `doubleUnwrap`, and `constrainedTwice` gives:

- `(forall a. a -> output $ a) -> t -> output $ output $ t`
- `(forall a. input $ a -> a) -> input $ input $ t -> t`
- `(forall a. constrain $ a => a -> a) -> constrain $ t => t -> t`

It can be seen from looking at these types that the various type-level function identifiers only interfere with each other by introducing a constraint that the `output` of the first application of `f` must be a possible `input` to the second. ###FIXME REWRITE

Making that constraint explicit enables the types of these functions all to have a common generalization, `selfCompose`:

```haskell
selfCompose ::
  forall constrain input output v w.
  (forall u. constrain $ u => input $ u -> output $ u) ->
  (constrain $ v, (output $ v) ~ (input $ w), constrain $ w) =>
  input $ v -> output $ w
selfCompose f x = f @w (f @v x)

-- ghci> :t selfCompose @(LIFT Any) @ID @(LIFT _)
-- selfCompose @(LIFT Any) @ID @(LIFT _)
--   :: (forall u. Any u => u -> t u) -> v -> t (t v)
doubleWrap_using_selfCompose :: forall m t. (forall a. a -> m a ) -> t -> m (m t)
doubleWrap_using_selfCompose = selfCompose @(LIFT Any) @ID @(LIFT m)

-- ghci> :t selfCompose @(LIFT Any) @(LIFT _) @ID
-- selfCompose @(LIFT Any) @(LIFT _) @ID
--   :: (forall u. Any u => t u -> u) -> t (t w) -> w
doubleUnwrap_using_selfCompose :: forall w t. (forall a. w a -> a) -> w (w t) -> t
doubleUnwrap_using_selfCompose = selfCompose @(LIFT Any) @(LIFT w) @ID

-- ghci> :t selfCompose @(LIFT _) @ID @ID
-- selfCompose @(LIFT _) @ID @ID
--   :: (forall u. t u => u -> u) -> w -> w
constrainedTwice_using_selfCompose :: forall c t. (forall a. c a => a -> a) -> c t => t -> t
constrainedTwice_using_selfCompose = selfCompose @(LIFT c) @ID @ID
```

Furthermore, the type of `selfCompose` turns out to be sufficiently general to specialize with all the other types of `\f x -> f (f x)` given above (see Appendixes A and B).

### dfeuer example

```haskell
foob :: (forall a b. (forall f. f a -> f b) -> (forall g c. g (a, c) -> g (b, c)))
     -> (forall a b. (forall f. f a -> f b) -> (forall g c d. g ((a, c), d) -> g ((b, c), d)))
foob f = \x -> f (f x)

fooc :: forall b. (forall a. (forall f. f a -> f b) -> (forall g c. g (a, c) -> g (b, c)))
               -> (forall a. (forall f. f a -> f b) -> (forall g c d. g ((a, c), d) -> g ((b, c), d)))
fooc f = \x -> f (f x)

foob' :: (forall a b. (forall f. f a -> f b) -> (forall g c. g (a, c) -> g (b, c)))
      -> (forall a b. (forall f. f a -> f b) -> (forall g c d. g ((a, c), d) -> g ((b, c), d)))
foob' = selfCompose @(LIFT Any) @SRC @DST

  -- (forall u. constrain $ u => (forall v. input v $ u) -> (forall w. output w $ u) ->
data SRC :: (k,k) --> Type
type instance SRC $ p = forall f. f (FST $ p) -> f (SND $ p)

data DST :: (k,k) --> Type
type instance DST $ p = forall g c. g (FST $ p, c) -> g (SND $ p, c)

data FST :: (a,b) --> a
type instance FST $ '(a,b) = a

data SND :: (a,b) --> b
type instance SND $ '(a,b) = b
```

### FIXME Conclusion?

### FIXME is constrain redundant? implement w/ output?
