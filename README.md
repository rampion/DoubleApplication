---
author: Noah Luck Easterly
title: A More General Type for `\f x -> f (f x)`
date: https://github.com/rampion/DoubleApplication
theme: moon
css: README.css
---

### What *is* the type of `\f x -> f (f x)`?

::: notes

This is a literate haskell file, so we need to specify all our `LANGUAGE` 
pragma and `import`s up front. But just because we *need* to doesn't mean we 
need to show them to the reader.

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

:::

---

Bartosz Milewski, "[Limits and Colimits](https://bartoszmilewski.com/2015/04/15/limits-and-colimits)"

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

::: notes

I'm not going to get in to what a pullback is here.

While the given type of `twice` is certainly the [principal type] given normal 
Haskell, the `Rank2Types` language extension makes it possible to give 
other different types to `\f x -> f (f x)`, not compatible with `(a -> a) -> a -> a`.

> An expression's or function's principal type is the least general type that, 
> intuitively, "contains all instances of the expression". For example, the 
> principal type of head is `[a]->a`; `[b]->a`, `a->a`, or even `a` are correct types, 
> but too general, whereas something like `[Integer]->Integer` is too specific. 
> The existence of unique principal types is the hallmark feature of the 
> Hindley-Milner type system, which forms the basis of the type systems of 
> Haskell, ML, Miranda, ("Miranda" is a trademark of Research Software, Ltd.) 
> and several other (mostly functional) languages.

:::

---

What types for `\f x -> f (f x)` does `Rank2Types` enable?

::: notes

Anyone unfamiliar with language paragma?

How about [`Rank2Types`](https://wiki.haskell.org/Rank-N_types)?

> Normal Haskell '98 types are considered Rank-1 types. A Haskell '98 type signature such as
>
>     a -> b -> a
>
> implies that the type variables are universally quantified like so:
>
>     forall a b. a -> b -> a
>
> `forall` can be floated out of the right-hand side of -> if it appears there, so:
>
>     forall a. a -> (forall b. b -> a)
>
> is also a Rank-1 type because it is equivalent to the previous signature.
>
> However, a `forall` appearing within the left-hand side of (`->`) cannot be 
> moved up, and therefore forms another level or rank. The type is labeled 
> "Rank-N" where N is the number of foralls which are nested and cannot be 
> merged with a previous one. For example:
>
>     (forall a. a -> a) -> (forall b. b -> b)
>
> is a Rank-2 type because the latter `forall` can be moved to the start but the former one cannot. Therefore, there are two levels of universal quantification.

:::

---

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

---

```haskell
{-# LANGUAGE ConstraintKinds, AllowAmbiguousTypes #-}
-- ...
constrainedTwice :: (forall a. c a => a -> a) -> c t => t -> t
constrainedTwice f x = f (f x)
```

::: notes

Enabling `ConstraintKinds` and `AllowAmbiguousTypes` allows even more types for `\f x -> f (f x)`.

All of these functions have the same implementation, `\f x -> f (f x)`, yet their types look very different.

What then is a more general type for `\f x -> f (f x)`?

:::

---

What does it mean for two types to be different, or, for that matter, the same?

:::notes

Before we get to that question, there's a more basic question that needs to be 
answered.

:::

---

### Nominal Equality

- `Int ~ Int`
- `Int ≁ Char`

::: notes

In haskell, we compare non-polymorphic types using *nominal equality*; if two 
types have the same name, they are nominally equal. If not, they're not. So 
`Int` is nominally equal to `Int`, but not to `Char`.

:::

---

### Unifiable Types

- `(Int, a)`
- `m (Char -> b)`

::: notes

Parametric types are a little different since some parts of the name (the 
parameters) are missing, so they can't necessarily be tested for nominal 
equality. However, the compiler can still test if two parametric types are 
*unifiable*; that is, determine if there's a substitution of parameters that 
makes the two types equal. For example, `(Int, a)` and `m (Char -> b)` are 
unifiable using the substitutions `m ~ (,) Int` and `a ~ (Char -> b)`, but 
there's no way to unify `(Int, a)` and `[b]`.

:::

---

### Specialization

- `(Int, a) ~> (Int, Char -> b)` via `a ~ Char -> b`
- `m (Char -> b)` via `m ~ `(,) Int`

::: notes

Type *specialization* is a specific sort of type unification where given a type 
**a** and **b**, on esays that type **a** specializes to type **b** if there 
exists a substitution of parameters to type **a** that makes it equal to type 
**b**. In the prior example, though `(Int, a)` and `m (Char -> b)` are 
unifiable, neither type can specialize to the other. Both, though, can 
specialize to `(Int, Char -> b)`.

:::

---

What is a type for `\f x -> f (f x)` that can specialize to any other type for `\f x -> f (f x)`?

::: notes

Bringing us back to the notion of a principal type.

:::

---

### Comparing types using GHC

```haskell
constrainedTwice_using_twice :: (forall a. c a => a -> a) -> c t => t -> t
constrainedTwice_using_twice = twice
```

::: notes

GHC is pretty good at type unification and specialization. For example, the 
type of `twice` can be specialized to the type of `constrainedTwice`:

:::

---

```
ghci> :{
  ... twice_using_constrainedTwice :: (t -> t) -> t -> t
  ... twice_using_constrainedTwice = constrainedTwice
  ... :}

<interactive>:152:32: error:
    * Could not deduce: c0 t arising from a use of `constrainedTwice`
...
<interactive>:152:32: error:
    * Could not deduce: t ~ a
...
```

::: notes

However, GHC can't specialize the type of `constrainedTwice` to that of `twice` without assistance.

Part of the problem is that GHC can not automatically infer what the type 
variable `c` in `constrainedTwice`'s type should be (this is the ambiguity 
`AllowAmbiguousTypes` allowed), and so doesn't know whether the outer type 
parameter `t` satisfies it. Another part is that it doesn't know how to unify 
`forall a. c a => a -> a` and `t -> t` without assuming `t ~ a`.

The constraint `t ~ a` can also be written in prefix form as `(~) t a`, which 
gives an elegant solution to both problems. If GHC can be told to substitute 
`(~) t` for `c`, then not only is the ambiguity resolved, but `forall a. (t ~ a) => a -> a` 
can be unified with `t -> t`.

:::

---

Substitute `(~) t` for `c`:

```haskell
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
-- ...
twice_using_constrainedTwice :: forall t. (t -> t) -> t -> t
twice_using_constrainedTwice = constrainedTwice @((~) t)
```

::: notes

GHC's `TypeApplications` language extension adds an `@` operator to perform 
such substitutions. This can be used to give GHC the hint it needs to 
specialize the type of `constrainedTwice` to that of `twice`.

:::

---

### A more restricted function

```haskell
restrictedTwice :: (forall a. a -> a) -> t -> t
restrictedTwice f x = f (f x)
```

::: notes

As a step towards discovering a more general type, consider a more specific 
one. `\f x -> f (f x)` can also be applied to a family of functions `forall a. a -> a`.


The only value of type `forall a. a -> a` that doesn't involve bottom is `id`, 
so `restrictedTwice` isn't a very useful function, but the type of 
`restrictedTwice` *is* unifiable with that of `twice`:

:::

---

```haskell
restrictedTwice_using_twice :: (forall a. a -> a) -> t -> t
restrictedTwice_using_twice = twice
```

---

```haskell
{-# LANGUAGE FlexibleInstances #-}
-- ...
class Any a
instance Any a

restrictedTwice_using_constrainedTwice :: (forall a. a -> a) -> t -> t
restrictedTwice_using_constrainedTwice = constrainedTwice @Any
```

::: notes

The type of `restrictedTwice` is also almost identical to that of 
`constrainedTwice`, but GHC again needs some help to specialize the type of the 
latter to that of the former.

:::

---

```haskell
-- import Data.Functor.Identity
-- ...
-- doubleWrap :: (forall a. a -> m a) -> t -> m (m t)
restrictedTwice_using_doubleWrap :: (forall a. a -> a) -> t -> t
restrictedTwice_using_doubleWrap f = runIdentity . runIdentity . doubleWrap (Identity . f)

-- doubleUnwrap :: (forall a. w a -> a) -> w (w t) -> t
restrictedTwice_using_doubleUnwrap :: (forall a. a -> a) -> t -> t
restrictedTwice_using_doubleUnwrap f = doubleUnwrap (f . runIdentity) . Identity . Identity
```

::: notes

Even though the type of `restrictedTwice` can't be specialized from the types 
of `doubleWrap` or `doubleUnwrap`, it can be implemented in terms of either of 
them using the `Identity` functor.

Here `Identity` is used to substitute for the type constructor `m` in 
`doubleWrap`'s type `(forall a. a -> m a) -> t -> m (m t)` and for the type 
constructor `w` in `doubleUnwrap`'s type `(forall a. w a -> a) -> w (w t) -> t`, 
yet the wrapping and unwrapping it introduces obscures the relation between 
the types of `restrictedTwice` and `doubleWrap` or `doubleUnwrap`.

:::

---

What if we could skip the wrapping and unwrapping?

```haskell
type Is a = a
```

```
ghci> :t doubleWrap @Is

<interactive>:1:1: error:
    * The type synonym 'Is' should have 1 argument, but has been given none
```

::: notes

Consider the type synonym `Is`:

If it were possible to use the unsaturated type synonym `Is` as a substitute 
for `m` in `doubleWrap`'s type or `w` in `doubleUnwrap`'s type, then `Is (Is a) ~ a` 
would follow for free, without all that wrapping and unwrapping.  However, 
unsaturated type synonyms like `Is` can't be passed arround in 
Haskell. Attempting to do so results in a compile error. 

This limitation can be overcome using [**defunctionalization**](https://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/).

:::

---

### Type level functions:

1. type synonyms; must be saturated, but can resolve to any type of any kind
2. type constructors; need not be saturated, but must resolve to a paramterized type of kind `*`.

::: notes

In broad strokes, there are two sorts of type-level functions in Haskell:

- *type synonyms* defined by `type` (like `Is` above). These can't be passed around unsaturated, but saturated type synonyms can evaluate to arbitrary types of any kind.

- *type constructors* defined by `data` or `newtype` (e.g. `IO`, `Maybe` or `Either`). These may be passed around unsaturated, but type constructors must eventually evaluate to parameterized types of kind `Type` (aka `*`).

:::

---

### Defunctionalization

The best of both worlds:

- Use a type constructor as a type-level function *identifier*
- Use a type family to map an identifier to an *implementation*.

::: notes

Defunctionalization uses the best of both varieties of type-level functions while bypassing the limitations of each.

- Define a type constructor to use as an *identifier* for a type-level function; allowing the type-level function to be passed around unsaturated.

- Use a type family to match type-functions' identifiers with their implementations; allowing the type-level function to evaluate to types of any kind.

:::

---

A kind for identifiers of type-level functions:

```haskell
{-# LANGUAGE TypeOperators #-}
-- ...
type a --> b = (b -> Type) -> a -> Type
infixr 0 -->
```

::: notes

In Haskell, strongly-typed defunctionalization can be implemented with only a 
slight syntatic cost.

First, define a kind `a --> b` for identifier types for type-functions that map 
a type of kind `a` to a type of kind `b`:

The choice of implementation of `a --> b` here is thoroughly arbitrary. 
`(a,b,b) -> Type` or `Either b a -> Type` or `a -> b -> Type` all work just as 
well, as do infinitely many others. As we will never be saturating the type, 
all that matters on the right-hand side of the definition is:

- it mentions `a` and `b`, so they're still in scope for typechecking the implementation, and
- it's a function to kind `Type`, so `a --> b` can be used as a kind for unsaturated type constructors.

:::

---

An open type family for implementations of type-level functions:

```haskell
{-# LANGUAGE PolyKinds, DataKinds, TypeInType, TypeFamilies #-}
-- ...
type family ($) (f :: a --> b) (x :: a) :: b
infixr 0 $
```

::: notes

An open type family can be used to connect an identifier type with its 
implementation. Since types and values have different namespaces in Haskell, 
this type family is here named `$`, punning off the use of `$` at the value 
level for application.

:::

---

### Type-level functions vs value-level functions

```haskell
data ID :: a --> a
type instance ID $ a = a
```

vs

```haskell
id :: a -> a
id a = a
```

::: notes

Defining type-level functions is now very similar to defining value-level functions.

- Declare the type-level function's identifier type and its kind on one line.
- Define the type-level function's implementation on the next line.

:::

---

```haskell
data CONST :: c -> a --> c
type instance (CONST c) $ _ = c
```

vs

```haskell
const :: c -> a -> c
const c _ = c
```

---

Lifting type constructors to identifiers:
```haskell
data LIFT :: (a -> b) -> a --> b
type instance (LIFT f) $ a = f a
```

::: notes

Another useful identifier is `LIFT f`, which lifts a type constructor `f :: a -> b` 
to a type-level function identifier of kind `a --> b`.

:::

---

Using `ID` and `LIFT`:

```
doubleWrap :: (forall a. a -> m a) -> t -> m (m t)
doubleWrap :: (forall a. a -> LIFT m $ a) -> t -> LIFT m $ LIFT m $ t
doubleWrap :: (forall a. ID $ a -> LIFT m $ a) -> ID $ t -> LIFT m $ LIFT m $ t

doubleUnwrap :: (forall a. w a -> a) -> w (w t) -> t
doubleUnwrap :: (forall a. LIFT w $ a -> a) -> LIFT w $ LIFT w $ t -> t
doubleUnwrap :: (forall a. LIFT w $ a -> ID $ a) -> LIFT w $ LIFT w $ t -> ID $ t

constrainedTwice :: (forall a. c a => a -> a) -> c t => t -> t
constrainedTwice :: (forall a. LIFT c $ a => a -> a) -> LIFT c $ t => t -> t
constrainedTwice :: (forall a. LIFT c $ a => ID $ a -> ID $ a) -> LIFT c $ t => ID $ t -> ID $ t
```

::: notes

Applying defunctionalization to the types of `doubleWrap`, `doubleUnwrap`, and 
`constrainedTwice` we can expand their types until we can see them all as 
specializations of a common type.

It can be seen from looking at these types that the various type-level function 
identifiers only interfere with each other by introducing a constraint that the 
`output` of the first application of `f` must be a possible `input` to the 
second.

:::

---

```haskell
selfCompose ::
  forall constrain input output v w.
  (forall u. constrain $ u => input $ u -> output $ u) ->
  (constrain $ v, (output $ v) ~ (input $ w), constrain $ w) =>
  input $ v -> output $ w
selfCompose f x = f @w (f @v x)
```

---

``` haskell
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

::: notes

Furthermore, the type of `selfCompose` turns out to be sufficiently general to
specialize with all the other types of `\f x -> f (f x)` given above

:::

---

### Is this the principal type of `\f x -> f (f x)`?

Probably not, but it's closer than `(a -> a) -> a -> a`.

---

Dave Feuer's example:

> The trouble is that you can't have quantifiers in the RHS of a type family instance.

```haskell
foob :: (forall a b. (forall f. f a -> f b) -> (forall g c. g (a, c) -> g (b, c)))
     -> (forall a b. (forall f. f a -> f b) -> (forall g c d. g ((a, c), d) -> g ((b, c), d)))
foob f = \x -> f (f x)
```

---

Using Stephen Dolan's [Algebraic Subtyping](https://www.cl.cam.ac.uk/~sd601/thesis.pdf), we could give the type as:

```
(a -> a ∩ b) -> a -> b
```

::: notes

This isn't strictly haskell, but an alternate type system that Dolan proposes.

:::

---

### Questions?
