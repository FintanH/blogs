
# These Changes

Today let us visit a lesser known data type: `These`. I came across it first while working
at Formation. I can't remember the first use of it, but I do remember how I ended up using it
a while later, and a colleague using it in the exact same way; vindication, yes! So what we'll
cover in this post is what the `These` data type is, how it can be used, and how we used it to
manage feature flags & safe code migrations.

## This, That, and the Other

`These` can be thought of as `Either`, but extend it with an extra case where one variant can
hold both values. Code speaks louder than words so here's what it would look like in Haskell:

```haskell
data These a b
  = This a
  | That b
  | These a b
```

Earlier we said that `These` can be thought of as an extended `Either`, so let's examine the
similarities. With `Either` we have the `Left` case which holds a value of type `a` and `Right`
which holds a value of type `b`.

With `These` we have a `This` case which holds a value of type `a` (similar to `Left`) and a
`That` case which holds a value of type `b` (similar to `Right`). We extend our two cases with
one more case, `These`, which holds two values, one of type `a` and one of type `b`.
Another way to think about this is that we're removing the mutual exclusivity that we see using
`Either` by allowing both values to occur at once as well.

## Those Useful Functions

What kind of things can we do with `These`? What's in our toolbox with this new data type?
We have our usual suspects, `These` is a [`Functor`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#t:Functor) so we can use `fmap` on it:

```haskell
instance Functor (These a) where
  fmap f (This a)    = This a
  fmap f (That b)    = That (f b)
  fmap f (These a b) = These a (f b)
```

We also have [`Applicative`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#t:Applicative) and [`Monad`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#t:Monad) instances as long as the `a` is a `Semigroup`:

```haskell
instance Semigroup a => Applicative (These a) where
  pure = That

  This a <*> _            = This a
  That _ <*> This b       = This b
  That f <*> That x       = That (f x)
  That f <*> These b x    = These b (f x)
  These a _ <*> This b    = This (a <> b)
  These a f <*> That x    = These a (f x)
  These a f <*> These b x = These (a <> b) (f x)

instance Semigroup a => Monad (These a) where
  return = pure

  This a >>= _ = This a
  That x >>= f = f x
  These a x >>= f = case f x of
    This b    = This (a <> b)
    That y    = These a y
    These b y = These (a <> b) y
```

What we can glean from this behaviour is that it is short-circuiting when encountering `This`
unless we also encounter a `These`, then we want to gather `a`s while also preserving the
`b` value.

The final useful instance (in my humble opinion) in the `These` toolbox is its [`Bifunctor`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Bifunctor.html#t:Bifunctor)
instance:

```haskell
instance Bifunctor These where
  bimap f _ (This a)    = This (f a)
  bimap _ g (That b)    = That (g b)
  bimap f g (These a b) = These (f a) (g b)
```

This allows to apply functions to transform any of the values inside. We can target the `a`s,
the `b`s, or both.

## Migrating Them

Now that we know how to use `These`, let's talk about one use case for the data type. When I
was working on a couple of features in Formation I was adding functionality to an existing
code path. A healthy dose of paranoia meant that we wanted make these changes so that we could
deploy things in a phased fashion. We would create an enumeration that would look like
the following:

```haskell
data FeatureMigration
  = NoChange   -- ^ Don't use the new code path at all
  | SafeChange -- ^ Use the new code path and fall back on the old path
  | FullChange -- ^ Use the new code path only
```

Let's take an example of what one of these migrations would involve. In one case we were
validating data. Initially this was a simple look up of a `Map` and placing the values into
a domain type. Roughly speaking, we would have a function `validate` like:

```haskell
validate :: Map Key Value -> Either ValidationErrors DomainType
```

Later we wanted to supercharge this validation by inspecting the values and checking invariants
on them. For example, if we were dealing with numbers we would check that they were greater than
zero. The aforementioned paranoia set in and we wanted to ensure that any data that was already
persisted could still be validated. We already had persisted data that got past the previous
validation function but we cannot be sure that it would pass the new validation.

Imagine the following scenario. We add a check in our validation saying that we do not want to
process lists that are longer than 5. Unbeknownst to us we have persisted a list that is of
length 6. This data is in production and is technically still valid. In the future this would
prevented by our validation logic, but for now it lives on. So essentially we want to have
backwards compatibility while we monitor things that go wrong.

So where does `These` come into this? Well we conveniently have a three-way case of calling
code! We want to make sure that any errors that can happen in any of the code paths are handled
and placed in the `This` case. If we are successful in validating without any errors we return
our errors in `That`. The interesting case is where we want to fall back on the old validation
when the new validation fails. If the new code path fails but the old code path succeeds we
will return the result in `These`. Let's look at some pseudo-code to drive this point home.

```haskell
import qualified Validation.Old as Old
import qualified Validation.New as New

data ValidationErrors
  = OldValidationErrors Old.ValidationErrors
  | NewValidationErrors New.ValidationErrors

validate :: FeatureMigration -> Map Key Value -> These ValidationErrors DomainType
validate migration kvs =
  let oldValidation = Old.validate kvs
      newValidation = New.validate kvs

  in case migrations of
      NoChange -> either (This . OldValidationErrors) That oldValidation
      FullChange -> either (This . NewValidationErrors) That newValidation
      SafeChange ->
        either
        -- New validation failed so delegate to old validation
        (\errs -> either (This . OldValidationErrors) (These errs) oldValidation)
        -- New validation succeeded so we're safe
        That
        newValidation
```

From there we can call `validate` and if we want to keep track of any deviations in the
`SafeChange` case, we can log messages and/or metrics:

```haskell
case validate kvs of
  These errs result -> do
    logValidationErrors errs
    updateValidationErrorMetricCount errs
    processResult result
  This errs -> do
    logValidationErrors errs
    err500
  That result ->
    processResult result
```

## Conclusion

So next time you're considering adding some new functionality you want to carefully monitor,
consider using the [`these`](https://hackage.haskell.org/package/these) package and the
technique above. For posterity's we will also link the [`monad-chronicle`](http://hackage.haskell.org/package/monad-chronicle) package which is the Monad Transformer for `These`.
And for some self-promotion, if you're also in the Rust world I ported some of this
functionality in my [`these` crate](https://crates.io/crates/these).
