# Compose Tetris

#### In the Beginning there was Function Composition

So let's talk composition. As programmers we see composition constantly.
It's there even if you're not aware. If we have two functions
`f` and `g` and we apply them one after the other, we're doing
function composition.

```haskell
f (g (x))
```

or in mathematical notation:

```haskell
(f ∘ g)(x)
```

In Haskell this is so common that we have an operator for it:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
```

It's fun to note that composition is associative. Meaning if we have
three functions `f`, `g`, and `h` then it doesn't
matter what order we compose them in:

```haskell
 (f . g) . h
= f . (g . h)
= f . g . h
```

And here's an example use of function composition in Haskell:

```haskell
capitaliseAllWords :: String -> String
capitaliseAllWords = unwords . map capitaliseWord . words
  where capitaliseWord = map toUpper
```

We can read this right-to-left: splitting the incoming
`String` into separate words, i.e.
`[String]`, then
`map capitaliseWord` over this list, and
concatenate the list back into `String`.
A simple and concise example to wet our whistles.

#### Take me Higher and Higher!

We can take the idea of composition to the next level. Before this
though, we have to quickly go over higher-kinded types. Higher-kinded
types are types that have a parameter. This means they are expecting
types so that they can be a regular, happy type. Think of it like a type
function:

```haskell
Type         -- Regular type
Type -> Type -- Higher-kinded type expecting one Type
```

Without further ado, let's look at examples! The first higher-kinded
type that we will look at is the `Maybe`
type. It goes by a few names but here is its definition in Haskell:

```haskell
data Maybe a = Nothing | Just a
```

When we look at the left-hand side of the `=` we see that the `Maybe`
type has a parameter called`a`. We can
fill the `a` with other types, e.g.
`Maybe Int`, `Maybe String`, `Maybe (Maybe Bool)`.

Another common type that is also higher-kinded is the list type:

```haskell
data [a] = [] | a : [a]
```

Again, we have an `a` parameter that the
list is waiting for to be filled. **Exercise:** Using
the `:kind` command in the ghci-repl,
explore the kind of the `[]` data type.

We can also talk about things that take a type parameter more
abstractly. We can say there is some type constructor `f` that takes a type parameter `a`:

```haskell
f a
```

With that covered we can talk about a type called
`Compose`, defining it here as:

```haskell
newtype Compose f g a = Compose
```

It takes to two higher-kinded types `f`
and `g` and a type parameter
`a` and gives us `f` and `g` composed. This
looks a lot like our function composition right?!

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
            ^^^^^^^ COMPOSITION!!!
```

"So, what's special about this new type we've introduced?", I hear you
ask. I'll tell you what! We can define functions over the composition of
these two things, but abstractly! We can compose two
`Functor`s and two
`Applicative`s. These commonly trip
people up when exploring this. Especially since the nesting of two the
`f` and `g` can make for some difficult type tetris when trying to
implement `(<*>)` for two
`Compose` values. I want to provide some
further intuition on how to come to the solution to writing these
instances.

But before you read on, I would encourage you to fire up your ghci repl,
open up your favourite editor and try implementing these. If you
*reaaallllly* get stuck then you can read on, but learn by doing first 😄

**Exercise**: Implement the `Functor` and
`Applicative` instance for
`Compose`.

#### Functors on Functors on Functors

We'll start off with talking about composing two functors. To begin
we'll look at the instance declaration:

```haskell
instance (Functor f, Functor g) => Functor (Compose f g) where
```

As we've said, we're composing two `Functor`s so it would make sense that we rely on
`f` and `g` being instances of `Functor`. The aim here is to change the `a`, nested within our `f`
and `g`, into a `b`. This is more clear when we take a look at the
signature for `fmap` specialised to
`Compose`.

```haskell
fmap :: (a -> b) -> Compose f g a -> Compose f g b
```

This gives us an idea of where to begin when trying to implement
`fmap`. We'll need to introduce the
function `a -> b` and the
`Compose f g` value. We can also use
pattern matching to unwrap the `f (g a)`
inside the `Compose`.

```haskell
fmap f (Compose fg) = _full_solution
```

Armed with the knowledge that we want to change the innermost value
`a`, we will approach the problem by
thinking inside-out. This method of working from the inside-out will be
reused throughout the implementations and will help us think about
implementing these instances.

The innermost thing we can work with is `a`, but this is trivial since we know that turning the
`a` into a `b` can be done by using the function `f`.

The next level up we are looking at the `g`, more specifically `g a`. So how can we go from a `g a` to a `g b`?

Alarm bells 🚨🚨🚨 should be ringing in your head here because we know
`g` is a `Functor` and we know how to get a function
`g a -> g b` by using
`fmap`. So we end up with:

```haskell
fmap f (Compose fg) = _full_solution
  where
    -- gb :: g a -> g b
    gb ga = fmap f ga
```

So now we need to move to the next layer up and figure out how to turn
our `f (g a)` into a
`f (g b)`. Inspecting the types of what
we have at our fingertips will reveal how to get past this hurdle:

```haskell
fg :: f (g a)     -- The value inside the Compose
gb :: g a -> g b  -- The function we defined
```

Again we see a familiar pattern of trying to access the inner part, in
this case we want to access the `g a`
inside of the `f`. `fmap` is our friend, once again, and the
`(a -> b)` in this case is specialised to
`(g a -> g b)`. That is to say
`fmap` looks like the following for
`f`:

```haskell
fmap :: (g a -> g b) -> f (g a) -> f (g b)
```

And here's our solution for `fmap` for
the `Compose` `Functor`:

```haskell
fmap f (Compose fg) = Compose $ fmap gb fg
  where
    -- gb :: g a -> g b
    gb ga = fmap f ga
```

We can reduce this to show a cleaner solution by replacing
`gb` with its definition (thank you
equational reasoning 🙌🏼):

```haskell
fmap f (Compose fg) = Compose $ fmap (fmap f) fg
```

and the unwrapping of `Compose` with the
function `unCompose`:

```haskell
fmap f fg = Compose $ fmap (fmap f) $ (unCompose fg)
```

From there we can see function composition falling into place by
removing `fg`:

```haskell
fmap f = Compose . fmap (fmap f) . unCompose
```

To concretise the idea of our higher-kinded composition we can see how
the composition of two `Functor`s is just
the two `fmap` functions coming together
and forming our one `fmap` function for
`Compose`. Neat!

Let's test out our implementation by choosing the two functors we
mentioned earlier: `[]` and
`Maybe`.

```haskell
λ> unCompose $ (+1) <$> Compose [Just 1, Just 2, Nothing]
[Just 2, Just 3, Nothing]
```

#### Mind melting Applicative

Going up the chain of typeclasses we will take a look at the composition
of `Applicative`s. Again, we can start
off with a similar instance declaration:

```haskell
instance (Applicative f, Applicative g)
  => Applicative (Compose f g) where
```

The `Applicative`[typeclass](https://hackage.haskell.org/package/base-4.11.1.0/docs/Prelude.html#t:Applicative) has two functions associated with its definition:
`pure` and `(<*>)` (also `liftA2`, but we
won\'t look at that here). We will go after the easier game first and
write the `pure` implementation.
Specialising `pure` to
`Compose` we get the following function
signature:

```haskell
pure :: a -> Compose f g a
```

Again, we can work with our intuition of working from the inside-out. So
how can we get a `g a`? You guessed it,
`pure`!

```haskell
pure a = _full_solution
  where
    -- ga :: g a
    ga = pure a
```

Next, how can we get an `f (g a)`,
pfffttt you got this!

```haskell
pure a = _full_solution
  where
    -- fga :: f (g a)
    fga = pure ga
```

```haskell
    -- ga :: g a
    ga  = pure a
```

And to finish it all off we wrap our results in `Compose`:

```haskell
pure a = Compose fga
  where
    -- fga :: f (g a)
    fga = pure ga
```

```haskell
    -- ga :: g a
    ga  = pure a
```

In the same vein as `fmap` we can work
back to a cleaner solution. The first step is to exchange
`ga` for its definition:

```haskell
pure a = Compose fga
  where
    fga = pure (pure a)
```

We can then do the same with `fga`:

```haskell
pure a = Compose (pure (pure a))
```

Now we can really see the composition shine by dropping the
`a` and use the composition of these
functions to define `pure`:

```haskell
pure = Compose . pure . pure
```

😍

Now, here's the part that has tripped up many of us in the past. We'll
define `(<*>)` for
`Compose`. We are going to use a
type-hole driven development approach alongside the idea of working from
the inside out to converge on the solution for this function. But first,
let's look at the type signature:

```haskell
(<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
```

We can start off by unwrapping the two `Compose` values:

```haskell
Compose f <*> Compose k = _full_solution
```

And it's important to keep track of the types we're working with from
here:

```haskell
f :: f (g (a -> b))
k :: f (g a)
```

It's good to also note that when utilising type holes GHC will provide
us with relevant bindings in scope, as well.

Starting from the inner part first we will just consider the
`g` part of the types. If we remove the
`f` we end up with wanting an expression
of type:

```haskell
g (a -> b) -> g a -> g b
```

Well doesn't *that* look familiar! So now we know that we will need to
work with the `(<*>)` function that's
specific to `g`.

What we want to do is to access the `g (a -> b)` inside the `f` and
supply the `(<*>)` with its first
argument. When we are thinking about getting inside something we should
start to think of `fmap` and that's
exactly what we will use:

```haskell
Compose f <*> Compose k = _full_solution
  where
    liftedAp :: _liftedAp
    liftedAp = fmap (<*>) f
```

Notice that I have written the type signature as
`_liftedAp` where GHC will be kind enough
to tell us what the type of this expression is:

```haskell
• Couldn't match expected type ‘_liftedAp’
  with actual type ‘f (g a -> g b)’
...
```

Unfortunately, we can't place this signature here without turning on
some extensions so we will leave it as a comment to ensure that we
remember the type:

```haskell
Compose f <*> Compose k = _full_solution
  where
    -- liftedAp :: f (g a -> g b)
    liftedAp = fmap (<*>) f
```

From here we know that we want to end up with a final value of
`f (g b)`. Looking at
`liftedAp`, we know that if we can lift
some `g a` in then we will get back that
`f (g b)`. So let's look at what we have
to work with again:

```haskell
liftedAp :: f (g a -> g b)
k        :: f (g a)
```

At this stage, I think we're pros at type tetris and realise that we're
working with something that we have seen before. Indeed, this is
`(<*>)` specialised:

```haskell
-- the original definition with 'k', 'x' and 'y'
-- as the type variable names to avoid confusion
(<*>) :: k (x -> y) -> k x -> k y
```

```haskell
-- the 'k' is our f
-- the 'x' is our g a
-- the 'y' is our g b
(<*>) :: f (g a -> g b) -> f (g a) -> f (g b)
```

Noting that we also have to wrap it up in a `Compose` in the end, we get the solution:

```haskell
Compose f <*> Compose k = Compose $ liftedAp <*> k
  where
    liftedAp :: f (g a -> g b)
    liftedAp = fmap (<*>) f
```

An astute reader will notice that this can be written differently
knowing that `fmap` can be written using
its infix operator `<$>`:

```haskell
Compose $ (<*>) <$> f <*> k
```

Which gives us the intuition that we're lifting the
`(<*>)` over our `f`s and applying it to our `g`s.

Let's look at another concrete example using `[]` and `Maybe`:

```haskell
λ> unCompose $ Compose [Just (+1), Just (+2), Nothing] <*> Compose [Just 1, Just 2, Nothing]
```

```haskell
[ Just 2
, Just 3
, Nothing
, Just 3
, Just 4
, Nothing
, Nothing
, Nothing
, Nothing
]
```

#### Conclusion

I hope this provided some insight on how we can compose higher-kinds. In
turn we were able to talk about composing typeclasses, specifically
`Functor` and `Applicative`. The beauty is that now we can use `fmap`, `pure`, and
`(<*>)` on *any two*
`Functor`s or `Applicative`s!

If you want to take this learning further, I would encourage you to
implement `Foldable` and
`Traversable` for `Compose`. Once you have done this, investigate why you cannot
compose two `Monad`s and then you will be
ready to have fun with Monad Transformers! (Robots in Disguise)

**Shoutouts:** to
[Sandy](http://reasonablypolymorphic.com/) (who's got excellent content on Haskell too btw), and
Joe (\@jkachmar on FPChat) for helping me proof-read and polish this upe
