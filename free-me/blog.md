# Free Me --- Exploring the Free Data Type

![Image taken from:
<http://www.brainlesstales.com/2010-10-28/set-me-free>](https://cdn-images-1.medium.com/max/800/1*V8EaLWgy9CmG6FByI1uF5A.jpeg)

So you may have heard of the Free monad upon your journey of exploring
functional programming. You may have even read a blog or two, or even
three about it. You may have even implemented a DSL (Domain Specific
Language) with it. Well hopefully I'm going to provide you with a fresh
take on exploring Free. What I wanted to do for myself was explore Free
from first principles and implement some core functionality from the
ground up to see how things work under the hood. This post is about what
happened when I did this.

### New Beginnings

The first part of my journey was to take a trip on over to Edward
Kmett's package `free`, more
specifically, the data definition:
<https://www.stackage.org/haddock/lts-9.4/free-4.12.4/Control-Monad-Free.html#t:Free>

```haskell
data Free f a =
    Pure a
  | Free (f (Free f a))
```

So we have our data definition with the name `Free` taking two type parameters, `f` and `a`. With this
definition we have two constructors `Pure` and `Free`. There are
many good explanations of this structure and I won't get into it here.
There are better explanations that I will provide below, but for now we
will say that this data type describes and abstract syntax tree (AST)
for general computation. Or in other words, we can write programs with
it.

### Functor

Alrighty, we have our data type and that's cool. But now we want to be
able to do things with it. More specifically, we will make this data
type an instance of three of our favourite typeclasses: Functor,
Applicative and Monad. We will start from `Functor` and work our way up the chain.

```haskell
instance Functor f => Functor (Free f) where
```

So already we have something interesting here. We cannot straight up
define a `Functor` for
`Free` without saying that the
`f` inside is a `Functor`. The reason being, that we need the power of
`fmap` to access the structure.

So let's think about what the type signature of `fmap` looks like for `Free`.
Our regular `fmap` looks like
`fmap :: (a -> b) -> f a -> f b`. If we
substitute our data definition for `Free`
in there we get `fmap :: (a -> b) -> Free f a -> Free f b`. Cool, so let's try implement it!

```haskell
instance Functor f => Functor (Free f) where
  fmap f (Free freer) = _iWantToBreakFree
```

I'm going to use a little type hole driven development here and reason
about the types. So the types in scope are:

```haskell
freer :: f (Free f)
_iWantToBreakFree :: Free f b
```

So we need to do something with `freer`
and we know `f` is a functor, thus we
will reach for `fmap`.

```haskell
instance Functor f => Functor (Free f) where
  fmap f (Free freer) = fmap _iWantToBreakFree freer
```

This will get us some mileage. My thinking is that we have some sort of
`Free f a` inside that first
`f` and we will probably want to change
it into a `Free f b`. And you know what?
That's exactly what we will do, and we have the perfect function
`(a -> b) -> Free f a -> Free f b`, also
known as `fmap`!

```haskell
instance Functor f => Functor (Free f) where
  fmap f (Free freer) = fmap (fmap f) freer
```

Now the only thing we are missing here is that we unwrapped a
`Free` so we need to wrap it up again,
and our final solution for this is:

```haskell
instance Functor f => Functor (Free f) where
  fmap f (Free freer) = Free $ fmap (fmap f) freer
```

"What about `Pure`?!", I hear you cry
out. Well you are not wrong and let's bang it out.

```haskell
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free freer) = Free $ fmap (fmap f) freer
```

We just need to unwrap, apply and wrap up. Ezpz. So this is cool, we
have a `Functor` instance for
`Free` so we can lift functions into our
`Free` structure; very nice!

### Applicative

Now that we've defined `Functor` for the
`Free` data type, we are free to define
`Applicative` (I promise that's the first
and last time I will make that pun).

```haskell
instance Functor f => Applicative (Free f) where
```

Again, we start by saying that we need `f` to be a `Functor` so we
have enough power in our hands to get things working. First off let's
get a quick refresher on `Applicative`.
To define an `Applicative` we need to
define two functions `pure` and
`(<*>)`. Let's make things super clear
and write out the types for these functions when we talk about them with
`Free`.

```haskell
instance Functor f => Applicative (Free f) where
 pure :: a -> Free f a
 (<*>) :: Free f (a -> b) -> Free f a -> Free f b
```

Let's get some of the easier things out of the way.

```haskell
instance Functor f => Applicative (Free f) where
 — | I think the name was a hint
 pure = Pure
```

```haskell
 — | Unwrap the function and value, apply and rewrap!
 (Pure f) <*> (Pure a) = Pure $ f a
```

```haskell
 — | Here we have a function wrapped in `Pure` and a `Free`
 — | recursive structure.
 (Pure f) <*> (Free freer) = Free $ fmap (fmap f) freer
```

I think these last two are super cool, because they are actually just
`fmap` once we unwrap the function from
`Pure`! So we can write this more
succinctly as:

```haskell
(Pure f) <*> freer = fmap f freer
```

We have two cases left, both of which have something like
`Free freeFunc` on the left hand side of
our `<*>` operator. Now I am not gonna
lie, this one really tripped me up because I was fighting with myself
and the compiler implementing them. Maybe you see the solution already,
and if so nice one! But what I am going to do here is take you through
my struggle. My initial template looked like the following:

```haskell
(Free freeFunc) <*> (Free freer) =
 _innerFunc <*> _innerFreer
```

There was a couple of things I knew. First I had to unwrap some
structure and was going to end up applying `<*>` recursively. Let's look at the types we have to work
with now.

```haskell
freeFunc :: f (Free f (a -> b))
freer :: f (Free f a)
```

So we are posed with the challenge of accessing the things inside our
`f`. We should be immediately thinking
`fmap` here! So let's try add to our
solution:

```haskell
(Free freeFunc) <*> (Free freer) =
 Free $ fmap (\innerFunc -> innerFunc <*> _innerFreer) freeFunc
```

Our next step was to `fmap` over
`freeFunc` to get to our inner
`Free f (a -> b)` and then we want to
`<*>` to some `_innerFreer`, not knowing what that is, yet. Finally, wrapping it
back up in `Free`. So that leaves us with
`_innerFreer` being the type of
`Free f a` and we still have to use
`freer`. So let's try add it in.

```haskell
(Free freeFunc) <*> (Free freer) =
 Free $ fmap (\innerFunc -> fmap (\inner -> innerFunc <*> inner) freer) freeFunc
```

Here we will get some noise (I say noise, but it's actually really
helpful) from the compiler about the following errors:

```haskell
Expected type: Free f b
Actual type: Free f (Free f b)
```

```haskell
…
```

```haskell
Expected type: f (Free f (Free f b))
Actual type: f (f (Free f b))
```

Well, if we remember that we unwrapped our `freer`. When we unwrap we need to rewrap! So we finish it all
of by doing:

```haskell
(Free freeFunc) <*> (Free freer) =
 Free $ fmap (\innerFunc -> Free $ fmap (\inner -> innerFunc <*> inner) freer) freeFunc
```

So we have one last case to implement with `Free` on the left hand side and `Pure` on the right.

```haskell
(Free freeFunc) <*> (Pure a) =
 _iWantToBreakFree
```

As usual we will start off by looking at what types we have to work
with:

```haskell
freeFunc :: f (Free f (a -> b))
a :: a
```

Somehow we are going to have to get into the `f` of our `freeFunc`, and I
am sure you are seeing the pattern now, we have to use
`fmap`.

```haskell
(Free freeFunc) <*> (Pure a) =
 Free $ fmap (\innerFunc -> innerFunc <*> _iWantToBreakFree) freeFunc
```

So the last thing we need is to fill in the
`_iWantToBreakFree` hole. If we think
about what type this is we will come to the conclusion it needs to be a
`Free a`. There's only one
`Free a` we can construct and that is
`Pure a`!

```haskell
(Free freeFunc) <*> (Pure a) =
 Free $ fmap (\innerFunc -> innerFunc <*> Pure a) freeFunc
```

And that is it! We have completed the `Applicative` instance for `Free`. I
know what you are thinking now, "But Fintan, you said you had trouble
with this one?" and you would be right to ask. So two things, I failed
to tease out the full implementation with the `Free` constructor on both sides, until writing this. So I
ended up cheating and looked at the source to get the answer. BUT! I did
learn that you can abstract the last two cases. So I will show you the
easier way of doing them and explain what is happening.

```haskell
(Free freeFunc) <*> freer =
 Free $ fmap (<*> freer) freeFunc
```

Damn that is nice! So what's up here? As usual we need to unwrap but
this time we are only doing the left hand side. "Why are we only
unwrapping the left hand side?", I hear you ask. Very good question, you
are an astute reader (more astute than me, anyway). Well let's look at
the types!

```haskell
freeFunc :: f (Free f (a -> b))
freer :: Free f a
```

So, we know we want to apply `<*>`
recursively. When we `fmap` into the
inner part of `freeFunc` and want to use
`<*>` all we are looking for is a
`Free a`. Look what we have here, it's a
`Free a` going by the name
`freer`. Making the whole thing easy for
us.

### Monad

We are onto the final of our three. We are going to put the
`Monad` in `Free Monad`! We will gloss over `return` since it is the same as `pure`. What we will look at more closely is
`(>>=)` . So as we usually do, let's take
a look at the types.

```haskell
(>>=) :: Free f a -> (a -> Free f b) -> Free f b
```

That's cool. We have our `Free f a` then
a function `a -> Free f b` and get out a
`Free f b`. Let's get started with the
implementation!

```haskell
instance Functor f => Monad (Free f) where
  (Pure a) >>= k = k a
```

This one is easy money, unwrap our `a`
and apply our function `k` and, hey
presto, we have a `Free f b`. Moving onto
the `Free` constructor case.

```haskell
instance Functor f => Monad (Free f) where
  (Pure a) >>= k = k a
  (Free freer) >>= k = _iWantToBreakFree
```

Hmmmm, what should we do? Oh ya, the same as every other time!
`fmap` over, apply recursively and wrap
it all up again.

```haskell
instance Functor f => Monad (Free f) where
  (Pure a) >>= k = k a
  (Free freer) >>= k = Free $ fmap (>>= k) freer
```

### Some Helpers From my Friends

This is awesome, we have our instances, which from the look of the
`free` there can be many more! But
instead, we will define two functions that will help us in our endeavour
of writing a DSL using `Free`. These two
functions will be `liftF` and
`foldFree`. `liftF` allows us to lift any `Functor` into a `Free` structure
and `foldFree` allows us to fold our
`Free` structure into a
`Monad`. The first is useful for writing
helper functions to describe actions in the DSL and the second is
helpful for interpreting our DSL into some `Monad`.

Let's look at `liftF` first:

```haskell
liftF :: Functor f => f a -> Free f a
liftF = _iWantToBreakFree
```

My first instinct is to use the `Free`
constructor on our `Functor` but that
does not give us what we need. To explain, we want a
`Free f a` but the constructor
`Free` needs a
`f (Free f a)`. So to turn the inside of
our Functor into a `Free` will utilise
`fmap` and `Pure`. Our final definition is:

```haskell
liftF :: Functor f => f a -> Free f a
liftF = Free . fmap Pure
```

Next up on our list is `foldFree`:

```haskell
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
```

To quickly explain, the first parameter to `foldFree` is a function going from any `f x` to a `Monad`,
`m x`. This is called a "natural
transformation" if you want to explore this concept more. The
`forall x.` says that this function must
work for any `x` but we don't know what
`x` at this time. We then use this
function to fold down our `Free f x`
structure to the `Monad` ,
`m`. Let's get into the thick of it and
implement it.

Let's do the easy case first:

```haskell
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree _ (Pure a) = pure a
```

We extract our `a` from
`Pure` and use the `Monad`\'s `pure` to place in
that context instead. Next up:

```haskell
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree k (Free freer) = _iWantToFoldFree
```

Let's see what we have here:

```haskell
k :: f x -> m x
freer :: f (Free f a)
```

We don't have our trusty `fmap` here
because we never said `f` was a functor,
so there's really only one thing we can do, and that's apply
`k` to `freer`!

```haskell
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree k (Free freer) = _iWantToFoldFree (k freer)
```

Let's just think about what this gives us in terms of types:

```haskell
k :: f x -> m x
freer :: f (Free f a)
k freer :: m (Free f a)
```

So when we apply it we actually have a `Monad` with `Free f a` inside
it. Well what can we do next to break down that inner
`Free` even further? Well since we are
working with a `Monad` we can utilise
`(>>=)`.

```haskell
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree k (Free freer) = k freer >>= _iWantToFoldFree
```

Again we should ask, what is the type of
`_iWantToFoldFree`? Well using the type
signature of `(>>=)` we can figure this
out. If `k freer` is
`m (Free f a)` and our result should be
`m x` due to our `foldFree` type signature, then we would expect:

```haskell
(>>=) :: m (Free f a) -> (Free f a -> m x) -> m x
```

Hmmm, that function in the middle of our signature looks pretty
familiar... That's right! It's our `foldFree` with the natural transformation already applied! And of
course we can feel at ease with this solution because it's recursive.

```haskell
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree k (Free freer) = k freer >>= foldFree k
```

### Final Words

Well damn, that turned out to be quite the lengthy article just to
explain a few functions, but this actually gives us enough power to
write as many DSLs as we want. On top of this we now understand how
`Free` actually works and not just how to
use it! That's something extremely useful in itself.

Something else we can learn from this is that type driven development is
amazingly useful. When we constrain ourselves to certain typeclasses and
polymorphism we can only do so many things. This way our solutions tend
to "fall out" and we can reason about what our program is doing. Then we
can utilise the polymorphism, writing the implementation once and
reusing many times. Very cool!

If you want to see some actual code with rambling comments that inspired
this article you can check <https://github.com/FintanH/free-me>. On top
of that there's a mini IO DSL example that everyone seems to write as
their Hello, World of `Free`. I will
probably write about that next time. Maybe.

And as promised, here is some resources on `Free`:

-   [<http://dlaing.org/cofun/posts/free_and_cofree.html>]
-   [<http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html>]
-   [<https://youtu.be/eKkxmVFcd74?list=WL>]

If you have any feed back, questions or further information you can
reach out to me. I'm alway `Free` for a
chat (I lied there's the second pun).
