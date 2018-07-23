Free Me --- Exploring the Free Data Type {#free-me-exploring-the-free-data-type .p-name}
========================================

::: {.section .p-summary data-field="subtitle"}
So you may have heard of the Free monad upon your journey of exploring
functional programming. You may have even read a blog or two, or...
:::

::: {.section .e-content data-field="body"}
::: {.section .section .section--body .section--first .section--last name="c195"}
::: {.section-divider}

------------------------------------------------------------------------
:::

::: {.section-content}
::: {.section-inner .sectionLayout--insetColumn}
### Free Me --- Exploring the Free Data Type {#8e0f .graf .graf--h3 .graf--leading .graf--title name="8e0f"}

![Image taken from:
<http://www.brainlesstales.com/2010-10-28/set-me-free>](https://cdn-images-1.medium.com/max/800/1*V8EaLWgy9CmG6FByI1uF5A.jpeg){.graf-image}

So you may have heard of the Free monad upon your journey of exploring
functional programming. You may have even read a blog or two, or even
three about it. You may have even implemented a DSL (Domain Specific
Language) with it. Well hopefully I'm going to provide you with a fresh
take on exploring Free. What I wanted to do for myself was explore Free
from first principles and implement some core functionality from the
ground up to see how things work under the hood. This post is about what
happened when I did this.

### New Beginnings {#d3c4 .graf .graf--h3 .graf-after--p name="d3c4"}

The first part of my journey was to take a trip on over to Edward
Kmett's package `free`{.markup--code .markup--p-code}, more
specifically, the data definition:
<https://www.stackage.org/haddock/lts-9.4/free-4.12.4/Control-Monad-Free.html#t:Free>

``` {#8560 .graf .graf--pre .graf-after--p name="8560"}
data Free f a =
    Pure a
  | Free (f (Free f a))
```

So we have our data definition with the name `Free`{.markup--code
.markup--p-code} taking two type parameters, `f`{.markup--code
.markup--p-code} and `a`{.markup--code .markup--p-code}. With this
definition we have two constructors `Pure`{.markup--code
.markup--p-code} and `Free`{.markup--code .markup--p-code}. There are
many good explanations of this structure and I won't get into it here.
There are better explanations that I will provide below, but for now we
will say that this data type describes and abstract syntax tree (AST)
for general computation. Or in other words, we can write programs with
it.

### Functor {#eb9b .graf .graf--h3 .graf-after--p name="eb9b"}

Alrighty, we have our data type and that's cool. But now we want to be
able to do things with it. More specifically, we will make this data
type an instance of three of our favourite typeclasses: Functor,
Applicative and Monad. We will start from `Functor`{.markup--code
.markup--p-code} and work our way up the chain.

``` {#9c67 .graf .graf--pre .graf-after--p name="9c67"}
instance Functor f => Functor (Free f) where
```

So already we have something interesting here. We cannot straight up
define a `Functor`{.markup--code .markup--p-code} for
`Free`{.markup--code .markup--p-code} without saying that the
`f`{.markup--code .markup--p-code} inside is a `Functor`{.markup--code
.markup--p-code}. The reason being, that we need the power of
`fmap`{.markup--code .markup--p-code} to access the structure.

So let's think about what the type signature of `fmap`{.markup--code
.markup--p-code} looks like for `Free`{.markup--code .markup--p-code}.
Our regular `fmap`{.markup--code .markup--p-code} looks like
`fmap :: (a -> b) -> f a -> f b`{.markup--code .markup--p-code}. If we
substitute our data definition for `Free`{.markup--code .markup--p-code}
in there we get `fmap :: (a -> b) -> Free f a -> Free f b`{.markup--code
.markup--p-code}. Cool, so let's try implement it!

``` {#3b45 .graf .graf--pre .graf-after--p name="3b45"}
instance Functor f => Functor (Free f) where
  fmap f (Free freer) = _iWantToBreakFree
```

I'm going to use a little type hole driven development here and reason
about the types. So the types in scope are:

``` {#b3a0 .graf .graf--pre .graf-after--p name="b3a0"}
freer :: f (Free f)
_iWantToBreakFree :: Free f b
```

So we need to do something with `freer`{.markup--code .markup--p-code}
and we know `f`{.markup--code .markup--p-code} is a functor, thus we
will reach for `fmap`{.markup--code .markup--p-code}.

``` {#88d7 .graf .graf--pre .graf-after--p name="88d7"}
instance Functor f => Functor (Free f) where
  fmap f (Free freer) = fmap _iWantToBreakFree freer
```

This will get us some mileage. My thinking is that we have some sort of
`Free f a`{.markup--code .markup--p-code} inside that first
`f`{.markup--code .markup--p-code} and we will probably want to change
it into a `Free f b`{.markup--code .markup--p-code}. And you know what?
That's exactly what we will do, and we have the perfect function
`(a -> b) -> Free f a -> Free f b`{.markup--code .markup--p-code}, also
known as `fmap`{.markup--code .markup--p-code}!

``` {#7336 .graf .graf--pre .graf-after--p name="7336"}
instance Functor f => Functor (Free f) where
  fmap f (Free freer) = fmap (fmap f) freer
```

Now the only thing we are missing here is that we unwrapped a
`Free`{.markup--code .markup--p-code} so we need to wrap it up again,
and our final solution for this is:

``` {#a621 .graf .graf--pre .graf-after--p name="a621"}
instance Functor f => Functor (Free f) where
  fmap f (Free freer) = Free $ fmap (fmap f) freer
```

"What about `Pure`{.markup--code .markup--p-code}?!", I hear you cry
out. Well you are not wrong and let's bang it out.

``` {#11dc .graf .graf--pre .graf-after--p name="11dc"}
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free freer) = Free $ fmap (fmap f) freer
```

We just need to unwrap, apply and wrap up. Ezpz. So this is cool, we
have a `Functor`{.markup--code .markup--p-code} instance for
`Free`{.markup--code .markup--p-code} so we can lift functions into our
`Free`{.markup--code .markup--p-code} structure; very nice!

### Applicative {#adec .graf .graf--h3 .graf-after--p name="adec"}

Now that we've defined `Functor`{.markup--code .markup--p-code} for the
`Free`{.markup--code .markup--p-code} data type, we are free to define
`Applicative`{.markup--code .markup--p-code} (I promise that's the first
and last time I will make that pun).

``` {#333b .graf .graf--pre .graf-after--p name="333b"}
instance Functor f => Applicative (Free f) where
```

Again, we start by saying that we need `f`{.markup--code
.markup--p-code} to be a `Functor`{.markup--code .markup--p-code} so we
have enough power in our hands to get things working. First off let's
get a quick refresher on `Applicative`{.markup--code .markup--p-code}.
To define an `Applicative`{.markup--code .markup--p-code} we need to
define two functions `pure`{.markup--code .markup--p-code} and
`(<*>)`{.markup--code .markup--p-code}. Let's make things super clear
and write out the types for these functions when we talk about them with
`Free`{.markup--code .markup--p-code}.

``` {#3504 .graf .graf--pre .graf-after--p name="3504"}
instance Functor f => Applicative (Free f) where
 pure :: a -> Free f a
 (<*>) :: Free f (a -> b) -> Free f a -> Free f b
```

Let's get some of the easier things out of the way.

``` {#10cf .graf .graf--pre .graf-after--p name="10cf"}
instance Functor f => Applicative (Free f) where
 — | I think the name was a hint
 pure = Pure
```

``` {#7192 .graf .graf--pre .graf-after--pre name="7192"}
 — | Unwrap the function and value, apply and rewrap!
 (Pure f) <*> (Pure a) = Pure $ f a
```

``` {#853d .graf .graf--pre .graf-after--pre name="853d"}
 — | Here we have a function wrapped in `Pure` and a `Free`
 — | recursive structure.
 (Pure f) <*> (Free freer) = Free $ fmap (fmap f) freer
```

I think these last two are super cool, because they are actually just
`fmap`{.markup--code .markup--p-code} once we unwrap the function from
`Pure`{.markup--code .markup--p-code}! So we can write this more
succinctly as:

``` {#c4e5 .graf .graf--pre .graf-after--p name="c4e5"}
(Pure f) <*> freer = fmap f freer
```

We have two cases left, both of which have something like
`Free freeFunc`{.markup--code .markup--p-code} on the left hand side of
our `<*>`{.markup--code .markup--p-code} operator. Now I am not gonna
lie, this one really tripped me up because I was fighting with myself
and the compiler implementing them. Maybe you see the solution already,
and if so nice one! But what I am going to do here is take you through
my struggle. My initial template looked like the following:

``` {#1ea2 .graf .graf--pre .graf-after--p name="1ea2"}
(Free freeFunc) <*> (Free freer) =
 _innerFunc <*> _innerFreer
```

There was a couple of things I knew. First I had to unwrap some
structure and was going to end up applying `<*>`{.markup--code
.markup--p-code} recursively. Let's look at the types we have to work
with now.

``` {#22b3 .graf .graf--pre .graf-after--p name="22b3"}
freeFunc :: f (Free f (a -> b))
freer :: f (Free f a)
```

So we are posed with the challenge of accessing the things inside our
`f`{.markup--code .markup--p-code}. We should be immediately thinking
`fmap`{.markup--code .markup--p-code} here! So let's try add to our
solution:

``` {#8f34 .graf .graf--pre .graf-after--p name="8f34"}
(Free freeFunc) <*> (Free freer) =
 Free $ fmap (\innerFunc -> innerFunc <*> _innerFreer) freeFunc
```

Our next step was to `fmap`{.markup--code .markup--p-code} over
`freeFunc`{.markup--code .markup--p-code} to get to our inner
`Free f (a -> b)`{.markup--code .markup--p-code} and then we want to
`<*>`{.markup--code .markup--p-code} to some `_innerFreer`{.markup--code
.markup--p-code}, not knowing what that is, yet. Finally, wrapping it
back up in `Free`{.markup--code .markup--p-code}. So that leaves us with
`_innerFreer`{.markup--code .markup--p-code} being the type of
`Free f a`{.markup--code .markup--p-code} and we still have to use
`freer`{.markup--code .markup--p-code}. So let's try add it in.

``` {#edbb .graf .graf--pre .graf-after--p name="edbb"}
(Free freeFunc) <*> (Free freer) =
 Free $ fmap (\innerFunc -> fmap (\inner -> innerFunc <*> inner) freer) freeFunc
```

Here we will get some noise (I say noise, but it's actually really
helpful) from the compiler about the following errors:

``` {#7a81 .graf .graf--pre .graf-after--p name="7a81"}
Expected type: Free f b
Actual type: Free f (Free f b)
```

``` {#090c .graf .graf--pre .graf-after--pre name="090c"}
…
```

``` {#d6f9 .graf .graf--pre .graf-after--pre name="d6f9"}
Expected type: f (Free f (Free f b))
Actual type: f (f (Free f b))
```

Well, if we remember that we unwrapped our `freer`{.markup--code
.markup--p-code}. When we unwrap we need to rewrap! So we finish it all
of by doing:

``` {#fb96 .graf .graf--pre .graf-after--p name="fb96"}
(Free freeFunc) <*> (Free freer) =
 Free $ fmap (\innerFunc -> Free $ fmap (\inner -> innerFunc <*> inner) freer) freeFunc
```

So we have one last case to implement with `Free`{.markup--code
.markup--p-code} on the left hand side and `Pure`{.markup--code
.markup--p-code} on the right.

``` {#67da .graf .graf--pre .graf-after--p name="67da"}
(Free freeFunc) <*> (Pure a) =
 _iWantToBreakFree
```

As usual we will start off by looking at what types we have to work
with:

``` {#095a .graf .graf--pre .graf-after--p name="095a"}
freeFunc :: f (Free f (a -> b))
a :: a
```

Somehow we are going to have to get into the `f`{.markup--code
.markup--p-code} of our `freeFunc`{.markup--code .markup--p-code}, and I
am sure you are seeing the pattern now, we have to use
`fmap`{.markup--code .markup--p-code}.

``` {#73d7 .graf .graf--pre .graf-after--p name="73d7"}
(Free freeFunc) <*> (Pure a) =
 Free $ fmap (\innerFunc -> innerFunc <*> _iWantToBreakFree) freeFunc
```

So the last thing we need is to fill in the
`_iWantToBreakFree`{.markup--code .markup--p-code} hole. If we think
about what type this is we will come to the conclusion it needs to be a
`Free a`{.markup--code .markup--p-code}. There's only one
`Free a`{.markup--code .markup--p-code} we can construct and that is
`Pure a`{.markup--code .markup--p-code}!

``` {#b0a6 .graf .graf--pre .graf-after--p name="b0a6"}
(Free freeFunc) <*> (Pure a) =
 Free $ fmap (\innerFunc -> innerFunc <*> Pure a) freeFunc
```

And that is it! We have completed the `Applicative`{.markup--code
.markup--p-code} instance for `Free`{.markup--code .markup--p-code}. I
know what you are thinking now, "But Fintan, you said you had trouble
with this one?" and you would be right to ask. So two things, I failed
to tease out the full implementation with the `Free`{.markup--code
.markup--p-code} constructor on both sides, until writing this. So I
ended up cheating and looked at the source to get the answer. BUT! I did
learn that you can abstract the last two cases. So I will show you the
easier way of doing them and explain what is happening.

``` {#41a5 .graf .graf--pre .graf-after--p name="41a5"}
(Free freeFunc) <*> freer =
 Free $ fmap (<*> freer) freeFunc
```

Damn that is nice! So what's up here? As usual we need to unwrap but
this time we are only doing the left hand side. "Why are we only
unwrapping the left hand side?", I hear you ask. Very good question, you
are an astute reader (more astute than me, anyway). Well let's look at
the types!

``` {#909c .graf .graf--pre .graf-after--p name="909c"}
freeFunc :: f (Free f (a -> b))
freer :: Free f a
```

So, we know we want to apply `<*>`{.markup--code .markup--p-code}
recursively. When we `fmap`{.markup--code .markup--p-code} into the
inner part of `freeFunc`{.markup--code .markup--p-code} and want to use
`<*>`{.markup--code .markup--p-code} all we are looking for is a
`Free a`{.markup--code .markup--p-code}. Look what we have here, it's a
`Free a`{.markup--code .markup--p-code} going by the name
`freer`{.markup--code .markup--p-code}. Making the whole thing easy for
us.

### Monad {#1147 .graf .graf--h3 .graf-after--p name="1147"}

We are onto the final of our three. We are going to put the
`Monad`{.markup--code .markup--p-code} in `Free Monad`{.markup--code
.markup--p-code}! We will gloss over `return`{.markup--code
.markup--p-code} since it is the same as `pure`{.markup--code
.markup--p-code}. What we will look at more closely is
`(>>=)`{.markup--code .markup--p-code} . So as we usually do, let's take
a look at the types.

``` {#4129 .graf .graf--pre .graf-after--p name="4129"}
(>>=) :: Free f a -> (a -> Free f b) -> Free f b
```

That's cool. We have our `Free f a`{.markup--code .markup--p-code} then
a function `a -> Free f b`{.markup--code .markup--p-code} and get out a
`Free f b`{.markup--code .markup--p-code}. Let's get started with the
implementation!

``` {#0575 .graf .graf--pre .graf-after--p name="0575"}
instance Functor f => Monad (Free f) where
  (Pure a) >>= k = k a
```

This one is easy money, unwrap our `a`{.markup--code .markup--p-code}
and apply our function `k`{.markup--code .markup--p-code} and, hey
presto, we have a `Free f b`{.markup--code .markup--p-code}. Moving onto
the `Free`{.markup--code .markup--p-code} constructor case.

``` {#e4ca .graf .graf--pre .graf-after--p name="e4ca"}
instance Functor f => Monad (Free f) where
  (Pure a) >>= k = k a
  (Free freer) >>= k = _iWantToBreakFree
```

Hmmmm, what should we do? Oh ya, the same as every other time!
`fmap`{.markup--code .markup--p-code} over, apply recursively and wrap
it all up again.

``` {#c4b6 .graf .graf--pre .graf-after--p name="c4b6"}
instance Functor f => Monad (Free f) where
  (Pure a) >>= k = k a
  (Free freer) >>= k = Free $ fmap (>>= k) freer
```

### Some Helpers From my Friends {#17d8 .graf .graf--h3 .graf-after--pre name="17d8"}

This is awesome, we have our instances, which from the look of the
`free`{.markup--code .markup--p-code} there can be many more! But
instead, we will define two functions that will help us in our endeavour
of writing a DSL using `Free`{.markup--code .markup--p-code}. These two
functions will be `liftF`{.markup--code .markup--p-code} and
`foldFree`{.markup--code .markup--p-code}. `liftF`{.markup--code
.markup--p-code} allows us to lift any `Functor`{.markup--code
.markup--p-code} into a `Free`{.markup--code .markup--p-code} structure
and `foldFree`{.markup--code .markup--p-code} allows us to fold our
`Free`{.markup--code .markup--p-code} structure into a
`Monad`{.markup--code .markup--p-code}. The first is useful for writing
helper functions to describe actions in the DSL and the second is
helpful for interpreting our DSL into some `Monad`{.markup--code
.markup--p-code}.

Let's look at `liftF`{.markup--code .markup--p-code} first:

``` {#b663 .graf .graf--pre .graf-after--p name="b663"}
liftF :: Functor f => f a -> Free f a
liftF = _iWantToBreakFree
```

My first instinct is to use the `Free`{.markup--code .markup--p-code}
constructor on our `Functor`{.markup--code .markup--p-code} but that
does not give us what we need. To explain, we want a
`Free f a`{.markup--code .markup--p-code} but the constructor
`Free`{.markup--code .markup--p-code} needs a
`f (Free f a)`{.markup--code .markup--p-code}. So to turn the inside of
our Functor into a `Free`{.markup--code .markup--p-code} will utilise
`fmap`{.markup--code .markup--p-code} and `Pure`{.markup--code
.markup--p-code}. Our final definition is:

``` {#e006 .graf .graf--pre .graf-after--p name="e006"}
liftF :: Functor f => f a -> Free f a
liftF = Free . fmap Pure
```

Next up on our list is `foldFree`{.markup--code .markup--p-code}:

``` {#6290 .graf .graf--pre .graf-after--p name="6290"}
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
```

To quickly explain, the first parameter to `foldFree`{.markup--code
.markup--p-code} is a function going from any `f x`{.markup--code
.markup--p-code} to a `Monad`{.markup--code .markup--p-code},
`m x`{.markup--code .markup--p-code}. This is called a "natural
transformation" if you want to explore this concept more. The
`forall x.`{.markup--code .markup--p-code} says that this function must
work for any `x`{.markup--code .markup--p-code} but we don't know what
`x`{.markup--code .markup--p-code} at this time. We then use this
function to fold down our `Free f x`{.markup--code .markup--p-code}
structure to the `Monad`{.markup--code .markup--p-code} ,
`m`{.markup--code .markup--p-code}. Let's get into the thick of it and
implement it.

Let's do the easy case first:

``` {#6a82 .graf .graf--pre .graf-after--p name="6a82"}
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree _ (Pure a) = pure a
```

We extract our `a`{.markup--code .markup--p-code} from
`Pure`{.markup--code .markup--p-code} and use the `Monad`{.markup--code
.markup--p-code}\'s `pure`{.markup--code .markup--p-code} to place in
that context instead. Next up:

``` {#8b83 .graf .graf--pre .graf-after--p name="8b83"}
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree k (Free freer) = _iWantToFoldFree
```

Let's see what we have here:

``` {#f70a .graf .graf--pre .graf-after--p name="f70a"}
k :: f x -> m x
freer :: f (Free f a)
```

We don't have our trusty `fmap`{.markup--code .markup--p-code} here
because we never said `f`{.markup--code .markup--p-code} was a functor,
so there's really only one thing we can do, and that's apply
`k`{.markup--code .markup--p-code} to `freer`{.markup--code
.markup--p-code}!

``` {#f016 .graf .graf--pre .graf-after--p name="f016"}
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree k (Free freer) = _iWantToFoldFree (k freer)
```

Let's just think about what this gives us in terms of types:

``` {#4e23 .graf .graf--pre .graf-after--p name="4e23"}
k :: f x -> m x
freer :: f (Free f a)
k freer :: m (Free f a)
```

So when we apply it we actually have a `Monad`{.markup--code
.markup--p-code} with `Free f a`{.markup--code .markup--p-code} inside
it. Well what can we do next to break down that inner
`Free`{.markup--code .markup--p-code} even further? Well since we are
working with a `Monad`{.markup--code .markup--p-code} we can utilise
`(>>=)`{.markup--code .markup--p-code}.

``` {#0a7b .graf .graf--pre .graf-after--p name="0a7b"}
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree k (Free freer) = k freer >>= _iWantToFoldFree
```

Again we should ask, what is the type of
`_iWantToFoldFree`{.markup--code .markup--p-code}? Well using the type
signature of `(>>=)`{.markup--code .markup--p-code} we can figure this
out. If `k freer`{.markup--code .markup--p-code} is
`m (Free f a)`{.markup--code .markup--p-code} and our result should be
`m x`{.markup--code .markup--p-code} due to our `foldFree`{.markup--code
.markup--p-code} type signature, then we would expect:

``` {#07bb .graf .graf--pre .graf-after--p name="07bb"}
(>>=) :: m (Free f a) -> (Free f a -> m x) -> m x
```

Hmmm, that function in the middle of our signature looks pretty
familiar... That's right! It's our `foldFree`{.markup--code
.markup--p-code} with the natural transformation already applied! And of
course we can feel at ease with this solution because it's recursive.

``` {#fdbc .graf .graf--pre .graf-after--p name="fdbc"}
foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree k (Free freer) = k freer >>= foldFree k
```

### Final Words {#7f88 .graf .graf--h3 .graf-after--pre name="7f88"}

Well damn, that turned out to be quite the lengthy article just to
explain a few functions, but this actually gives us enough power to
write as many DSLs as we want. On top of this we now understand how
`Free`{.markup--code .markup--p-code} actually works and not just how to
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
their Hello, World of `Free`{.markup--code .markup--p-code}. I will
probably write about that next time. Maybe.

And as promised, here is some resources on `Free`{.markup--code
.markup--p-code}:

-   [<http://dlaing.org/cofun/posts/free_and_cofree.html>]{#2782}
-   [<http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html>]{#1981}
-   [<https://youtu.be/eKkxmVFcd74?list=WL>]{#909d}

If you have any feed back, questions or further information you can
reach out to me. I'm alway `Free`{.markup--code .markup--p-code} for a
chat (I lied there's the second pun).
:::
:::
:::
:::

By [Fintan Halpenny](https://medium.com/@fintan.halpenny){.p-author
.h-card} on [September 17, 2017](https://medium.com/p/c863499a82f8).

[Canonical
link](https://medium.com/@fintan.halpenny/free-me-exploring-the-free-data-type-c863499a82f8){.p-canonical}

Exported from [Medium](https://medium.com) on July 23, 2018.
