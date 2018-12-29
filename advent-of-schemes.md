# Advent of Schemes

If you hang around fellow Haskeller/Scala head, Greg Pfeil (a.k.a sellout), enough you will
hear about recursion schemes, as well as some other abstract concepts but those are not
the topic of the day. No, today we will join the conversation and talk about recursion
schemes as well.

I have dabbled with the concept for a long time. Due to being a colleague of Greg's at
[Formation](https://formation.ai/) I had to familiarise myself somewhat with the concepts.
But what annoyed me was that I didn't always get a full understanding of it. I could be given
a jumping off point to work with, but could never arrive at the initial ideas myself.

On the lookout for the perfect opportunity to add some skill points to my recursion shcemes
knowledge I decided to tackle the [Advent of Code](https://adventofcode.com/) challenges but
force myself to use recursion schemes. Granted I am very slow at solving these problems, and
even slower at writing and publishing them. I hope everyone enjoyed the holidays!

So I started Day 1, armed with the [yaya](https://github.com/sellout/yaya) library, my point
of contact, @sellout, some tunes on Spotify (listen to some Jacques Greene and The Blaze),
and the thirst for knowledge.

Before I discuss Day 1 though, I would like to explore my thoughts on recursion schemes
that fell out from this attempt and speaking with Greg. Recursion schemes seem to fall into
the space of algorithm implementation or business logic. So when we look to use them we have
some problem we are trying to solve. I think this is why I found previously read literature
on the topic frustrating. One would usually find how to recover natural numbers or lists
from pattern functors. While these are useful concepts, I saw a massive valley between these
ideas and actually moving towards using recursion schemes. This is why I felt the advent of code
was the perfect medium to marry exploring recursion schemes and trying to solve problems.

So as I've said, recursion schemes tackle problem solving. They are an abstraction of recursion
in data structures and computations. With data structures they factor out the recursion we would
tend to reach for and we gain pattern functors. For example we will heavily use the `List` pattern
functor `XNor`. For computations we abstract folding and unfolding by utilising the fact that these
computations can be done for any `Functor`.

Already we are off to a good start because we have gained two concepts for free. Many pattern functors
already exist and it easy to define one for your domain, and this in turn gives us a cornucopia of
folds and unfolds for free. What does that leave us? The stuff we care about, our business logic or
algorithm implementation.

We take our pattern functor we believe matches the structure we would want to work with, and attempt
to write the function definition as simple as possible. I have personally found myself starting with
the smallest assumption I can make about what I need and adding new power or functionality as I see
fit. We then massage these implementations into algebras or coalgebras that can used in our folds/unfolds.
As I explore recursion schemes further I hope to keep these ideas in mind and refine them to gain a full
understanding of the concepts.

Without further ado, lets see how we can solve Day 1 of AOC with recursion schemes!

## Day 1 Problem

To understand what we need to do, let's discuss the problem first. We are given some input that
consists of a series of numbers separated by newline characters. Each number is prefixed by a `+`
or `-` to show whether is positive or negative.

For example:
```
+1
-2
+3
```

The first part of Day 1 is to write a function that can read this input and sum them. Thus
the answer to the above example would be `2`.

The second part of Day 1 is to write a function that while summing the input can detect if
the current sum has been detected before. If the end of the series of numbers has been reached
we should loop back over them until we reach a repeated frequency. In the example input our answer
would be `1`. To demonstrate:
```
0 + 1 = 1
1 - 2 = -1
-1 + 3 = 2
2 + 1 = 3
3 - 2 = 1 -- we saw 1 when we did 0 + 1
```

So our stream of numbers was: `1, -2, 3, 1, -2`.

## Some Initial Imports

Not much to see here other than some general set up.

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Frequencies where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.State

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parser.Char (char, digit, newline)

import Yaya
import Yaya.Control
import Yaya.Data
import Yaya.Either
import Yaya.Unsafe.Data () -- imported for orphan instances
import Yaya.Zoo
```

## Parsing

I've left the parsing of the input upto [`attoparsec`](http://hackage.haskell.org/package/attoparsec)
since it's lightweight. This does the heavy lifting of grabbing our numbers
and turning them into positive or negative numbers.

```haskell
-- | Parse a `+` token.
plus :: Parser Char
plus = char '+'

-- | Parse a `-` token.
minus :: Parser Char
minus = char '-'

-- | Parse a sign (+/-) followed by its digits.
number :: Parser Int
number = do
  sign <- try (minus >> pure negate) <|> (plus >> pure id)
  sign . read <$> many1 digit

-- | Parse a newline separated string of 'number'.
numbers :: Parser [Int]
numbers = number `sepBy` newline
```

## Algebraic!

Here is our first function that defines an algebra for summing
numbers.

```haskell
sum' :: Algebra (XNor Int) Int
sum' = \case
  Both x y -> x + y
  Neither  -> 0
```

This `Algebra` serves as the summing of our incoming frequencies.
XNor acts as our "pattern functor" which is the pattern functor
for lists, i.e. `[a]`. We can see this by looking at the definition
of `XNor` from `yaya`.

```haskell
  data XNor a b
    = Neither
    | Both a b
```

The case of `Neither` acts as the base case `[]` (or `Nil`).
While `Both` acts as `a : [a]` (or `Cons`). It contains
the head of the list and the tail of the list.

`Algebra f a` is an alias for `f a -> a`, so we can
translate the original type signature to `XNor Int Int -> Int`.

The `x` can be seen as the head of the list, our current element,
and the `y` being the accumulated sum, from the tail upwards.

`Neither` then acts as the base case of our recrsion
and thus we can return 0.

This `Algebra` can then be passed to `cata` which will
process things from the back of the list, read leaf of computation,
up to towards the head.

We can already see the usefulness of expressing our functions via
pattern functors and simple functions. We are simply expressing the
semantics of summation when we write `x + y`, and `0` acting as our
base element. We did not have to talk about recursion and conflate our
logic.

This captures everything we need for part 1, but we will demonstrate
the computation later. Before that we will discuss our other functions
for solving part 2.

## Again, and Again, and Again, and ...

To build up our solution for part 2 we need to solve the issue of repeating elements.
When we consider repeating elements we must think about how we need _at least_ one
element to repeat. That means we will be dealing with a `NonEmptyList`.

```haskell
repeatConcat :: Coalgebra ((,) a) (NonEmptyList a, NonEmptyList a)
repeatConcat (orig, current) =
  case project current of
    Only a     -> (a, (orig, orig))
    Indeed a t -> (a, (orig, t))
```

This `Coalgebra` serves as taking the `NonEmptyList` and repeating it, followed
by concatenating them together.

This can be visualised as:
 * Given a list: 1, 2, 3, 4
 * Results in: 1, 2, 3, 4, 1, 2, 3, 4, ...

`NonEmptyList a` is a type alias for `Mu (AndMaybe a)`. There are two things
we need to address here, so let's talk about `AndMaybe` first.

`AndMaybe` is our pattern functor and acts as, you guessed it, the pattern functor
for non-empty lists. We can understand this by looking at its definition:

```haskell
data AndMaybe a b
  = Only a
  | Indeed a b
```

We can read this as:
```haskell
a `AndMaybe` b
```

We are always guaranteed an `a` and possibly a `b`. There is a guaranteed item in `Only`,
and then `Indeed` looks similar to `Both` from `XNor`, so the head, and the rest of the list.

A usual construction of NonEmpty is that we have a guaranteed `head` of the
list and the rest of the list. In this case `Only` actually acts as the guaranteed `last`
of the list, and `Indeed` as the front (`init`) of the list.

We also introduced `Mu` when inspecting `NonEmptyList`. Let's gain some intuition for what it is
by saying that the type alias `List a` can be defined as `Mu (XNor a)`.
`Mu` is the fixed-point operator for finite/inductive data structures. This means
we can build up a `List`, embedding our pattern functor, `XNor` in `Mu` and this
expresses that we have a finite computation. We can do the same for `NonEmptyList` by
embedding `AndMaybe a` into `Mu`.

At this point we can unpack what our type signature means:
``haskell
      Coalgebra ((,) a) (NonEmptyList a, NonEmptyList a)
  === (NonEmptyList a, NonEmptyList a) -> (a, (NonEmptyList a, NonEmptyList a))
```

We keep the original list as our first element in the `(,)`, and the
current `NonEmptyList` that we are inspecting. Calling `project` lets
us unwrap one layer of `Mu` and this gives us back an `AndMaybe` value that we case on.

If we have `Only` one item we return the last item of the list and the original
list again, i.e. we've reached the end so now we repeat.

In the case where we have `Indeed` we unpack the head and pass on our tail.

We always keep the original to refer to it once we get to the end of the list.

## JTO (Just the One)

Since our input will be a list of numbers we want to write a function that
will give us a `NonEmptyList` from a `List`.

```haskell
nonEmpty :: Algebra (XNor a) (a -> NonEmptyList a)
nonEmpty = \case
  Neither  -> embed . Only
  Both a f -> embed . flip Indeed (f a)
```

As we saw in `sum'` we were representing a list through its pattern functor
`XNor`, which we also use here.

And in `repeatConcat` we looked through `NonEmptyList` and how it was an
alias for `Mu (AndMaybe a)` our pattern functor for non-empty lists and
the fixed-point operator for finite data structures.

So lets look at what this `Algebra` breaks out into:
```haskell
      Algebra (XNor a) (a -> NonEmptyList a)
  === XNor a (a -> NonEmptyList a) -> (a -> NonEmptyList a)
```

So, given an `XNor` of `a`s as the items, and `a -> NonEmptyList a`
as the tail or continuation of the list, we get back a function from
`a -> NonEmptyList a`.

This comes across as intuitive, since we can turn any list into a
non-empty list as long as provide at lease _one_ item. Our initial `a`
from the function!

In the case of `Neither`, our function will be `Only`. So we take that
`a` passed in and return the singleton, non-empty list.

In the case of `Both`, we have the head of our list, and the `NonEmptyList`
being built up from our `Only` element. So, we apply the continuation and
it now acts as the head of our `Indeed` non-empty list. Again, waiting
for the next continuation, when we finally call this function.

## Gently Down the Stream

We are starting to move out of the territory of writing basic functions
and into the territory of using these functions in folds and unfolds.
The task at hand being able to create a `Stream` of `Int`s from a `List`
of `Int`s.

```haskell
makeStream :: List Int -> Stream Int
makeStream l = case project l of
  Neither  -> ana duplicate 0
  Both h t -> ana repeatConcat (duplicate $ (cata nonEmpty t h))
  where
    duplicate :: Coalgebra ((,) a) a
    duplicate i = (i, i)
```

Our input is a `List Int` which is a type alias for `Mu (XNor a)`.
At this point we know by now that combining the pattern functor for
lists, `XNor` and `Mu` we are saying we have a finite list structure.

Our output is `Stream Int`, which is a type alias for `Nu ((,) a)`.
This particular pattern we have not come across yet. So lets see what
these two things in tandem entail.

`Nu` is the dual fixed-point operator to `Mu`. As its dual it means
that it describes infinite/co-inductive data structures. So when we
combine `Nu` with pattern functors, we are saying that we have potentially
infinite data. This is very true for our case of repeating a list.

`(,) a` is chosen as the pattern functor because it naturally represents
an infinite stream of data. We have the head of the stream, the first element
of the tuple, and the rest of the stream, the second element of the tuple.

We could anaolgise this to a manual recursive definition of a stream:
```haskell
data Stream = { element :: a, more :: Stream a }
```

Another way of looking at it is that streams are lists without a Nil case.

With this explanation out of the way we can break down how to implementation works.
We first `project` our list `l` to unwrap one layer of our `List`.
In the case of `Neither` we make a "safe" move of producing a stream of 0s.
We cannot do much with an empty list, since it is undefined for `Stream` data,
but it works in our larger problem because adding 0 is no-op.

In the case of `Both` we utilise our `nonEmpty` algebra to convert our `List`
into a `NonEmptyList`. We duplicate this result and infinitely generate, via `ana`,
a `Stream` of `NonEmptyList`s using `repeatConcat`.

## Finding the Repeat in a Haystack

This our final coalgebra that implements finding the repeating frequency in
a stream of input.

We can break up the problem into its constituent parts:
  * An input of numbers that we need to sum. This is represented by
    a `Stream Int`.
  * The frequencies we have previously seen while inspecting the
    stream of numbers. This is represented by a `Set Int`.
  * The current sum, or tally, we have while inspecting the stream
    of numbers.

The output is the first repeated element we find. But the thing is, we might
actually never find a repeating element. Consider the input of a single `+1`.
We will forever compute the natural numbers until we overflow, since we are using
`Int`.
Thus, we split our computation into two. We either return a repeated frequency, or
we continue our computation.

  * This split is represented by `Either Int (Int, Set Int, Stream Int)` where
    our `Left` case short-circuits the computation and returns our result, while
    the `Right` case continues our computation.

```haskell
repeatCoalgebra :: Coalgebra (Either Int) (Int, Set Int, Stream Int)
repeatCoalgebra (tally, env, stream) =
  let (currentElem, restOfStream) = project stream
      result = tally + currentElem
  in if result `Set.member` env
     then Left result
     else Right $ (result, Set.insert result env, restOfStream)
```

As we mentioned above, we don't necessarily know how long this algorithm can go on
for so we describe it as a `Coalgebra` and provide it a seed. The seed if made up
of our constituent parts of: a tally, the previously seen frequencies, and the stream
of numbers.

The first thing we want to do is inspect the head of the stream, this
can be done by using `project` to split the stream into its head and its
tail.

We can then get our latest tally by summing our current element, the head
of the stream, with our aggregated tally, the seed.

We then check that our `result` is a member of the `Set` of previously seen tallies.
If it is, then great! We found our result and short-circuit with `Left`.
If not, we continue the computation updating our seed values, and returning
with `Right`.

Lets take a moment to consider the semantics of `Either` in this case, because
it's important to note how its acting and its general use in recursion schemes.

When we think about `Either` we generally think about short-circuiting behaviour.
This is drilled into us when we write the `Functor`, `Applicative`, and `Monad` instances.
When we see the `Left` case we propagate this result.
In our case it's the natural pattern functor for partial functions. This can be seen in the
newtype defined in `yaya`, `Partial`. It is defined as `Partial a = Nu (Either a)`.
The `Partial` relates to the short-circuiting. We _may_ get a result and this will be
passed back as a `Left` value. Or we may just keep computing forever with a `Right` value.

In our case this is very true given our previous problem case of having a single input of `+1`.

## Why do we fall, Bruce?

While trying to figure out how to solve part 2 I wrote the following function.
It has a couple of problems and it's left as an exercise to the reader to see what they are.

```haskell
repeatAlgebra :: XNor Int Int -> State (Set Int) (Either Int Int)
repeatAlgebra = \case
  Both currentElement tally -> do
    let result = currentElement + tally
    env <- get
    if result `Set.member` env
       then pure $ Left result
       else put (Set.insert result env) >> pure (Right result)
  Neither -> pure (Right 0)
```

## Folding it All Together

Our answer to part 1 is implemented in `frequency`. We parse our list of numbers and
use the combination of `cata` and our `sum'` algebra to find the answer. We simply
explode if our parsing fails.

```haskell
frequency :: ByteString -> Int
frequency = either (error . show) id . fmap (cata sum') . parseOnly numbers
```

Our answer to part 2 is implemented in `findRepeat`. This is the helper to run our `repeatCoalgebra`.
It uses `ana` to unfold our seed values `(0, {0}, makeStream numbers)` and uses the `runToEnd` fold to
evaluate our result to get our final result.

```haskell
findRepeat :: ByteString -> Int
findRepeat = either (error . show) id
           . fmap (runToEnd . runner)
           . parseOnly numbers
  where
    runner :: [Int] -> Nu (Either Int)
    runner ns = ana repeatCoalgebra (0, Set.singleton 0, makeStream $ cata embed ns)
```

So there we have it. Our first day of Advent of Code implemented using recursion schemes
and some quick parser functions. I hope this helps bridge the gap between defining natural
numbers using recursion schemes and actually solving problems.

You can follow the rest of my Advent of Code antics on my [github](https://github.com/FintanH/aocrc).
They'll be coming through slow and steady! You can feel free to contact with me with any feedback or questions :)
