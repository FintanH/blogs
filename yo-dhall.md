# Yo, Yoneda!

## A Tale of Normalisation

In the [last post](https://functional.works-hub.com/learn/bowl-full-of-lentils-fcbf3) we left off having gone through the basics of
[Dhall](https://github.com/dhall-lang/dhall-lang) and defining an `Either` type.
For this post the aim of the game is to look into defining `Functor`s in Dhall and then seeing how `Yoneda`
relates to `Functor`s and why it helps us in Dhall. Without further ado, let's
get stuck in with `Functor`!

## Functor

To begin, let's deconstruct what we know about `Functor` (in most programming languages)
and build up the definition from there. The first thing we know about `Functor`
is that it abstacts over some `f` that has a kind `Type → Type`.

The other thing we know about `Functor` is that it defines a higher-order function called `map`.
This higher order function takes an `(a → b)` and gives us back a function `f a → f b`.

To define this in Dhall we define it is a record type with a field called `map`.
So let's see what that looks like:

```
  λ(f : Type → Type)
→ { map : ∀(a : Type) → ∀(b : Type) → (a → b) → f a → f b }
```

Breaking this down we can see:

* An `f` that is of type `Type → Type`, corresponding to our required kind.
* A record containing a field `map` which defines the higher-order function.
* The higher-order function for all `a`, and for all `b`, and the required function `(a → b) → f a → f b`.

Placing this in a file `Functor/Type` and running it through Dhall we get:
```
$ dhall <<< "./Functor/Type"
∀(f : Type → Type) → Type

λ(f : Type → Type) → { map : ∀(a : Type) → ∀(b : Type) → (a → b) → f a → f b }
```

With that I invite you to try implement `Functor` for `Either` under
a file called `Either/functor`. If you get stuck the solution is below.
Some things to note are that you will need to import the `Either` type that
was defined in the last post (or write it in-line), and the `Functor/Type` we
have just defined to add a type annotation so that we make sure we aren't lying
about the implementation.











```
    let Functor = ./../Functor/Type

in  let Either = ./Type

in    λ(a : Type)
    →   { map =
              λ(b : Type)
            → λ(c : Type)
            → λ(f : b → c)
            → λ(either : Either a b)
            →     let E = constructors (Either a c)

              in  merge
                  { Left =
                      λ(x : a) → E.Left x
                  , Right =
                      λ(x : b) → E.Right (f x)
                  }
                  either
        }
      : Functor (Either a)

```

Since `Either` has a kind `Type → Type → Type` we have to introduce what the `a` for the `Left`
part of the union type. Then we introduce the types we will be transforming in our `map`
function (`b` and `c` in this case) and the `either` that we will be mapping over. We will need to construct new
values of type `Either a c`, handling each case of the union. Finally we collapse the `either` we were given and reconstruct
a new one with function `f` applied to the `Right` side.

## Traffic Problem with `merge`

This is where we're going to see that we run into a bit of code bloat with Dhall.
Let's look at what happens when we `map` multiple functions one after the other over some `Either` value.

```
    let Either = ./Either/Type Text Natural

in  let map = (./Either/Functor Text).map

in    λ(e : Either)
    → map
      Natural
      Natural
      (λ(i : Natural) → i + 5)
      ( map
        Natural
        Natural
        (λ(i : Natural) → i + 4)
        ( map
          Natural
          Natural
          (λ(i : Natural) → i + 3)
          ( map
            Natural
            Natural
            (λ(i : Natural) → i + 2)
            (map Natural Natural (λ(i : Natural) → i + 1) e)
          )
        )
      )

```

Not the prettiest code ever but it will be demonstrative of the point so let's
run this through Dhall:

```
$ dhall <<< "./lots-o-map"


∀(e : < Left : Text | Right : Natural >) → < Right : Natural | Left : Text >

  λ(e : < Left : Text | Right : Natural >)
→ merge
  { Left =
      λ(x : Text) → < Left = x | Right : Natural >
  , Right =
      λ(y : Natural) → < Right = y + 5 | Left : Text >
  }
  ( merge
    { Left =
        λ(x : Text) → < Left = x | Right : Natural >
    , Right =
        λ(y : Natural) → < Right = y + 4 | Left : Text >
    }
    ( merge
      { Left =
          λ(x : Text) → < Left = x | Right : Natural >
      , Right =
          λ(y : Natural) → < Right = y + 3 | Left : Text >
      }
      ( merge
        { Left =
            λ(x : Text) → < Left = x | Right : Natural >
        , Right =
            λ(y : Natural) → < Right = y + 2 | Left : Text >
        }
        ( merge
          { Left =
              λ(x : Text) → < Left = x | Right : Natural >
          , Right =
              λ(y : Natural) → < Right = y + 1 | Left : Text >
          }
          e
        )
      )
    )
  )
```

That's a lot of nested merges! And if we output it into a file by doing
`dhall <<< "./lot-o-map" > output` we can inspect the size of it and we can see it's 941B.
In more complicated use cases where you are using `map` repeatedly your expressions can
become GBs; Woof! 🐶

Sure this seems like a trivial case but it can occur (and did in our [Formation](https://formation.ai/) code base) in more complex code. While
using Dhall at work we had a `traverse` that contained multiple uses of `map` inside the
body, that traversed a large AST of nested unions. This meant there was a lot of `map`s accumulating.
We had an output of 300MB, which was slowing down the Haskell code that was trying to read this in. So what can we do about it?!

## Yo, Yoneda!

Enter `Yoneda`! I first heard about this through my good friend
[reasonablypolymorphic](http://reasonablypolymorphic.com/). Sandy was talking about `Yoneda` and how it can help
Haskell generics code for more efficient implementations. Its use doesn't stop there though,
but first let's take a look at what it is.

We can define `Yoneda` in Dhall like so:
```
λ(f : Type → Type) → λ(a : Type) → ∀(b : Type) → (a → b) → f b
```

We will make this easier to digest by looking at each part individually. The first thing
we have is an `f` that is of kind `Type → Type`. We then have an `a` of kind `Type`.

When these are applied we get back a higher-order function
`(a → b) → f b` for all `b`. This description should start to sound _very_ familiar.

`Yoneda` is known as the "Free Functor" because we can define a `Functor` `map` operation
on it for _anything_ that is of kind `Type → Type`!

So at this point we should look at how the `Functor` implementation is defined for `Yoneda`:
```
    -- unfortunately we have to define this here...
    let compose =
            λ(a : Type)
          → λ(b : Type)
          → λ(c : Type)
          → λ(f : b → c)
          → λ(g : a → b)
          → λ(x : a)
          → f (g x)

in let  Yoneda = ./Type

in    λ(f : Type → Type)
    →   { map =
              λ(a : Type)
            → λ(b : Type)
            → λ(g : a → b)
            → λ(yoneda : Yoneda f a)
            → λ(c : Type)
            → λ(k : b → c)
            → yoneda c (compose a b c k g)
        }
      : ./../Functor/Type (Yoneda f)
```

At the top we define `compose` to make the definition a bit easier to read, and unfortunately
there isn't a builtin way to compose two functions in Dhall.

Moving on, since `Yoneda` has kind `(Type → Type) → Type → Type` we need to introduce our `f : Type → Type`.
We then see our usual set up of `map` but things get interesting at `λ(c : Type)`.

Remember that `∀(b : Type)`? Well the `λ(c : Type)` is fulfilling this part of `Yoneda` for us.
Next, `λ(k : b → c)` is the `(a → b)` part of the `Yoneda` definition. For the final line
we'll inspect each piece individually because it can be a bit mind-melty 🙀.

1. Reasoning about the type of `yoneda : Yoneda f a` we can find that it's
   `∀(b : Type) → (a → b) → f b` since we have just applied the first two requirements, `f` and `a`.

2. `yoneda c` applies the `c` type to our `∀(b : Type)` so its type is `(a → c) → f c`

3. `compose a b c k g` seems a bit noisy, but the first three parameters are the types `a`, `b`, and `c`.
It then composes our two functions `k : b → c` and `g : a → b`, giving us a function of type `a → c`.

4. Applying the result from 3. to the result of 2. we get an `f c`.

So what `Yoneda` is doing is composing the two functions and associating them to
the right. This reminds us of one of the `Functor` laws:

```
map g . map f === map (g . f)
```

On top of this function composition is associative:

```
    map h . (map g . map f)
=== (map h . map g) . map f
=== map h . map g . map f
=== map (h . g . f)
```

But of course we aren't always working in terms of `Yoneda`. We need different semantics for
different scenarios. Such as error handling with `Either`. So for this we have two functions
in the `Yoneda` tool box to help us: `lift` and `lower`.

`lift` will _lift_ your `f` into `Yoneda` and we define it in Dhall as:

```
  λ(f : Type → Type)
→ λ(functor : ./../Functor/Type f)
→ λ(a : Type)
→ λ(fa : f a)
→ λ(b : Type)
→ λ(k : a → b)
→ functor.map a b k fa
```

To summarise, we need:
1. Our `f` that we're lifting
2. Its `Functor` implementation
3. The `a` `Type` that we're working on in the `f`
4. The `f a` value
5. And the body of the `Yoneda` from `λ(b : Type)` onwards, i.e. `Yoneda f a`.

Conversely, `lower` _lowers_ the `Yoneda` to our `f`. Defined in Dhall as:
```
  λ(f : Type → Type)
→ λ(a : Type)
→ λ(yoneda : ./Type f a)
→ yoneda a (λ(x : a) → x)
```

It uses the identity function, `λ(x : a) → x)`, and to understand why we can once again
turn to one of the `Functor` laws that states:

```
map id === id
```

So identity acts as the "default" argument that acts as a no-op and gives us back our `f`
structure.

## Slim Fast

We've jumped through all these hoops: defined `Yoneda`, `lift`, and `lower`, and now what? Well let's see what
happens when we change the earlier example to use `Yoneda`.

We first `lift` our `Either` data into `Yoneda`, apply the series of `map`s,
and finally `lower` the `Yoneda` back to `Either` so the interface of the function still looks the same.

```
    let EitherText = ./Either/Type Text

in  let Either = EitherText Natural

in  let lift = ./Yoneda/lift EitherText (./Either/Functor Text)

in  let lower = ./Yoneda/lower EitherText

in  let map = (./Yoneda/Functor EitherText).map

in    λ(e : Either)
    → lower
      Natural
      ( map
        Natural
        Natural
        (λ(i : Natural) → i + 5)
        ( map
          Natural
          Natural
          (λ(i : Natural) → i + 4)
          ( map
            Natural
            Natural
            (λ(i : Natural) → i + 3)
            ( map
              Natural
              Natural
              (λ(i : Natural) → i + 2)
              (map Natural Natural (λ(i : Natural) → i + 1) (lift Natural e))
            )
          )
        )
      )
```

And let's run it!

```
$ dhall <<< "./less-o-map"
∀(e : < Left : Text | Right : Natural >) → < Right : Natural | Left : Text >

  λ(e : < Left : Text | Right : Natural >)
→ merge
  { Left =
      λ(x : Text) → < Left = x | Right : Natural >
  , Right =
      λ(y : Natural) → < Right = ((((y + 1) + 2) + 3) + 4) + 5 | Left : Text >
  }
  e
```

🙌 look at that reduction! Getting some hard numbers by outputting to a file again by doing
`dhall <<< "./less-o-map" > output`, we can see that's it 221B! That's roughly 4 times smaller!
The best part about this is that reduction stays constant no matter how many maps we introduce because
we will always only need one merge! 🎉 Remember that 330MB I mentioned before? It was reduced to 35MB
which is roughly 10 times smaller!

One open question that I have not resolved is do we definitely _need_ a `Functor` implementation to `lift`?
I said that we can use `Yoneda` with _any_ `Type → Type`, but we still need it to be a
`Functor` in the end. This could be useful for things like `Set` that do not fit into the `Functor` definition
due to it reliance on `Ord`. But at least we get some sweet normalisation fusion, and that makes me a happy programmer!
