# DRAFT

```

## Functor

We're almost at the core of this blog post. The last hurdle for us to get over is to define
`Functor` and implement it for our `Either` type. We know that a `Functor` involves a
`map` operation, two types `a` and `b`, and a higher-kinded type `f`. So let's see if we
can translate this into Dhall in a file under `Functor/type`:
```
  λ(f : Type → Type)
→ { map : ∀(a : Type) → ∀(b : Type) → (a → b) → f a → f b }
```

Breaking this down we get:

* An `f` that is of type `Type → Type`
* A record containing a field `map` which says
* Forall `a`, and forall `b` we get a higher-order function of `(a → b) → f a → f b`

Running this file through Dhall:
```
$ dhall <<< "./Functor/Type"
∀(f : Type → Type) → Type

λ(f : Type → Type) → { map : ∀(a : Type) → ∀(b : Type) → (a → b) → f a → f b }
```

With that I invite you to try implement `Functor` for `Either` under
a file called `Either/functor`. If you get stuck the solution is below.




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

Since `Either` has a kind `Type → Type → Type` we have to introduce what the `a` for `Left`
part of the union type. Then we introduce the types we will be transforming in our `map`
function and the `either` that we will be mapping over. We will need to construct new
values of type `Either a c`, and finally collapse the `either` we were given and reconstruct
a new one with function `f` applied to the `Right` side.

## Traffic Problem with `merge`

We're here. This is the crux of the blog post. Let's look at what happens when map multiple
functions one after the other to some `Either` type.

```
    let Either = ./Either/Type Text Natural

in  let map = (./Either/Functor Text).map

in  let pOne = λ(i : Natural) → i + 1

in  let pTwo = λ(i : Natural) → i + 2

in  let pThree = λ(i : Natural) → i + 3

in  let pFour = λ(i : Natural) → i + 4

in  let pFive = λ(i : Natural) → i + 5

in  let foo =
            λ(e : Either)
          → map
            Natural
            Natural
            pFive
            ( map
              Natural
              Natural
              pFour
              ( map
                Natural
                Natural
                pThree
                (map Natural Natural pTwo (map Natural Natural pOne e))
              )
            )

in  foo
```

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

That's a lot of nested merges! And if output it into a file by doing
`dhall <<< "./lot-o-map" > output` we can inspect the size of it and we can see it's 941B.
Woof! 🐶
Sure this seems like a trivial case but it can occur (and did) in more complex code. While
using Dhall at work we had a `traverse` that contained multiple uses of `map` inside the
body. This meant there was a lot of `map`s accumulating. So what can we do about it?!

## Yo, Yoneda!

Enter `Yoneda`! I first heard about this through my good friend at
[reasonablypolymorphic](link-here). Sandy was talking about `Yoneda` and how it can help
Haskell generics code for more efficient implementations. It's use doesn't stop there though,
but first let's take a look at what it is.

We can define `Yoneda` in Dhall like so:
```
λ(f : Type → Type) → λ(a : Type) → ∀(b : Type) → (a → b) → f b
```

Breaking it down we can see: an `f` that takes a `Type` and gives us back a `Type`, an `a`
type, and with those supplied we get back a type that is forall `b` a function of type
`(a → b) → f b`.

The latter should look very familiar to us. If we think back to the definition of `Functor`
the `map` function ended in `(a → b) → f a → f b`. So `Yoneda` looks like a partially applied
`map` where the `f a` is already supplied. This is no coincidence! `Yoneda` is known as the
"free functor". This roughly translates to: given any `f` that is of kind `Type → Type` we
get a `Functor` for free. The `f` does not necessarily have to be a `Functor` itself.

So at this point we should look at how the `Functor` implementation is defined for `Yoneda`:
```
    let compose =
            λ(a : Type)
          → λ(b : Type)
          → λ(c : Type)
          → λ(f : b → c)
          → λ(g : a → b)
          → λ(x : a)
          → f (g x)

in    λ(f : Type → Type)
    →   { map =
              λ(a : Type)
            → λ(b : Type)
            → λ(g : a → b)
            → λ(yoneda : ./Type f a)
            → λ(c : Type)
            → λ(k : b → c)
            → yoneda c (compose a b c k g)
        }
      : ./../Functor/Type (./Type f)
```

At the top we define `compose` to make the definition a bit easier to read, and unfortunately
there isn't a builtin way to compose two functions in Dhall. Moving on, since `Yoneda` has
kind `(Type → Type) → Type → Type` we need to introduce our `f : Type → Type`. We then see
our usual set up of `map` but things get interesting at `λ(c : Type)`.
Remember that `∀(b : Type)`? Well the `λ(c : Type)` is fulfilling this part of `Yoneda` for us.
Next, `λ(k : b → c)` is the `(a → b)` part of the `Yoneda` definition. For the final line
we'll inspect each piece individually.

1. Reasoning about the type of `yoneda : ./Type f a` we can see that it's
   `∀(b : Type) → (a → b) → f b`
2. `yoneda c` applies the `c` type to our `∀(b : Type)` so its type is `(a → c) → f c`
3. `compose a b c k g` composes our two functions `k : b → c` and `g : a → b`, giving
   us a function of type `a → c`.
4. Applying rhe result 3. to the result of 2. we get an `f c`.

So what `Yoneda` is doing is composing the two functions and making them associating them to
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
different scenarios. Such as error handling with `Either`. So for this we have two things to
help us: `lift` and `lower`

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

We need:
1. Our `f` that we're lifting
2. Its `Functor` impementation
3. The `a` that is we're working on in the `f`
4. The `f a` value
5. And the body of the `Yoneda` from `λ(b : Type)` onwards.

Converseley, `lower` _lowers_ the `Yoneda` to our `f`. Defined in Dhall as:
```
  λ(f : Type → Type)
→ λ(a : Type)
→ λ(yoneda : ./Type f a)
→ yoneda a (λ(x : a) → x)
```

It simply uses the identity function as the function that `Yoneda` is waiting for.

## Slim fast

We've jumped through all these hoops, defined `Yoneda`, and now what? Well let's see what
happens when we change the earlier example to use `Yoneda`. We first `lift` our `Either` data
into `Yoneda`, apply the series of `map`s, and finally `lower` the `Yoneda` back to `Either` so
the interface of the function still looks the same.

```
    let EitherText = ./Either/Type Text

in  let Either = EitherText Natural

in  let YonedaE = ./Yoneda/Type EitherText

in  let lift = ./Yoneda/lift EitherText (./Either/Functor Text)

in  let lower = ./Yoneda/lower EitherText

in  let map = (./Yoneda/Functor EitherText).map

in  let pOne = λ(i : Natural) → i + 1

in  let pTwo = λ(i : Natural) → i + 2

in  let pThree = λ(i : Natural) → i + 3

in  let pFour = λ(i : Natural) → i + 4

in  let pFive = λ(i : Natural) → i + 5

in  let foo =
            λ(e : Either)
          → lower
            Natural
            ( map
              Natural
              Natural
              pFive
              ( map
                Natural
                Natural
                pFour
                ( map
                  Natural
                  Natural
                  pThree
                  ( map
                    Natural
                    Natural
                    pTwo
                    (map Natural Natural pOne (lift Natural e))
                  )
                )
              )
            )

in  foo
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
`dhall <<< "./less-o-map" > output`, we can see that's it 221B! That's roughly 4 times smaller,
and it stays the same no matter how many maps we introduce 🎉
