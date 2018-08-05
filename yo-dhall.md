
# DRAFT

In this blog post I'm going to take a break from Haskell
and spread the good word of [Dhall](link-here). It won't be a complete break from Haskell,
because what I want to discuss here is a technique that I stole
from Haskell who in turn stole it from category theory. I won't (nor can't)
go through the innards of the category theory but that doesn't mean we
can't utilise it! So strap yourself in for a wild ride of, Dhall, some `Functor`ing, a new
friend `Yoneda`, and a warm feeling of knowing that mathematics is here to help.

## Introduction to Dhall

Before we get into the meat of this let's run through how Dhall works.
One can almost view Dhall as JSON with functions and types, but really it's so much better.
There is an excellent [tutorial](https://hackage.haskell.org/package/dhall-1.16.1/docs/Dhall-Tutorial.html)
that lives in the library. It goes through the simple usage of Dhall and how you can utilise
Dhall files in your Haskell code. I will try summarise here the main points to give you a taste.
To _really_ get a feel we can grab the Dhall executable. An easy way to get is if you have
[`stack`](link-to-stack), we can run `stack install dhall --resolver=lts-12.0`;
grabbing the `dhall` package at LTS-12.0.

The native types to Dhall can be enumerated:
* Bool
* Natural
* Integer
* Double
* Text
* List
* Optional
* Unit

So let's take a look at them via `dhall` command. Running the following we can see what we
can do with Dhall.
```bash
$ dhall <<< "True && False"
Bool

False
```

```bash
$ dhall <<< "1"
Natural

1
```

```bash
$ dhall <<< "1 + 1"
Natural

2
```

```bash
$ dhall <<< "-1"
Integer

-1
```

```bash
$ dhall <<< "3.14"
Double

3.14
```
Note:
> There are no built-in operations on `Integer`s or `Double`s. For all practical purposes they are opaque values within the Dhall language

```bash
$ dhall <<< "\"Hello\" ++ \" World\""
Text

"Hello World"
```

```bash
$ dhall <<< "[1, 2, 3] # [4, 5, 6]"
List Natural

[ 1, 2, 3, 4, 5, 6 ]
```

```bash
$ dhall <<< "List/fold Bool [True, False, True] Bool (λ(x : Bool) → λ(y : Bool) → x && y) True"
Bool

False
```

```bash
$ dhall <<< "List/length Natural [1, 2, 3]"
Natural

3
```

```bash
$ dhall <<< "Optional/fold Text ([\"ABC\"] : Optional Text) Text (λ(t : Text) → t) \"\""
Text

"ABC"
```

```bash
$ dhall <<< "Optional/fold Text ([] : Optional Text) Text (λ(t : Text) → t) \"\""
Text

""
```

```bash
$ dhall <<< "{=}"
{}

{=}
```
The Unit type looks like an empty records, which segues us onto our next topic!

On top of all these types we can make records that have named fields.
For example let's define a user
with a name, age, and email.
```bash
$ dhall <<< "{ name : Text, age : Natural, email : Text }"
Type

{ name : Text, age : Natural, email : Text }
```
Notice that we didn't have to bind the record type to some name such as `User`.
If we wanted to ensure that we were defining the correct fields we can bind the type
and then assert that the value is of the correct type.
```bash
$ dhall <<< "let User = { name : Text, age : Natural, email : Text } in { name = \"Fintan\", age = 25, email = \"fintan dot halpenny at gmail dot com\" } : User"
{ name : Text, age : Natural, email : Text }

{ name = "Fintan", age = 25, email = "fintan dot halpenny at gmail dot com" }
```

Just to prove to ourselves that Dhall is type checking correctly, let's leave off the email
value and see what happens.
```bash
$ dhall <<< "let User = { name : Text, age : Natural, email : Text } in { name = \"Fintan\", age = 25 } : User"

Use "dhall --explain" for detailed errors

Error: Expression doesn't match annotation

{ - email : …
, …
}


{ name = "Fintan", age = 25 } : User


(stdin):1:60
```

We can access one or more record fields use the `.` accessor.
```bash
$ dhall <<< "{ name = \"Fintan\", age = 25 }.age"
Natural

25
```

```bash
$ dhall <<< "{ name = \"Fintan\", age = 25 }.{ age, name }"
{ age : Natural, name : Text }

{ age = 25, name = "Fintan" }
```

As well as records we can define union types. For example we can enumerate the days of the week.
```
$ dhall <<< "
< Monday : {}
| Tuesday : {}
| Wednesday : {}
| Thursday : {}
| Friday : {}
>
"
Type

< Monday : {} | Tuesday : {} | Wednesday : {} | Thursday : {} | Friday : {} >
```

And construct values of union types using the `constructors` keyword:
```bash
$ dhall <<< "
    let Days =
          < Monday :
              {}
          | Tuesday :
              {}
          | Wednesday :
              {}
          | Thurday :
              {}
          | Friday :
              {}
          >

in  let DaysConstructors = constructors Days

in  DaysConstructors.Monday {=}
"
< Monday : {} | Tuesday : {} | Wednesday : {} | Thurday : {} | Friday : {} >

< Monday = {=} | Tuesday : {} | Wednesday : {} | Thurday : {} | Friday : {} >
```

When we want to collapse union data we use the `merge` keyword:
```bash
$ dhall "
    let Days =
          < Monday :
              {}
          | Tuesday :
              {}
          | Wednesday :
              {}
          | Thurday :
              {}
          | Friday :
              {}
          >

in  let DaysConstructors = constructors Days

in  let doesGarfieldHate =
            λ(day : Days)
          → merge
            { Monday =
                λ(_ : {}) → True
            , Tuesday =
                λ(_ : {}) → False
            , Wednesday =
                λ(_ : {}) → False
            , Thurday =
                λ(_ : {}) → False
            , Friday =
                λ(_ : {}) → False
            }
            day

in        if doesGarfieldHate (DaysConstructors.Monday {=})

    then  \"Garfield hates Mondays...\"

    else  \"Garfield is happy today!\"

Text

"Garfield hates Mondays..."
"
```

## Either

Ok, so that was a whirlwind tour of Dhall and I'm sure we missed some things along the way
but it should be enough to get us writing a self defined `Either` data type. If we were
to go off of the knowledge we covered above, our first attempt at `Either` would be:
```
< Left : a | Right : b >
```

That is to say, we have a `Left` union entry of type `a`, and a `Right` union entry of type `b`.
The question is, where do `a` and `b` come from? Well, this is where type functions come in.
In Dhall, types are passed along to say what types of things we are working with. Let's see
this in action with the full definition of `Either`:
```bash
$ dhall <<< "\(a : Type) -> \(b : Type) -> < Left : a | Right : b >"
∀(a : Type) → ∀(b : Type) → Type

λ(a : Type) → λ(b : Type) → < Left : a | Right : b >
```

Notice how the output uses `∀`, `λ`, and `→`. We can format our files to use these symbols,
and I strongly recommend you do so. If we put the above definition in a file `Either/type` and
run `dhall format --inplace ./Either/type`, it will convert all the symbols for us. So pretty 😍.

Let's see our `Either` in action! Assuming you're following along and have defined the above in
`Either/type` we can try the following:
```bash
$ dhall <<< "let E = constructors (./Either/Type Text Natural) in E.Right 2"
< Left : Text | Right : Natural >

< Right = 2 | Left : Text >
```

```bash
dhall <<< "let E = constructors (./Either/Type Text Natural) in E.Left \"Hello\""
< Left : Text | Right : Natural >

< Left = "Hello" | Right : Natural >
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
