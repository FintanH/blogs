# Bowl Full of Lentils

In this blog post I'm going to take a break from Haskell
and spread the good word of [Dhall](https://github.com/dhall-lang/dhall-lang/).
The good news is that it won't be a break from functional programming. We still have all the good stuff in Dhall like: lambdas, products, sums and types!
We'll take a look through some of the basics of Dhall and work our way up to defining `Either`.
This will be a two part series and in the second part we'll take the definition of `Either` and
work with it to see some more functional concepts in Dhall.

## Introduction to Dhall

One can almost view Dhall as JSON with functions and types, but really it's so much better.
There is an excellent [tutorial](https://hackage.haskell.org/package/dhall-1.16.1/docs/Dhall-Tutorial.html)
that lives in the library. It goes through the simple usage of Dhall and how you can utilise
Dhall files in your Haskell code. I will try summarise here the main points to give you a taste.
To _really_ get a feel we can grab the Dhall executable. An easy way to get it is if you have
[`stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install), we can run `stack install dhall --resolver=lts-12.0`; grabbing the `dhall` package (version 1.16.1) at LTS-12.0.

The native types to Dhall can be enumerated:
* Bool
* Natural
* Integer
* Double
* Text
* List
* Optional
* Unit

## Basic Types

So let's take a look at them via the `dhall` command. Running the following we can see what we
can do with Dhall.

### Booleans

```bash
$ dhall <<< "True && False"
Bool

False
```

### Naturals

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

### Integers and Doubles

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

### Text

```bash
$ dhall <<< "\"Hello\" ++ \" World\""
Text

"Hello World"
```

### Lists

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

### Optionals

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

### Unit

```bash
$ dhall <<< "{=}"
{}

{=}
```
The Unit type looks like an empty record, which segues us onto our next topic!


## Records

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


## Unions

As well as records we can define union types. For example we can enumerate the days of the week.
```bash
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
