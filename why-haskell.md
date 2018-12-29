# Me

1. reason is using types to solve problems. They're a powerful idea that can be reasoned about with some simple math (Algebra!). You can translate a problem spec to your types and then work in that specific domain. And then they're all checked so you don't fuck anything up.
2. We use programs to automate a lot of tasks for us. That's sort of what they're invented for. So it's a reasonable choice to have a computer automate checking our programs for us so that we don't make mistakes. And GHC is one of the best programs around that does that for us.
3. Because of the above two, refactoring is a fucking breeze. You make a change and then you can see all the parts of the system that get affected by that change. When I wrote Python I had to keep a mental model of all the parts that I was affecting by making a change somewhere else. Which also falls into reasoning about what you're writing.

Haskell ecosystem: https://github.com/Gabriel439/post-rfc/blob/master/sotu.md

Anecdotes of Python:
* Using C to make it faster
* Concurrency
* SQS count

Trust between programmers, by talking in types.

# Sandy

i like that hs gives you really high-level tools to work with
i think it's really informative to look at quicksort written in c vs written in python vs written in haskel
it keeps getting smaller because your intrinsic building blocks become more and more powerufl
and most of the time you don't pay a runtime performance cost for that developer performance gain
i feel like all of the most inspiring and smartest people i know who do software all do hs
there is always more to know
it seems like a lot of the memes about haskell are that we just jerk off talking about semigroupoids and monoids in the category of endofunctors and etc etc
but like, learning these things is fantastic---it's good for your brain, the material generalizes outside of haskell, and a lot of times it IS the exact solution to a problem you would have written a shit solution for otherwise
languages like C++ also "always have more to know", but i would claim that's a bug there
because the knowledge you learn from C++ doesn't really generalize. it's like "how can combine these obscure, arcane features, and god what happens when i do"
#3) i hate debugging
man i hate debugging
it's just so shitty
i like that i almsot never need to debug in haskell
(and that i almost never need to write tests!!)
the only times in the last year i've done any debugging is when i've been screwing around with honest-to-goodness math
haskell is the only language i know of where i don't need to debug

# Ian

maintainability is my single most important reason.

Compososability

# Paul

Actually, I can summarize as “the ability to understand my code with about 4 9’s reliability before ever running it.”

# Kris

I want a machine to be able to check my work, and to be able to change my software easily while keeping it working correctly. Haskell provides me with the best tools for both.

# Kyle

The big one for me is that FP (in scala not just haskell) helps to make your code much more easily testable, or in some cases even removes the need for testing

# Tim

for me it's brittleness - type errors are a good message bus between colleagues. it also makes everybody separate their concerns a little better than they otherwise might.

can work really well at a team scale, though it doesn't always
and performance is pretty good given the level of abstraction

# Ross

Substitution and locality of reasoning.  I can zoom in and out without paralyzing fear that something off screen is trying to eat me.

# Greg

Yeah, I saw … it’s a good question. I think for me it’s the “constraints liberate” argument. Along with that is the ability to reason about it. Even more than with “actual math” – like, I can’t skip logical steps or anything – the compiler’s like “but how do you _know_ m + n is n + m”?

FP has done more to improve my understanding of all sorts of things than anything else has. Like CT isn’t really encodable in FP, but without typed FP, I don’ think I’d have even a fraction of the appreciation & understanding of rigor that is needed for math in general.

# Joe

- Shared, mutable state appears to be strongly linked to incidental complexity in programs and functional programming _seems_ to be one of the better tools we have for battling this
   - "Out of the Tar Pit" and "Can Programming Be Liberated from the von Neumann Style?" feel like they point towards this (although I haven't finished reading the former, and have only skimmed the latter)

- Strong typing and an expressive type system allows the compiler to "understand" more of the nuance of my code, so I can both rely on it to check my work _and_ to automatically generate a lot of useful code from the structural representation of my models
   - Generic derivation of JSON ser/de, Arbitrary typeclass instances, and a lot of the Servant stuff seem to be decent examples here

- Having a tag (i.e. `IO`) that separates certain runtime system operations that are potentially complex and error prone from "pure" operations means that I can enforce much stronger guarantees on the responsibilities that pieces of code have
   - This is especially helpful when trying to figure out how liberal I can be with some refactoring; if I have a lot of restricted code, I can take more liberties with aggressively moving stuff around, since I know that there likely won't be much logic that the compiler won't be able to check

# Chris

- Haskell and a useful type system make my job more predictable. They permit me to focus on the interesting stuff and to spend less time fixing mundane functionality and to write fewer tests while achieving the same level of quality assurance.
- Haskell’s concurrency structures and patterns give me something akin to superpowers for writing parallel & concurrent programs. I don’t have to rewrite my whole program to sprinkle some efficient concurrency in where it is needed. GHC thread preemption means I can always yank a run-away sub-program stage right as needed too.
- Haskell’s expressiveness, which derives in part from it being a functional language, makes it so that there’s no trade-off in obtaining the above benefits. I can write code as concise and readable as anything else with better performance and less time spent fixing runtime bugs.

# Nick

Abstraction, trust in the compiler, I never have to keep the entire program in my head

# Matt

safety, expressive power, library quaility
