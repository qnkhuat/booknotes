## Chapter 1 -- introduction

This book is about learning when and why to use Macros.

Focus on combinations of macros -- one of the area that CS research haven't fully explored because it frightens researchers.

> Coding style is necessary only where understanding is missing
Style is more like guideline for novice programmers, once we reached understanding we no longer need the guideline.

## Chapter 2 -- Closures

The book name `let over lambda` is another name for lexical scoping or Closure.

Lisp and clojure has both lexical scoped and dynamically scoped variable.
In clojure it's defined by assigning a meta `:dynamic`

Python doesn't have dynamic scoped, so that's why you can't do the same thing as you do with dynamic var in Clojure.

In lisp, the `lambda` is called __function designator__, means it's not a function,
but it desginate a list as a function so we can call `function` on it: `(function '(lambda (x) (+ 1 x)))`,
A short hand is to use the read-macro: `#'(lambda (x) (+ 1 x))`

There is no creation over-head for lambda, once they're compiled they're stored persistently in memory and ready for reuse.

We could think of lambda as procedure with states, thus a lot of OOP problem could be solved using lambda.

```lisp
(let ((counter 0))
(lambda () (incf counter)))
```
a lambda that increases counter every time it's called

## Chapter 3 -- Macro basics

Symbol is a thing designed for efficient comparision, especially with `eq`.

So many things like time-unit, we use symbol instead of string for it.

The author argued that using free variable is a good technique even though it's not idomatic.
The technique is called Free variable injection in which you have a closure that has a free variable, the varialbe only bound
when they are used

```lisp
(let ((x 1))
(lambda () x)) ;; < x is free in this lambda but is bound under `let`
```

Lisp supports both dynamic scope and lexical scope, some argues this is a bad thing. But the beneift is it gives us a lots of power and make it easy to write macro

## Chapter 4 -- Read macros
- `#.` reader make a form to be evaluate at read time ( for exmaple you want to store a now - where now is the time the code was read )
- `*` varialbe stores last result from REPL
- `+` variable stores the current form

`(equal * (eval +))`

The performance myth about: low-level langugage let you write more performance code because it let you write code closer to the hardware is false.
This is because the lower the langugage, the more it prevents you from writing the optimizsations that actually matter.

`cl-ppcre` is one great example: it's a regular expression langugage where the version in lisp runs twice as fast as the one in PERL.
This optimizsations come from the ability to optimize at compile time

`##` and `#=` are 2 read macros that let you write self-referential S-expressions

## Chapter 5 -- Program that write programs

The real purpose of functional programming is to seperate the functional description of what should happen from the mechanics of how it actualy does happen.

When writing a macro, you want to start with an abstraction first.

The first step is to understand exactly what you want to achieve with understanding the use cases of it first.

## Chapter 6 -- Anaphoric macros

Anaphoric macros are macros that deliberately capture variables from forms supplied to the macro.

For example this macros will inject a `self` variable
```lisp
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
    #'self))
```

Also learn about pandroic macro, hot patching

## Chapter 7 -- Macro efficiency topics
> If you tile a floor with tiles the size of your thumbnail, you don't waste many
> - Paul Graham

Opitmising data structures essentially come down to a concept of "locality" -- frequent access data should be fastest to access

Macros and declartion doesn't work well together, so we need to use read macros

Compiler macros are optimisations to function invocations, not for the functions themselves

Pointer scope like we had in C can be emulated in lisp using closures.
Pointers are dangerous and the source of many security bugs. It's accidental complexity and could be avoided using closures.
Rathers than just being a fixnum that can be used as an address, closures are code that is compiled to retrieve and set any sort of data in any sort of env.


Sorting networks offers a faster way to do sort for a fixed size array.
The author demonstrate that for an array with size of 3, the orders of doing sort matters.

And there is a way to generate a sorting orders automatically and it's called Batcher's algorithm

## Chapter 8 - Implementing Forth

Forth is a stack-based language and often used in embedding devices

Author argue that Forth has a very high local maximum in the space of language design.

In forth there are 2 stacks: parameter stack and return stack

Thead words are basically command that either defined in forth's core, or users.

One interesting aspect about Forth is thread, and it's not the conventional kind of threading.
Most popular threading are indrect threaded, but most forth implementaiton are direct threaded

Thread is a sequence of forht words that are executed in a particular order.

Threads in Forth provide a powerful way to organize and structure your code, making it easier to understand and maintain.

;; https://en.wikipedia.org/wiki/Threaded_code
Learn more about direct, indirect, subroutine and token threading.

"subroutine-threaded code" (also "call-threaded code") consists of a series of machine-language "call" instructions (or addresses of functions to "call", as opposed to direct threading's use of "jump")

Token-threaded code implements the thread as a list of indices into a table of operations; the index width is naturally chosen to be as small as possible for density and efficiency.
