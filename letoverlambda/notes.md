### Chapter 1 -- introduction

This book is about learning when and why to use Macros.

Focus on combinations of macros -- one of the area that CS research haven't fully explored because it frightens researchers.

> Coding style is necessary only where understanding is missing
Style is more like guideline for novice programmers, once we reached understanding we no longer need the guideline.

### Chapter 2 -- Closures

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

### Chapter 3 -- Macro basics

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
