Murex
=====

Murex is a programming language with several goals:

  * Support programming in-the-large
  * Support domain specific languages, both embedded and compiled
  * Move closer to dependent typing, while keeping the type system decidable
  * Create a competitor to ML w.r.t. parametric modules
  * Improve on Haskell's ad-hoc overloading
  * Show how to improve Lisp syntax
  * Use a high-level linker to eliminate dependency hell

Above all else, though, every decision that goes into Murex is motivate by what I call the "Five Pillars of Programming".

Five Pillars of Programming
===========================

Below, I give some small descriptions of each of the pillars. No pillar is any more or less important thatn any of the others. If any pillar is weak in some code or a programming environment, frustration is in store for the developers.

The implications of each directive are usually subtle, but also not meant to be grasped here. My decisions, as they relate to Murex, will be explained in the definitino with reference to these pillars.

Reliability
-----------
  * Given valid input, produce correct output.
  * Given invalid input report an error (at least "invalid input" or the like) which is trivially distinguishable from successful output.
  * Given invalid input perform no non-trivial side-effects. Trivial side-effects include memoization and logging.
  * Given an arbitrary, breaking change to the code, minimize the possibility that change makes it to runtime.

Efficiency
----------
  * Minimize latency.
  * Maximize throughput.
  * Minimize resource use (memory, files, sockets).

Concision
---------
  * Minimize development time.
  * Minimize repetitive or paradigmatic code.
  * Write no code unrelated to the problem at hand.
  * Minimize complexity.

Clarity
-------
  * Rigorous documentation.
  * Strict coding standards.
  * Extra-API documentation.

Communication
-------------
  * Error messages help explain the problem.
  * Version control.
  * Issue tracking.
  * Continuous integration: check interfaces, documentation, coding standards, test coverage.

Features
========

As I am still exploring some of the design space, this list is both incomplete. Since I've only just started implementation, many of these features don't actually exist yet.

  * Higher-kinded, higher-rank, polymorphic static type system with GADTs.
  * An integration of exceptions, first-class control, and winding protection.
  * Higher-order module system.
  * Compiletime ad-hoc overloading.
  * First-class patterns and rules.
  * Compiletime metaprogramming over a homoiconic abstract syntax.
  * Concurrency with first-class channels.
  * Modular, user-driven distributed fixity

Possible Features
-----------------

  * Limited support for dependent typing: type-level naturals and booleans. Dependent typing is nice for verification, too much dependent typing, and you have to debug your encoding of the specifications.
  * Mixin modules.
  * Datatype-generic programming.
  * Open data types.

Implementation Plan
===================

At this point, I have only just begin. There is roughtly no implementation whatsoever.

I plan to build Murex roughly according to the stages outlined below. The goal is to have a working system from the beginning. It's always fun to see a program execute, especially when it's actually two programs at once.

Stage One
---------
  * Basic data types (Number, Char, List, IO, finite sums and products)
  * Interpreter
  * Parser

Stage Two
---------
  * Syntactic sugar
  * Distfixes
  * Finite sums and products

Stage Three
-----------
  * Type and data constructors
  * Pattern matching

Stage Four
----------
  * Type checking
  * Reference cells

Stage Five
----------
  * Module system

Stage Six
---------
  * Multiple files
  * Basic linker

Stage Seven
-----------
  * Macros
  * Syntax abstraction

Stage Eight
-----------
  * Concurrency

Stage Nine
----------
  * Cleanup 
  * Compiler
  * REPL

Further
-------
At this point, I'll have a better understanding of the possible features and may begin integrating some. The definition should be nearly complete. I'll have a tiny set of librareis that I will begin to clean and expand on. As we gain users, I'll need a package archive. The compiler will need to do optimizations. We can begin work on foreign function interfaces to C and JavaScript, among others.




