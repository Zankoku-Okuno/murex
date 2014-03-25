# Murex

Murex is a programming language with several goals:

  * Take programming in-the-large seriously (i.e. scientifically)
  * Create a competitor to ML w.r.t. parametric modules
  * Use a high-level linker to eliminate dependency hell
  * Improve on Haskell's ad-hoc overloading
  * Support domain specific languages, both embedded and compiled
  * Move closer to dependent typing, while keeping the type system decidable
  * Show how to improve Lisp syntax

Above all else, though, every decision that goes into Murex is motivated by what I call the "Five Pillars of Programming", which I'll get to further below. First, let's see the feature rundown.

## Features

Instead of just giving a list of features and letting you work out the implications, I'll first give a layman's rundown of usefulness. The advantages of polymorphic static typing with type inference are clear, and type inference keeps code concise. Higher kinds allow for types to be parameterized by type constructors as well as normal types; higher ranks allow for all sorts of data abstraction, particularly statically verified region-local effects. An ML-style module system is still the best-known strategy for achieving modularity (i.e. abstraction and extension), and I hope to integrate it with Haskell-style ad-hoc overloading to reduce the syntactic burden. With delimited continuations and winding protection, we can have all the power of first-class control, but still be able to abstract and protect resource management. Channel-based concurrency (ala pi-calculus) is exactly the expressive power of Erlang, but allows for anonymous communication and makes clear dynamic reconfigurations of processes.

As I am still exploring some of the design space, this list is both incomplete. Since I've only just started implementation, many of these features don't actually exist yet.

  * Higher-kinded, higher-rank, polymorphic static type system with GADTs
  * Type inference
  * Higher-order functions
  * Higher-order module system
  * High-level linker
  * Concurrency with first-class channels
  * An integration of exceptions, first-class control, and winding protection
  * First-class patterns and rules
  * Compiletime ad-hoc overloading
  * Compiletime metaprogramming over a homoiconic abstract syntax
  * Modular, user-driven distributed fixity
  * Dynamic types with casts that give appropriate blame
  * Unicode source code support

### Possible Features

  * Limited support for dependent typing: type-level naturals and booleans. Dependent typing is nice for verification, too much dependent typing, and you have to debug your encoding of the specifications.
  * Mixin modules.
  * Datatype-generic programming.
  * Open data types.

## Five Pillars of Programming

Below, I give some small descriptions of each of the pillars. No pillar is any more or less important than any of the others. If any pillar is weak in some code or a programming environment, frustration is in store for the developers.

The implications of each directive are usually subtle, but also not meant to be grasped here. My decisions, as they relate to Murex, will be explained in the definition with reference to these pillars.

### Reliability
  * Given valid input, produce correct output.
  * Given invalid input report an error (at least "invalid input" or the like) which is trivially distinguishable from successful output.
  * Given invalid input perform no non-trivial side-effects. Trivial side-effects include memoization and logging.
  * Given an arbitrary, breaking change to the code, minimize the possibility that change makes it to runtime.

### Efficiency
  * Minimize latency.
  * Maximize throughput.
  * Minimize resource use (memory, files, sockets).

### Concision
  * Minimize development time.
  * Minimize repetitive or paradigmatic code.
  * Write no code unrelated to the problem at hand.
  * Minimize complexity.

### Clarity
  * Rigorous documentation.
  * Strict coding standards.
  * Extra-API documentation.

### Communication
  * Error messages help explain the problem.
  * Version control.
  * Issue tracking.
  * Continuous integration: check interfaces, documentation, coding standards, test coverage.

## Implementation Plan

At this point, I have only just begin. There is roughly no implementation whatsoever.

I plan to build Murex roughly according to the stages outlined below. The goal is to have a working system from the beginning. It's always fun to see a program execute, especially when it's actually two programs at once.

### Stage One
  * Basic data types (Number, Char, List, IO, finite sums and products)
  * Interpreter
  * Parser

### Stage Two
  * Syntactic sugar
  * Distfixes
  * Finite sums and products

### Stage Three
  * Multiple files
  * Basic linker

Scheduled cleanup

### Stage Four
  * Type and data constructors
  * Pattern matching

### Stage Five
  * Type checking
  * Reference cells

### Stage Six
  * Module system

Scheduled cleanup

### Stage Seven
  * Macros
  * Syntax abstraction

### Stage Eight
  * Spawn
  * Channels
  * Locking references

### Stage Nine
  * Compiler
  * REPL

Scheduled cleanup

### Further
At this point, I'll have a better understanding of the possible features and may begin integrating some. The definition should be nearly complete. I'll have a tiny set of libraries that I will begin to clean and expand on; we'll especially need a good OS interface. As we gain users, I'll need a package archive. The compiler will need to do optimizations. We can begin work on foreign function interfaces to C and JavaScript, among others. Since locking algorithms are not compositional, we'll need software transactional memory.




