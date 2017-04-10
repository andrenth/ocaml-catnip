# Catnip

This library provides a bunch of Category Theory stuff for OCaml. Its main
practical use is to enable programming with Free Monads in OCaml, which is
usually done in combination with the
[Interpreter Pattern](https://en.wikipedia.org/wiki/Interpreter_pattern).
The `examples` directory has code that shows how that can be done with Catnip.

The rest of it is mainly for my own amusement, although things like
Applicatives and Kleisli composition allow really clean-looking code,
especially in a language with no built-in syntatic sugar for monadic code.
