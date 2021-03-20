# type-theory
Haskell implementation of normalization by evaluation and bidirectional type checking based on <https://jozefg.github.io/papers/2019-implementing-modal-dependent-type-theory.pdf>

Run with `cabal run`. Tested on GHC 8.10.4, probably works with others.

Example code:

    nat = ^t:*.(t->t)->t->t : *
    2 = \t f x.f (f x) : nat
    4 = \t.2 (t->t) (2 t) : nat