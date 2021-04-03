# type-theory
Haskell implementation of normalization by evaluation and bidirectional type checking based on <https://jozefg.github.io/papers/2019-implementing-modal-dependent-type-theory.pdf>

Run with `cabal run`. Tested on GHC 8.10.4, probably works with others.

Example code:

    nonzero = \n.natrec{_.*;Bottom;_ _.Top;n} : Nat -> *
    unsquash = \p.usesquash{p;Bottom;b.b} : squash Bottom -> Bottom
    pred = \n.natrec{m.squash (nonzero m) -> Nat;unsquash;pn _.\_.pn;n} (getproof{nonzero n;n}) : Nat[n.nonzero n] -> Nat
