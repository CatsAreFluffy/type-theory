# type-theory
Haskell implementation of normalization by evaluation and bidirectional type checking based on <https://jozefg.github.io/papers/2019-implementing-modal-dependent-type-theory.pdf>

Run with `cabal run`. Tested on GHC 8.10.4, probably works with others.

Example code:

    nonzero = \n.natrec{_.*;Bottom;_ _.Top;n} : Nat -> *
    squash = \t.Top[_.t] : * -> *
    inst = \t q v.useproof{t[x.q x];q v;v;squash (q v);p.*{p:q v}} : ^t:*.^q:t->*.^v:t[x.q x].squash (q v)
    unsquash = \p.useproof{squash Bottom;Bottom;p;Bottom;b.b} : squash Bottom -> Bottom
    pred = \n.natrec{m.squash (nonzero m) -> Nat;unsquash;pn _.\_.pn;n} (inst Nat nonzero n) : Nat[n.nonzero n] -> Nat
