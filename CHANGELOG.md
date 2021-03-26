# Revision history for type-theory

## 0.1.0.0 -- 2021-03-18

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2021-03-21

* Started tracking changes in the changelog. Oops.
* Added natural numbers. The recursor syntax is `natrec{n.t;x;n p.y;n}`.
* Added syntax for natural numbers. To use this for a type other than the builtin one, create a function `fromNat : Nat->x` for some `x`.
* Fixed issues with let blocks.
* Bottom and Top now erase to free variables instead of identity functions, allowing slightly shorter programs.

## 0.3.0.0 -- 2021-03-25

* Added proof-annotated types, which allow you to annotate values with definitionally irrelevant proofs. (They probably have another name already.) Construct these types with `A[x.P]` and their terms with `x{p:P}`. `A[x.B]` is a subtype of `A`, and the proof component can be extracted using `useproof{X;P;x;Y;p.y}`, where `X` is the type of the proof-annotated term being analyzed, `P` is the type of the proof to extract, `x` is the proof-annotated term to extract a proof from, `Y` is the return type, and `y` is the output. Note that all outputs of `\p.y p` must be definitionally equal.
* Bottom is now definitionally proof-irrelevant, so that a function of type `Top[_.Bottom]->Bottom` is definable.
* Changed erasure of Bottom again, since I don't think using random stuff as Bottoms preserves termination.

## 0.3.0.1 -- 2021-03-26
* `* : (?{*:?} : *2[_.?])` now typechecks.
* Change the version in the .cabal file. Oops.
