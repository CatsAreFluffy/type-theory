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

## 0.3.0.2 -- 2021-03-27
* `natrec` now expects the correct type in the successor case.

## 0.4.0.0 -- 2021-03-28
* Added dependent pairs. Construct these types with `&x:A.B` and their terms with `pair{a,b}`. The projections are `left{p}` and `right{p}`.
* Fixed a potential issue with Pi subtyping.

## 0.5.0.0 -- 2021-04-03
* Added squash types. Construct these types with `squash`. `squash A` is always a supertype of `A`, so you don't need a constructor. Additionally, `squash` is idempotent and all squashed types are proof-irrelevant. The eliminator is `usesquash{p;Y;p.y}`, where `p` is the squashed value to eliminate, `Y` is the return type, and `y` is the returned value. As with `useproof`, `\p.y` must be constant.
* Terms of proof-annotated types can be directly constructed using squashed proofs (for example, `*{Nat:squash *} : ?[x.x]`), and the proofs can be directly extracted as elements of squash types using `getproof{P;x}` where `x` is a proof-annotated term and `squash P` is the type of the proof to extract from it.
* `useproof` is still available, but might be broken and/or removed in the future. I recommend replacing `useproof{X;P;x;Y;p.y}` with `usesquash{getproof{P;x:X};Y;p.y}`, possibly omitting the type annotation on `x`.
* Handling of `Abort`s in the discriminees of some eliminators was changed. Since `Bottom` is proof-irrelevant, this shouldn't change anything for users.
* Proof-annotated terms now check that the provided proof type is in fact a type.
* Cumulativity into `Sort LevelAboveW` (also known as `*x`) is now allowed.
