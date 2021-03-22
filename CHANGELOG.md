# Revision history for type-theory

## 0.1.0.0 -- 2021-03-18

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2021-03-21

* Started tracking changes in the changelog. Oops.
* Added natural numbers. The recursor syntax is `natrec{n.t;x;n p.y}`.
* Added syntax for natural numbers. To use this for a type other than the builtin one, create a function `fromNat : Nat->x` for some `x`.
* Fixed issues with let blocks.
* Bottom and Top now erase to free variables instead of identity functions, allowing slightly shorter programs.
