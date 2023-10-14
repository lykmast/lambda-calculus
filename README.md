# lambda-calculus
~ *Another lambda calculus interpreter* ~

Developed while reading through
[Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) by Benjamin C. Pierce.

## The calculus
The interpreter currently supports terms of the 
[simply typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) 
extended with:
- `Bool`s, `Nat`s and `Unit`
- top level bindings, in the form: `x = t`

## Syntax
```haskell
-- Bind
b ::= x = t 

-- Term
t ::= x
    | λp: T. t
    | \p: T. t -- same with using λ.
    | t1 t2
    | t1; t2
    | (t)
    | true
    | false
    | n -- natural number.
    | succ t
    | pred t
    | iszero t
    | if t1 then t2 else t3

-- Pattern
p ::= x
    | _

-- Type
T ::= Bool | Nat | Unit | T1 -> T2
```

## Interface
You can interface with the interpreter through a simple repl.
Apart from the evaluation of the term, the repl also prints out the term's type.

#### Example
```
λ> \x:Nat. x
λx: Nat. x
:: Nat -> Nat
λ> idBool = \x:Bool.x   
λx: Bool. x
:: Bool -> Bool
λ> idBool true
true
:: Bool
```

## Build/Run Instructions
Just run `stack run` in the root directory. If you want to just build the project you can run `stack build` instead.
