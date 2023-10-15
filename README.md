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
b ::= x = t | x = T

-- Term
t ::= x
    | λp: T. t
    | \p: T. t -- same with using λ.
    | t1 t2
    | t as T
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
T ::= Bool | Nat | Unit | T1 -> T2 | x
```

## Interface
You can interface with the interpreter through a simple repl.
Apart from the evaluation of the term, the repl also prints out the term's type.

#### Examples
- `Nats` and `Bool`s:
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

- `Unit` and wildcard:
```
λ> \_:Unit.unit
λ_: Unit. unit
:: Unit -> Unit
λ> (\_:Unit. \x:Unit -> Bool. x unit) unit
λx: Unit -> Bool. x unit
:: (Unit -> Bool) -> Bool
λ> (\_:Unit. \x:Unit -> Bool. x unit) unit (\_:Unit. true)
true
:: Bool
```

- Sequence expressions:
```
λ> \x:Unit.x;true
Type error:
Term 'λx: Unit. x' should be of type 'Unit' in 'λx: Unit. x; true'.
λ> (\x:Unit.x) unit; true
true
:: Bool
```

- Type ascription:
```
λ> x = \x:Bool. unit as Unit
λx: Bool. unit as Unit
:: Bool -> Unit
λ> x true as Unit
unit
:: Unit
λ> x (true as Unit)
Type error:
Term 'true'cannot be ascribed with type 'Unit'.
λ> (\x:Bool. unit) as Bool -> Unit
λx: Bool. unit
:: Bool -> Unit
λ> if true then 3 as Nat else 1 as Nat
3
:: Nat
λ> unit; iszero 3 as Bool
false
:: Bool
```

- Type aliases:
```
λ> UU = Unit -> Unit
UU = Unit -> Unit
λ> f = \x: UU. x unit
λx: UU. x unit
:: UU -> Unit
λ> f (\x:Unit. x)
unit
:: Unit
λ> NB = Nat -> Bool
NB = Nat -> Bool
λ> my_iszero = (\n:Nat. iszero n) as NB
λn: Nat. iszero n
:: NB
λ> my_iszero 3
false
:: Bool
λ> my_iszero 0
true
:: Bool
```
## Build/Run Instructions
Just run `stack run` in the root directory. If you want to just build the project you can run `stack build` instead.
