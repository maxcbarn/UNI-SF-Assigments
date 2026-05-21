# Assignment — Big-Step Semantics in Haskell

## Overview

This assignment implements a **big-step (natural) semantics** interpreter for a simple imperative language, written in Haskell. The interpreter evaluates arithmetic expressions, boolean expressions, and commands over a memory state (σ).

---

## Language Definition

The language is defined by three abstract syntax trees (ASTs):

### Arithmetic Expressions — `E`

| Constructor | Description |
|---|---|
| `Num Int` | Integer literal |
| `Var String` | Variable lookup in memory |
| `VarDin E` | Dynamic variable: evaluates `E` to get the variable name at runtime |
| `Soma E E` | Addition |
| `Sub E E` | Subtraction |
| `Mult E E` | Multiplication |
| `Div E E` | Integer division |

### Boolean Expressions — `B`

| Constructor | Description |
|---|---|
| `TRUE` / `FALSE` | Boolean literals |
| `Not B` | Negation |
| `And B B` | Logical conjunction (short-circuit) |
| `Or B B` | Logical disjunction (short-circuit) |
| `Leq E E` | Less than or equal (`e1 <= e2`) |
| `Igual E E` | Equality (`e1 == e2`) |

### Commands — `C`

| Constructor | Description |
|---|---|
| `Skip` | No operation |
| `Atrib E E` | Variable assignment (`x := e`) |
| `Seq C C` | Sequential composition |
| `If B C C` | Conditional |
| `While B C` | While loop |
| `TenTimes C` | Executes command `C` exactly 10 times |
| `Repeat C B` | Repeat `C` until `B` is true |
| `Loop E E C` | Executes `C` a total of `(e2 - e1)` times |
| `DuplaATrib E E E E` | Simultaneous double assignment: `v1 := e1` and `v2 := e2` |
| `AtribCond B E E E` | Conditional assignment: `v := e1` if `b`, else `v := e2` |
| `Swap E E` | Swaps the values of two variables |

---

## Memory

Memory (`Memoria`) is represented as an association list of variable names and their integer values:

```haskell
type Memoria = [(String, Int)]
```

Two helper functions manage memory:

- `procuraVar :: Memoria -> String -> Int` — looks up a variable's value
- `mudaVar :: Memoria -> String -> Int -> Memoria` — updates a variable's value

---

## Semantic Functions

| Function | Signature | Description |
|---|---|---|
| `ebigStep` | `(E, Memoria) -> Int` | Evaluates an arithmetic expression |
| `bbigStep` | `(B, Memoria) -> Bool` | Evaluates a boolean expression |
| `cbigStep` | `(C, Memoria) -> (C, Memoria)` | Executes a command, returning the final memory state |

---

## Implemented Programs

### Factorial
Computes `y = x!` using a `While` loop.
```haskell
-- Memory: [("x", 5), ("y", 0)]
cbigStep(fatorial, exSigma)
```

### Sum from 1 to N
Computes the sum `1 + 2 + ... + n` using `Loop`.
```haskell
-- Memory: [("s",1), ("n",5), ("sum",0)]
programSumToN
```

### Fibonacci
Computes the n-th Fibonacci number using `Repeat` and `DuplaATrib`.
```haskell
-- Memory: [("lastlast",0),("last",0),("n",1),("step",0),("steps",8)]
programFibonaci
```

### Maximum of Two Numbers
Finds the maximum between `x` and `y` using `AtribCond` / `If`.
```haskell
-- Memory: [("x",20), ("y",10), ("max",0)]
maxProgram
```

### Bubble Sort
Sorts an array stored in memory using `Loop`, `Swap`, and `VarDin` for dynamic indexing.
```haskell
-- Memory: [("size",10),("index",0),("0",1),("1",5),("2",2),...]
programBubbleSort
```

To run with a custom memory:
```haskell
programBubbleSortMem :: Memoria -> (C, Memoria)
programBubbleSortMem mem = cbigStep(bubbleSort, mem)
```

> **Note:** Bubble sort requires the `VarDin` extension to `E`, which enables runtime dynamic variable name resolution (e.g., accessing `arr[i]` where `i` is a variable).

---

## How to Run

Load the solution file in GHCi:

```bash
ghci assigment-1-solution.hs
```

Example calls:

```haskell
-- Factorial of 5
cbigStep(fatorial, [("x",5),("y",0)])

-- Sum 1..5
programSumToN

-- 8th Fibonacci number
programFibonaci

-- Bubble sort
programBubbleSort

-- Bubble sort with custom input
programBubbleSortMem [("size",3),("index",0),("0",3),("1",1),("2",2)]
```

---

## Files

| File | Description |
|---|---|
| `assigment-1-definitiion.hs` | Base definitions and partially implemented functions (student template) |
| `assigment-1-solution.hs` | Complete solution with all semantic rules and example programs |
