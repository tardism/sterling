
# Translating SOS Rules to OCaml
We assume you have read the `docs/semantic` folder for basic semantic definitions.
However, those definitions are for a Prolog-like system, which is far more flexible
than an OCaml-like system. Below are the restrictions we added to make Structured Operational Semantics (SOS) 
definitions suitable for OCaml code generation.

## Restrictions on Judgment Declarations
We have added the following restrictions on judgment declarations.

### Judgment Structure

A judgment must have at least three components:
- The **first component** is an input (typically an environment or context)
- The **second component** is the **primary component** (marked with `*`), which is the matched structure (also an input)
- The **third component** is the output type (the resulting value)

A judgment declaration can be understood as the signature of a function in OCaml.
For judgments with more than three arguments, the components up to and including
the primary component are inputs that can be used for computation; components
after the primary are outputs.

You might ask: doesn't OCaml only support one output value? Yes, but we use tuples
to wrap multiple output values into one. The outputs can be either variables that
are computed from the premises (the conditions above the inference bar), or they
can be constructors of an inductive datatype defined earlier.

### The Primary Component

We extend the meaning of "primary" - it also serves an important role in pattern
matching cases in OCaml. The primary component determines which pattern matching
case will be used in the generated OCaml code.

Tou can think a judgment declaration is OCaml function signature.
For example, the declaration:
```
Judgment eval_a : [(string, value)] a* value output
```
can be thought of as an OCaml function with the signature:
```ocaml
val eval_a : [(string, value)] -> a -> (value * output)
```

## Restrictions on Writing Inference Rules

There are additional restrictions on writing inference rules (i.e., writing the
operational semantics). You need to think about these rules as if you were writing
OCaml code.

### Rule Structure Requirements

**The consequent (below the inference bar):**
- The first term must be a judgment name you declared previously
- The remaining terms must be instances of inductive datatypes or variables
- If you use a data constructor, its arguments must be variables
- We do not accept nested data constructors like `f(g(x))`

**Multiple rules with the same judgment:**
You can have many inference rules with the same judgment name as the consequent.
Sometimes the primary component may also be the same across multiple rules.
In such cases, the premises must be different to distinguish between the rules. 

### Example: Value Equality

Here is a basic example from [examples/imp_executable/host/eval.sos](../examples/imp_executable/host/eval.sos):

```
value ::= intVal(int)
        | trueVal
        | falseVal

Judgment val_eq : value value* value


I1 == I2
-------------------------- [VE-Int-True]
val_eq intVal(I1) intVal(I2) trueVal

I1 != I2
-------------------------- [VE-Int-False]
val_eq intVal(I1) intVal(I2) falseVal
```

In this example:
- The judgment `val_eq` has three components: two input values (the second is the primary component marked by `*`), and one output value
- Rules `VE-Int-True` and `VE-Int-False` both have the same judgment name and primary component `intVal(I2)`
- The premises are different: `I1 == I2` versus `I1 != I2`
- This leads to different output values: `trueVal` versus `falseVal`

### Variable Naming Constraints

Due to limitations in our system, you must ensure that, in the consequent, arguments of data constructors
use the same variable names in the same positions across all rules with the same primary component, except for the output
(ones after the primary component).
For example, in both `VE-Int-True` and `VE-Int-False`'s consequent, the first variable is `I1` and the second is `I2`.

This constraint exists because the generated OCaml code uses if-else chains like:
```ocaml
if I1 == I2 then trueVal
else if I1 != I2 then falseVal
else raise (Failure "no rule matched")
```

The generated code is very mechanical. If you use different constructor argument names
across rules, our algorithm cannot group `VE-Int-True` and `VE-Int-False` together,
and some cases may never be reached.

### Additional Restrictions for Rules with the Same Primary Component

When multiple rules in a module share the same primary component, the following
restrictions apply:

1. **The Distinguishing Premise**: There must be exactly ONE premise that is critical
   for distinguishing different rules with the same primary component. We call this
   the *distinguishing premise*.

2. **Shared Prefix Premises**: Sometimes you need additional premises before the
   distinguishing premise (for example, to evaluate some of the consequent's variables for input of the distinguishing premise). If you have *n* premises before the distinguishing premise,
   then all *n* of these premises must be exactly the same in all other rules that
   share the same primary component.

3. **Forms of the Distinguishing Premise**: After the shared prefix premises, the
   distinguishing premise must take one of two forms:

   a. **A comparison between two expressions**, such as:
      - `E1 == E2`, `E1 != E2`, `E1 > E2`, `E1 < E2`, etc.

   b. **A relational judgment**, but with one or more different inductive datatype constructors
      after the primary component. The return values must be different across rules. The
      generated code will perform pattern matching on the result of this relation
      to determine which rule's consequent to use.


---

These are all the hard restrictions imposed on top of the original extensible judgment definition! 
