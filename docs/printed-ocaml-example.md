### Additional Restrictions for Rules with the Same Primary Component

For example:

```sml
eval_b E B1 falseVal O
--------------------------- [E-AndFalse1]
eval_b E and(B1, B2) falseVal O


eval_b E B1 trueVal O1
eval_b E B2 falseVal O2
appendOutput O1 O2 O
--------------------------- [E-AndFalse2]
eval_b E and(B1, B2) falseVal O


eval_b E B1 trueVal O1
eval_b E B2 trueVal O2
appendOutput O1 O2 O
-------------------------- [E-AndTrue]
eval_b E and(B1, B2) trueVal O
```
The corresponding generated ocaml code is:
```ocaml
let rec eval_b e var309 = 
 match e , var309 with 
 ...
  | (_, And (b1, b2)) -> 
     (match eval_b e b1 with 
      | (FalseVal, o) -> (*E-AndFalse1*)
         (FalseVal, o)  
      | _ ->
         (match (eval_b e b1, eval_b e b2) with 
         | ((TrueVal, o1), (FalseVal, o2)) -> (*E-AndFalse2*)
            let o = appendOutput o1 o2 in 
            (FalseVal, o)
         | _ ->
            (match (eval_b e b1, eval_b e b2) with
             | ((TrueVal, o1), (TrueVal, o2)) -> (*E-AndTrue*)
                let o = appendOutput o1 o2 in 
                (TrueVal, o)
             | _ ->
                raise (Failure "should not reach here"))))
   | ... 
```
If we walk through the logic, we will find that it tries to match `AndFalse1`'s
`eval_b E B1 falseVal O` first because the output has a constructor that
our compiler determines is a control flow term, and specifically to match
the output value to `(falseVal O)`. If this is not the case, it will proceed to
check `E-AndFalse2`. The compiler finds that
the first two are both distinguishing terms, so we **combine** those two
together into a single match case. If the return value of the
distinguishing term is a free variable, like `o1` or `o2`, we will simply not
match it and proceed with the computation, essentially only matching it partially.
Then it will compute `appendOutput O1 O2` to get `O` as before.
We do the same thing again on `E-AndTrue`. If none of the above match, it will
raise a failure.

When you write the SOS, you need to keep in mind that
the mechanics of translation are *from top to bottom*. Sometimes
the way you write it can affect the semantics. 
