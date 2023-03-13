# PROMELA

## Abstract
This is the **K** semantic definition of Promela, the modeling language for SPIN model checker.
Promela includes the following features:
- Processes. Promela is by nature a language for concurrent processes.
- Guards. Guards are typically used with nondeterministic choices, where an option can be chosen
  only if its guard is executable. In general, whenever an expression is used a statement,
  it blocks the following statements.
- Arrays. Promela supports one dimensional arrays. Multidimensional arrays can be constructed indirectly
  with the use of typedef definitions.

## Syntax
[Here](https://spinroot.com/spin/Man/grammar.html) is the official definition of the syntax of Promela.
For clarity, our k definition directly follows the official one.
```k
module PROMELA-SYNTAX
  imports DOMAINS-SYNTAX

//  syntax Spec ::= Module Modules // TODO ambiguity in Modules or Spec
// maybe syntax Spec ::= Module will do? (subsort declaration)
  syntax Module ::= Proctype
                  | Init
                  | Mtype
                  | DeclLst

  syntax Proctype ::= "active" "proctype" Name "(" ")" "{" Sequence "}"
                    | "proctype" Name "(" ")" "{" Sequence "}"

  syntax Init ::= "init" "{" Sequence "}"

  syntax Mtype ::= "mtype" "=" "{" Ids "}"

  syntax OneDecl ::= Typename Ivars [klabel(one_decl)]

  syntax Typename ::= "bit" | "byte" | "short" | "int" | "bool" [klabel(bool_T)] | "mtype" | "chan"

  syntax Sequence ::= Step
                    > Step ";" Sequence [klabel(seq)]
                    | Step "->" Sequence [macro]
 
  syntax Step ::= Stmnt
                | DeclLst

  syntax Ivar ::= Name 
                | Name "[" Const "]" [klabel(arr_ivar)]
                | Name "=" AnyExpr [strict(2), klabel(ivar)]
                | Name "[" Const "]" "=" AnyExpr

  syntax Varref ::= Name 
                  | Name "[" AnyExpr "]" [klabel(arr_varref)]

  syntax Assign ::= Varref "=" AnyExpr [klabel(assign)]
                  | Varref "++"
                  | Varref "--"

  syntax Stmnt ::= "do" Options "od"
                 | Assign
                 | "printf" "(" String ")"
                 | Expr

  syntax Options ::= "::" Sequence
                   | Options Options    // TODO can I do this with syntactic lists?

  syntax Binarop ::= "+" | "-" | "*" | "/" | "%" | "&" | "^" | "|"
                   | ">" | "<" | ">=" | "<=" | "==" | "!="
                   | "<<" | ">>"

  syntax AnyExpr ::= "(" AnyExpr ")" [bracket]
                   | AnyExpr Binarop AnyExpr  // TODO how about short-circuiting?
                   | Varref // TODO for now, varref also takes care of mtype values. fix this!
                   | Const

  syntax Expr ::= AnyExpr
                | "(" Expr ")" [bracket]

  syntax Name ::= Id

  syntax Const ::= Bool | "skip" | Int | Id // Id for mtype

  syntax Ids ::= NeList{Id, ","} [klabel(ids)]

  syntax Modules ::= NeList{Module, ""} // TODO why ambiguous?

  syntax DeclLst ::= OneDecl ";" DeclLst [klabel(decl_lst), prefer]
                   > OneDecl  // [klabel(decl_lst)]

  syntax Ivars ::= Ivar | Ivar "," Ivars [klabel(ivars)]
```

```k
endmodule

module PROMELA
  imports PROMELA-SYNTAX
  imports DOMAINS
```

## Configuration
```k
  configuration <T color="yellow">
                  <mtype color="pink"> .Set </mtype>
                  <procs color="LightSalmon"> // inactive procs
                    .List
                  </procs>
                  <threads color="orange"> // active procs
                    <thread multiplicity="*" color="blue">
                      <k color="green"> $PGM:Modules ~> execute </k>
                      <env color="LightSkyBlue"> .Map </env>
                    </thread>
                  </threads> 
                  <output color="Orchid"> .List </output>
                  <genv color="purple"> .Map </genv>
                  <store color="Salmon"> .Map </store>
                  <debug> .List </debug>
                </T>
```

## Values
Arrays evaluate to array reference values holding the location and the size of the array.
```k
  syntax Val ::= Int | Bool | String | array(Int, Int) | loc(Int) | mval(Id) // bit, byte, short
  syntax Name ::= Val
  syntax AnyExpr ::= Val
  syntax KResult ::= Val
```


## Initialization
In order to initialize the configuration with a K cell, preprocessing step is done
as defined in the `Definitions & Declarations` section.
After that, the preprocessing K cell is left with the `execute` token, which
gives signal to actually start off from the initialized configuration, and then disappears.
```k
  syntax KItem ::= "execute"
  rule <k> execute => . </k>
       <env> Env </env>
       <genv> .Map => Env </genv> [structural]     
```

## Definitions & Declarations

### Module Declaration
A Promela Specification is just a sequence of `Module`s.
```k
  /*** Module ***/
  rule M:Module ML:Modules => M ~> ML [structural]

  /*** Init ***/
  rule <k> init { S:Sequence } => . ...</k>
       (.Bag => <thread>... <k> S </k> ...</thread>) [structural]

  /*** Proctype ***/
  rule <k> active proctype _:Id ( ) { S:Sequence } => . ...</k>
       (.Bag => <thread>... <k> S </k> ...</thread>) [structural]
  rule <k> proctype _:Id ( ) { S:Sequence } => . ...</k>
       <procs> .List => ListItem(S) ...</procs> [structural]

  rule .Modules => . [structural]
  rule <thread>... <k> . </k> ...</thread> => .Bag [structural]

  /*** Mtype ***/
  rule mtype = { .Ids } => . [structural]
  rule <k> mtype = { (C:Id, CL:Ids => CL) } ...</k>
       <mtype>... .Set => SetItem(C) ...</mtype> [structural]
```

### Variable Declaration
The syntactic category `DeclLst` includes multiple declarations with the syntactic category `OneDecl`,
separated by the separator ";".
Declarations with the same type can be compactified into a single declaration inside `OneDecl`,
separated by the separator ",".
For simplicity, we decompose both `DeclLst` and `OneDecl` into atomic declarations, and define the K rules
for declaration in terms of atoms.
TODO: (including arrays)
- variable scoping
- init to all zero
```k
  syntax KItem ::= "undefined" /* used in declarations with default values */

  /* Decomposing Multiple Declarations */
  rule (D:OneDecl ; DL:DeclLst):DeclLst => D ~> DL [structural]
  rule T:Typename I:Ivar, IL:Ivars => T I ~> T IL [structural]

  /* (Syntactic Sugar) Declaration with Initialization */
  rule T:Typename X:Id = E:AnyExpr => T X ~> (X = E):Assign [structural]

  /* Without Initialization */
  rule <k> _:Typename X:Id => . ...</k>
       <env> Rho => Rho[X <- !L:Int] </env>
       <store>... .Map => !L |-> undefined ...</store> [structural]
```

### Array Declaration
```k
  //context int _:Id[HOLE]

  /* Without Initialization */
  rule <k> _:Typename X:Id [ I:Int ] => . ...</k>
       <env> Env => Env[X <- !N:Int] </env>
       <store>... .Map => !N |-> array(!N +Int 1, I)
                          (!N +Int 1) ... (!N +Int I) |-> undefined ...</store>
    requires I >=Int 0
```

## Sequences
```k
  /*** Sequence & Step ***/
  rule S:Expr ; SL:Sequence => Guard(S) ; SL [structural] 
  rule S:Step ; SL:Sequence => S ~> SL [structural, owise] 
    // Caution: In order for owise to work, var name should coincide (i.e. both rules should use S, SL)
  rule Guard(E:Expr) ; SL:Sequence => Guard(E) ~> SL [structural]
  rule S:Step -> SL:Sequence => S ; SL [structural] // syntactic sugar
```

## Statements
```k  
  /* Stmnt */
  rule do OL:Options od => OL ~> do OL od [structural]
```

### Assignments
```k
  rule <k> _ = (E:AnyExpr => Eval(E, L, G, S)) ...</k>
       <env> L </env> <genv> G </genv> <store> S </store> requires notBool(isKResult(E))

  context (HOLE => lvalue(HOLE)) = _

  rule <k> loc(L):Varref = V:Val => . ...</k> <store>... L |-> (_ => V) ...</store>
```

### I/O
```k
  rule <k> printf ( S:String ) => . ...</k>
       <output>... .List => ListItem(S) </output> [print] 
```


## Expressions
We establish a separate semantics for expressions.
While others are defined operationally, we define the semantics for expressions denotationally.
This is to facilitate defining the blocking semantics for guards.
Note that no interleaving occurs during the evaluation of expressions in Promela.

### Arithmetic Expressions
```k
//  syntax KItem ::= Eval(Expr, Map, Map, Map) [function] // expr, lenv, genv, store

  syntax Int ::= Eval(Expr, Map, Map, Map) [function]
//  rule Eval(I:Int, _, _, _) => I [simplification]

  //rule Eval(X:Varref, L, G, S) => #if L[X] =/=K #False #then S[L[X]] #else #if G[X] =/=K #False #then S[G[X]] #else #False #fi #fi [simplification]
//  rule Eval(X:Varref, L, _, S) => S[L[X]] requires L[X] =/=K #False [simplification]
  //rule Eval(X:Varref, _, G, S) => S[G[X]] requires G[X] =/=K #False [simplification, owise] // owise -> if then

//  rule Eval(E1:AnyExpr + E2:AnyExpr, L, G, S) => Eval(E1, L, G, S) +Int Eval(E2, L, G, S) [simplification]
//  rule Eval(E1:AnyExpr - E2:AnyExpr, L, G, S) => Eval(E1, L, G, S) -Int Eval(E2, L, G, S) [simplification]
//  rule Eval(E1:AnyExpr * E2:AnyExpr, L, G, S) => Eval(E1, L, G, S) *Int Eval(E2, L, G, S) [simplification]
//  rule Eval(E1:AnyExpr / E2:AnyExpr, L, G, S) => Eval(E1, L, G, S) /Int Eval(E2, L, G, S) requires Eval(E2, L, G, S) =/=K 0[simplification]

```

### Boolean Expressions
```k
//  syntax Bool ::= Eval(Expr, Map, Map, Map) [function]
//  rule Eval(E1:AnyExpr == E2:AnyExpr, L, G, S) => Eval(E1, L, G, S) ==K Eval(E2, L, G, S) [simplification]
```


### Mtypes
```k
/*  syntax AnyExpr ::= Mval
  syntax Mval ::= Mvalue(Id)
  rule <k> C:Id => Mvalue(C) ...</k>
       <mtype>... SetItem(C) ...</mtype> [structural] */
```


### Variable Lookup
```k
  /* Variable Lookup */
  rule <k> X:Varref => V ...</k>
       <genv>... X |-> L ...</genv>
       <store>... L |-> V ...</store> [lookup]
```

## Guard Semantics
In Promela, if an expression is used as a statement, it behaves as a guard -
that is, it "blocks" its following statements so long as it evaluates to false.
Dealing with the semantics of guards is quite tricky, because if naively dealt,
you might end up with infinite loops for merely checking if the guard is true over and over again.
Hence, we need to provide a mechanism to determine the result of the guard in a nontrivial way - i.e. only checks whenever something as changed.
The idea is to separate the guard semantics from the main semantics,
so that the evaluation of the expression takes place at the meta level which does not affect the main computation.

Once the guard becomes "executable", it should disappear.


```k
  syntax Step ::= Guard(Expr)
 // rule <k> Guard(E:Expr) => . ...</k> <env> L </env> <genv> G </genv> <store> S </store> requires Eval(E, L, G, S)
```

```k
  /* Guard Semantics */
  //syntax Step ::= Guard(Step) // TODO better change it to Step -> Expr ??
  //syntax Bool ::= Executable(Step, Map, Map) [function] // arg: Guard, genv, store TODO add (local) env
  //syntax KItem ::= GuardEval(AnyExpr, Map, Map) [function]
```

### Executable
TODO: binary relations e.g. >Int, >K in the definition don't parse. 
Somehow need to embed GuardEval into values such as Int so >Int on them makes sense.
```k
  /* Executable */
  //rule Executable(Guard(E1:AnyExpr > E2:AnyExpr), Rho, Sig) => GuardEval(E1, Rho, Sig) >K GuardEval(E2, Rho, Sig) [simplification]
  //rule Executable(Guard(E1:AnyExpr == E2:AnyExpr), Rho, Sig) => GuardEval(E1, Rho, Sig) <K GuardEval(E2, Rho, Sig) [simplification]
  //rule Executable(Guard(E1:AnyExpr == E2:AnyExpr), Rho, Sig) => GuardEval(E1, Rho, Sig) >=K GuardEval(E2, Rho, Sig) [simplification]
  //rule Executable(Guard(E1:AnyExpr == E2:AnyExpr), Rho, Sig) => GuardEval(E1, Rho, Sig) <=K GuardEval(E2, Rho, Sig) [simplification]
  //rule Executable(Guard(E1:AnyExpr == E2:AnyExpr), Rho, Sig) => GuardEval(E1, Rho, Sig) ==K GuardEval(E2, Rho, Sig) [simplification]
  //rule Executable(Guard(E1:AnyExpr != E2:AnyExpr), Rho, Sig) => GuardEval(E1, Rho, Sig) =/=K GuardEval(E2, Rho, Sig) [simplification]
```

### GuardEval
```k
  /* GuardEval */
  //rule GuardEval(X:Varref, Rho:Map, Sig:Map) => Sig[Rho[X]] [simplification]
  //rule GuardEval(C:Int, _:Map, _:Map) => C [simplification]

  //rule <k> Guard(S) => . ...</k>
  //     <genv> Rho </genv> <store> Sig </store> requires Executable(Guard(S), Rho, Sig)

  /*rule <k> Guard(E1:AnyExpr == E2:AnyExpr) ...</k>
       <genv> Rho </genv> <store> Sig </store>
       <debug> .List =>
                 ListItem(_:Varref :=K E1) // true
                 ListItem(_:Int :=K E2) // true
                 ListItem(_:Map :=K Rho) // true
                 ListItem(Executable(Guard(E1 == E2), Rho, Sig)) // false
                 ListItem(Sig[Rho[E1]])
                 ListItem(E2)
                 ListItem(Sig[Rho[E1]] ==K E2)
                 ListItem(GuardEval(E1, Rho, Sig))
                 ListItem(GuardEval(E2, Rho, Sig))
       </debug> */
```

## Auxiliary
```k
  syntax Map ::= Int "..." Int "|->" K [function]
  rule N...M |-> _ => .Map requires N >Int M
  rule N...M |-> K => N |-> K (N +Int 1)...M |-> K requires N <=Int M
```

```k
  syntax Varref ::= lvalue(K)
  syntax Varref ::= lookup(Int)
  syntax Varref ::= loc(Int)

  // global variable
  rule <k> lvalue(X:Varref => loc(L)) ...</k> <genv>... X |-> L:Int ...</genv> [structural]
  // local variable
  rule <k> lvalue(X:Varref => loc(L)) ...</k> <env>... X |-> L:Int ...</env> [structural]

  context lvalue(_:Id [HOLE:AnyExpr])
  context lvalue(HOLE::Id [_::AnyExpr])

  rule array(L,_) [ I:Int ] => lookup(L +Int I) [structural, anywhere]

  rule lvalue(lookup(L:Int) => loc(L)) [structural]
```

```k
endmodule 
```
