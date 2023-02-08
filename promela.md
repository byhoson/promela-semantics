# PROMELA

## Abstract
This is the **K** semantic definition of Promela, the modeling language for SPIN model checker.

## Syntax
```k
module PROMELA-SYNTAX
  imports DOMAINS-SYNTAX

//  syntax Spec ::= Module Modules // TODO ambiguity in Modules or Spec
// maybe syntax Spec ::= Module will do? (subsort declaration)
  syntax Module ::= Proctype
                  | Init
                  | Mtype
                  | DeclLst

  syntax Proctype ::= "active" "proctype" Id "(" ")" "{" Sequence "}"
                    | "proctype" Id "(" ")" "{" Sequence "}"

  syntax Init ::= "init" "{" Sequence "}"

  syntax Mtype ::= "mtype" "=" "{" Ids "}"

  syntax OneDecl ::= Typename Ivars
//                   | Typename Ivars "=" AnyExpr

  syntax Typename ::= "int" | "bool" | "mtype"

  syntax Sequence ::= Steps

  syntax Step ::= Stmnt
                | DeclLst

  syntax Ivar ::= Id
                | Id "=" AnyExpr [strict(2), klabel(ivar)]

  syntax Varref ::= Id

  syntax Assign ::= Varref "=" AnyExpr [strict(2), klabel(assign)]
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
                   | AnyExpr Binarop AnyExpr [strict(1,3)] // TODO how about short-circuiting?
                   | Varref // TODO for now, varref also takes care of mtype values. fix this!
                   | Const
                // but if a new nonterminal is introduced for mtype, how to distinguish?
  syntax Expr ::= AnyExpr
                | "(" Expr ")" [bracket]

  syntax Const ::= Bool | "skip" | Int

  syntax Ids ::= NeList{Id, ","} [klabel(ids)]
  syntax Modules ::= NeList{Module, ""} // TODO why ambiguous?
  syntax Steps ::= NeList{Step, ";"} | Steps "->" Steps // TODO handle -> better!
  syntax DeclLst ::= NeList{OneDecl, ";"}
  syntax Ivars ::= NeList{Ivar, ","} [klabel(ivars)]
//  syntax Ivars ::= Ids // TODO added this because the error msg required subsort decl. resolve this later
```

```k
endmodule

module PROMELA
  imports PROMELA-SYNTAX
  imports DOMAINS
```

## Configuration
```k
  syntax KResult ::= Int | Bool | String | Mval

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

```k
  syntax KItem ::= "execute"
  rule <k> execute => . </k>
       <env> Env </env>
       <genv> .Map => Env </genv> [structural]     
```

## Definitions & Declarations
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

  /*** DeclLst ***/
  rule .DeclLst => . [structural]
  rule D:OneDecl ; DL:DeclLst => D ~> DL [structural]

  /*** OneDecl TODO distinguish b/w local & global ***/
  rule _:Typename .Ivars => . [structural]
  // TODO decl w/o initialization
//  rule <k> mtype (X:Id = C:Id, IL:Ivars => IL) ...</k>
//       <mtype>... SetItem(C) ...</mtype>
//       <genv> Rho => Rho[X <- !N:Int] </genv>
//       <store>... .Map => !N |-> C ...</store> [structural]
/////       <genv>... .Map => X |-> C ...</genv> [structural]
  rule <k> int (X:Id = C:Int, IL:Ivars => IL) ...</k>
       <env> Rho => Rho[X <- !N:Int] </env>
       <store>... .Map => !N |-> C ...</store> [structural]


```

## Sequences
```k
  /*** Sequence & Step ***/

//  syntax Bool ::= Executable(Step) [function]
//  rule Executable(_:Step) => true [simplification]

  rule .Steps => . [structural]
  rule S:Expr ; SL:Steps => Guard(S) ; SL [structural] // requires _:Expr :=K S [structural]
  rule S:Step ; SL:Steps => S ~> SL [structural, owise]
  rule Guard(S) ; SL:Steps => Guard(S) ~> SL [structural]
  //rule Guard(S) => S ~> Guard(S) [structural]
  //rule true ~> Guard(_) => . [structural]
  //rule false ~> Guard(S) => . ~> Guard(S) [structural]
  // if you define the above rule as 'false ~> .', trouble happens when the program contains false
  // TODO how to resolve inifinite loop for false guards?
  rule S:Step -> SL:Steps => S ; SL [structural] // syntactic sugar


  //rule .Steps => . [structural]
  //rule S:Step -> SL:Steps => S ; SL [structural]
  //rule SL1:Steps -> SL2:Steps => SL1 ~> SL2 [structural, owise] // Syntactic Sugar -> TODO can't i use concat for syntactic lists instead of ~>?
  //rule S:Step ; SL:Steps => S ~> SL [structural] // requires Executable(S) [structural]
```

## Statements
```k  
  /* Stmnt */
  rule do OL:Options od => OL ~> do OL od [structural]

  // ASSIGNMENT rule X:Varref = I:Int

  rule <k> printf ( S:String ) => . ...</k>
       <output>... .List => ListItem(S) </output> [print] 

  // Options TODO: implement nondeterministic choice
  rule :: S:Sequence => S [structural]
```

## Expressions
```k
  rule I1:Int + I2:Int => I1 +Int I2
  rule I1:Int - I2:Int => I1 -Int I2
  rule I1:Int * I2:Int => I1 *Int I2
  rule I1:Int / I2:Int => I1 /Int I2



  // AnyExpr 
  //rule E1:AnyExpr == E2:AnyExpr
  rule Mvalue(C:Id) == Mvalue(C:Id) => true [equality]
  rule Mvalue(_:Id) == Mvalue(_:Id) => false [equality, owise]

  // TODO: distinguish from mtype, global & local
  // TODO: IMPORTANT!!! what if the exp evaluates to false??? should give it a second chance!!!!
  // at the same time, should avoid inifinite loop due to consecutively evaluating to false
  rule <k> X:Varref => V ...</k>
       <genv>... X |-> L ...</genv>
       <store>... L |-> V ...</store> [lookup]

  syntax AnyExpr ::= Mval
  syntax Mval ::= Mvalue(Id)
  rule <k> C:Id => Mvalue(C) ...</k>
       <mtype>... SetItem(C) ...</mtype> [structural]


  /* Expr */
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
  /* Guard Semantics */
  syntax Step ::= Guard(Step) // TODO better change it to Step -> Expr ??
  syntax Bool ::= Executable(Step, Map, Map) [function] // arg: Guard, genv, store TODO add (local) env
  syntax KItem ::= GuardEval(AnyExpr, Map, Map) [function]
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
  rule Executable(Guard(E1:AnyExpr == E2:AnyExpr), Rho, Sig) => GuardEval(E1, Rho, Sig) ==K GuardEval(E2, Rho, Sig) [simplification]
  rule Executable(Guard(E1:AnyExpr != E2:AnyExpr), Rho, Sig) => GuardEval(E1, Rho, Sig) =/=K GuardEval(E2, Rho, Sig) [simplification]
```

### GuardEval
```k
  /* GuardEval */
  rule GuardEval(X:Varref, Rho:Map, Sig:Map) => Sig[Rho[X]] [simplification]
  rule GuardEval(C:Int, _:Map, _:Map) => C [simplification]

  rule <k> Guard(S) => . ...</k>
       <genv> Rho </genv> <store> Sig </store> requires Executable(Guard(S), Rho, Sig)

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
endmodule 
```
