module PROMELA-SYNTAX
  imports DOMAINS-SYNTAX

//  syntax Spec ::= Module Modules // TODO ambiguity in Modules or Spec
// maybe syntax Spec ::= Module will do? (subsort declaration)
  syntax Module ::= Proctype
                  | Init
                  | Mtype

  syntax Proctype ::= "active" "proctype" Id "(" ")" "{" Sequence "}"
                    | "proctype" Id "(" ")" "{" Sequence "}"

  syntax Init ::= "init" "{" Sequence "}"

  syntax Mtype ::= "mtype" "=" "{" Id Ids "}"

  syntax Sequence ::= Steps

  syntax Step ::= Stmnt
  syntax Stmnt ::= "printf" "(" String ")"

  syntax Ids ::= List{Id, ","}
  syntax Modules ::= List{Module, ""} // TODO why ambiguous?
  syntax Steps ::= NeList{Step, ";"}

endmodule

module PROMELA
  imports PROMELA-SYNTAX
  imports DOMAINS
  syntax KResult ::= String

  configuration <T color="yellow">
//                <procs> non-active processes declared in the spec </procs>
                  <threads color="orange">
                    <thread multiplicity="*" color="blue">
                      <k color="green"> $PGM:Modules </k>
                    </thread>
                  </threads>
                  <output color="Orchid"> .List </output>
                </T>

  // Module
  rule M:Module ML:Modules => M ~> ML [structural]

  // Initialize
  rule <k> init { S:Sequence } => . ...</k>
       (.Bag => <thread>... <k> S </k> ...</thread>) [structural]
  rule <k> active proctype P:Id ( ) { S:Sequence } => . ...</k>
       (.Bag => <thread>... <k> S </k> ...</thread>) [structural]
  rule .Modules => . [structural]
  rule <thread>... <k> . </k> ...</thread> => .Bag [structural]

  // Sequence
  rule .Steps => . [structural]
  rule S:Step ; SL:Steps => S ~> SL [structural]

  // Stmnt
  rule <k> printf ( S:String ) => . ...</k>
       <output>... .List => ListItem(S) </output> [print] 

endmodule 
