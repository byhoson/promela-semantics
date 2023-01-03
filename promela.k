module PROMELA-SYNTAX
  imports DOMAINS-SYNTAX

  syntax Ids ::= List{Id, ","}
  syntax Modules ::= List{Module, ""} // TODO why ambiguous?

  syntax Spec ::= Module Modules

  syntax Module ::= Proctype
                  | Init
                  | Mtype

  syntax Proctype ::= "active" "proctype" Id "(" ")" "{" Sequence "}"
                    | "proctype" Id "(" ")" "{" Sequence "}"

  syntax Init ::= "init" "{" Sequence "}"

  syntax Mtype ::= "mtype" "=" "{" Id Ids "}"

  syntax Sequence ::= Steps

  syntax Step ::= Stmnt
  syntax Steps ::= NeList{Step, ";"}

  syntax Stmnt ::= "printf" "(" String ")"


endmodule

module PROMELA
  imports PROMELA-SYNTAX
  imports DOMAINS

  configuration <T>
                  <spec> $PGM:Spec </spec>
                  <threads>
                    <thread multiplicity="*" type="Set">
                      <id> -1 </id>
                      //<k>  </k>
                    </thread>
                  </threads>
                  <output> .List </output>
                  <state> .Map </state>
                </T>

  rule <spec> M:Module => .Module ... </spec>

  // 
//  rule S1:Stmt S2:Stmt => S1 ~> S2  [structural]

  // Print
//  rule <k> printf(S:String, Es => Es); ...</k> <output>... .List => ListItem(V) </output>
//    [print]
//  rule print(.Vals); => .  [structural]
endmodule
