(* Copyright Per Lindgren 2016, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* cimp/Parser.mly *)

%token <State__StateGen.id> ID 
%token <int> INTVAL
%token IF THEN ELSE END WHILE DO DONE FOR
%token TRUE FALSE AND NOT BEQ BLE BBE BL BB BNE OR BXR
%token SC LP RP ASSIGN PLUS MINUS MULT
%token EOF
%token <string> STRINGVAL

%left SC
%left OR
%left BXR
%left AND
%left MINUS PLUS
%left MULT
%right NOT


%{
  open Imp__Imp 
  open Common
  open Env
  open Why3extract.Why3__BigInt
  open State__StateGen
%}

%start prog

%type <Imp__Imp.com> prog 

%%

prog:
  | com EOF                        { $1 }
  
com: 
  | com SC com                     { Cseq ($1, $3) }
	| com SC                         { $1 }
  | ID ASSIGN aexpr                { Cassign ($1, $3) }
  | IF bexpr THEN com ELSE com END { Cif ($2, $4, $6) }  
  | IF bexpr THEN com END          { Cif ($2, $4, Cskip) }  
  | WHILE bexpr DO com DONE        { Cwhile ($2, $4) }
  | FOR LP initID = ID ASSIGN initexp = aexpr SC cond = bexpr SC addcom = com RP DO body = com DONE {
        Cseq(Cassign (initID, initexp), Cwhile (cond, Cseq (body, addcom)))	 
    }
	| ID PLUS PLUS                   { Cassign ($1, Aadd (Avar ($1), Anum (of_int 1))) }
	| ID MINUS MINUS                 { Cassign ($1, Asub (Avar ($1), Anum (of_int 1))) }
	| ID PLUS BEQ aexpr              { Cassign($1, Aadd (Avar $1, $4)) }
	| ID MINUS BEQ aexpr             { Cassign($1, Asub (Avar $1, $4)) }

bexpr:
  | LP bexpr RP                    { $2 }
  | TRUE                           { Btrue }
  | FALSE                          { Bfalse }
  | bexpr AND bexpr                { Band ($1, $3) }
	| bexpr OR bexpr                 { Bnot (Band (Bnot $1, Bnot $3)) }
	| bexpr BXR bexpr                { Band (Bnot (Band (Bnot $1, Bnot $3)), Bnot (Band ($1, $3)))  }
  | NOT bexpr                      { Bnot ($2) }
	| aexpr BL aexpr                 { Ble ($1, Asub ($3, Anum (of_int 1))) }
	| aexpr BB aexpr                 { Ble (Aadd ($3, Anum (of_int 1)), $1 ) }
  | aexpr BEQ aexpr                { Beq ($1, $3) }
  | aexpr BLE aexpr                { Ble ($1, $3) }
	| aexpr BBE aexpr                { Ble ($3, $1) }
	| aexpr BNE aexpr                { Bnot (Beq ($1, $3)) }


aexpr:
  | LP aexpr RP                    { $2 }
  | INTVAL                         { Anum (of_int $1) }
  | ID                             { Avar $1 }
  | aexpr PLUS aexpr               { Aadd ($1, $3) }
  | aexpr MINUS aexpr              { Asub ($1, $3) }
  | aexpr MULT aexpr               { Amul ($1, $3) }
	| MINUS aexpr                    { Asub (Anum (of_int 0), $2) }




