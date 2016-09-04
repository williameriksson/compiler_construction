exception Error

type token = 
  | WHILE
  | TRUE
  | THEN
  | STRINGVAL of (string)
  | SC
  | RP
  | PLUS
  | NOT
  | MULT
  | MINUS
  | LP
  | INTVAL of (int)
  | IF
  | ID of (State__StateGen.id)
  | FALSE
  | EOF
  | END
  | ELSE
  | DONE
  | DO
  | BLE
  | BEQ
  | ASSIGN
  | AND


val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Imp__Imp.com)