
(* The type of tokens. *)

type token = 
  | WHILE
  | TRUE
  | THEN
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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Imp__Imp.com)
