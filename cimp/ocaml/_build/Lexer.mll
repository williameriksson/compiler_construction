(* Copyright Per Lindgren 2016, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* cimp/Lexer.mll *)

{    
 open Parser   
 open Lexing
 open Common
 open Error  
 open State__StateGen
 open Env
   
 exception SyntaxError of string 

 (* helpers *)
 let badd buf lexbuf = Buffer.add_string buf (Lexing.lexeme lexbuf) 
}

(* regular expressions (regexps) *)
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id      = ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']*  
let digits  = ['0'-'9']+

  
(* lexing rules *)  
rule lex = parse
  | "IF"                 { IF }
  | "THEN"               { THEN }
  | "ELSE"               { ELSE }
  | "END"                { END }
  | "WHILE"              { WHILE }
  | "DO"                 { DO }
  | "DONE"               { DONE }

  | "TRUE"               { TRUE } 
  | "FALSE"              { FALSE }
  | "&&"                 { AND } 
  | "NOT"                { NOT }
  | "="                  { BEQ } 
  | "<="                 { BLE }  
 
  | ';'                  { SC } 
  | ":="                 { ASSIGN }
  | '+'                  { PLUS }
  | '-'                  { MINUS }
  | '*'                  { MULT }              
  
  | digits as i          { INTVAL (int_of_string i) }           (* literals/values *)
  
  | id as s              { ID (Id (add_id s)) }
  | white                { lex lexbuf }                         (* white space *)
  | newline              { next_line lexbuf; lex lexbuf }       (* new line *)
  | "//"                 { set_info lexbuf; comment lexbuf }    (* single line comment *) 
  | "(*"                 { set_info lexbuf; comments 0 lexbuf } (* nested comment *) 
  | '('                  { LP }                                 (* must come after comment *)
  | ')'                  { RP }
  | eof                  { EOF }
  | _                    { raise (SyntaxError("Unknown Symbol.")) }
        
and comment = parse
  | newline              { next_line lexbuf; lex lexbuf }
  | eof                  { EOF }                                (* // at last line *)
  | _                    { comment lexbuf; }    
    
and comments level = parse
  | "*)"                 { if level = 0 then lex lexbuf else comments (level-1) lexbuf }
  | "(*"                 { comments (level+1) lexbuf }
  | newline              { next_line lexbuf; comments level lexbuf }
  | _                    { comments level lexbuf }
  | eof                  { bol lexbuf; raise (SyntaxError("Comment not closed.")) }
    
                         
    
    
