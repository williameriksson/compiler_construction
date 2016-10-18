(* Copyright Per Lindgren 2016, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* cimp/Main *)

open Common
open Options
open Error
open Cmd
open Lexing
open Dump
open Compiler__Compile_com
open Listmap__Const
open Listmap__ListMap
open Vm_ex__VmEx
open Imp_ex__ImpEx
open Env
open Why3extract.Why3__BigInt
open ImpToMips
open Ast_opt__AexprOpt


let getProg (opt : bool) (lexbuf : lexbuf) : Imp__Imp.com =
	match opt with
	| false -> Parser.prog Lexer.lex lexbuf
	| true -> com_opt (Parser.prog Lexer.lex lexbuf)


let main () =

  cmd; (* parse command line options and put into opt *)
  p_stderr (string_of_opt opt);
  
  let inBuff =
    try Some (open_in opt.infile)
    with _ -> None
  in
  try
    match inBuff with
    | None -> raise (CompilerError("File open error :" ^ opt.infile))
    | Some inBuffer ->
      let lexbuf = Lexing.from_channel inBuffer in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = opt.infile };
      try
				let prog = getProg opt.optim lexbuf in
				(*let prog = Parser.prog Lexer.lex lexbuf in
        if opt.optim then 
           let prog = com_opt prog in
				*)
				
				let m_prog = m_compile_com prog in
				p_stderr ("MIPS Out : " ^ nl ^ string_of_m_prog m_prog ^ nl); 
        
        
        let oc = open_out opt.outfile in
        p_oc oc (string_of_m_prog m_prog);
        close_out oc;
        
        if opt.d_ast then
          p_stderr ("Raw AST : \n" ^ of_com prog ^ nl);
        if opt.d_past then
          p_stderr ("Pretty AST: \n" ^ pretty_of_com 0 prog ^ nl);
             
        let code = compile_program prog in 
        if opt.d_code then 
          p_stderr ("Raw Code : \n" ^ of_code false code ^ nl);
        if opt.d_pcode then 
          p_stderr ("Pretty Code : \n" ^ of_code true code ^ nl);
          
        (* imp_ex execution *)
        let c0 = of_int 0 in
        let st0 = const c0 in (* assume all variables 0 *)
        if opt.imp_ex > 0 then (
          try
            p_stdout ("Execute : imp_ex " ^ string_of_int opt.imp_ex ^ " steps");
            let st_end = ceval_ex st0 prog (Listaux__TakeDrop.of_int (of_int opt.imp_ex)) in
            p_stdout ("Memory : " ^ nl ^ string_of_map st_end);
          with
          | _ -> p_stdout "ceval : Exited with an error\n";
        );
        (* vm_ex execution *)
        if opt.vm_ex > 0 then (
          try
            p_stdout ("Execute : mult_ex " ^ string_of_int opt.vm_ex ^ " steps");
            let (p, s, st_end, n) = Vm_ex__VmEx.vm_ex_mult code c0 [] st0 (Listaux__TakeDrop.of_int (of_int opt.vm_ex)) in
            p_stdout ("Reached Ihalt in " ^  string_of_int (opt.vm_ex - (to_int (Listaux__TakeDrop.to_int n))) ^ " steps" ^ nl);
            p_stdout ("Memory : " ^ nl ^ string_of_map st_end);
          with 
          | VmNotEnd -> p_stdout "mult_ex : Did not reach end\n";
          | _ -> p_stdout "mult_ex : Exited with an error\n";
        ); 
        
        ()
      with
      | Lexer.SyntaxError msg -> raise (CompilerError ("Syntax error. " ^ msg ^ parse_err_msg lexbuf)); 
      | Parser.Error -> raise (CompilerError ("Parser error." ^ parse_err_msg lexbuf));
  with
  | CompilerError msg -> p_stderr msg;
  | Failure msg -> p_stderr ("Failure (internal error): " ^ msg);
  exit (-1);;

main ();;
