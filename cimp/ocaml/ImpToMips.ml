open Common
open Why3extract.Why3__BigInt
open Vm__Vm
open Imp__Imp
open State__StateEx
open State__StateGen
open List

type m_reg =
  | ZE | T of int | S0 | S1 | SP | GP | AT

type m_instr =
  | Mlab of string
  | Madd of m_reg * m_reg * m_reg
  | Maddi of m_reg * m_reg * int
  | Mand of m_reg * m_reg * m_reg
  | Mbeq of m_reg * m_reg * string
  | Mbne of m_reg * m_reg * string
  | Mblez of m_reg * string
  | Mbltz of m_reg * string
  | Mlui of m_reg * int
  | Mli of m_reg * int
  | Mori of m_reg * m_reg * int
  | Mxori of m_reg * m_reg * int
  | Mslt of  m_reg * m_reg * m_reg
  | Msub of m_reg * m_reg * m_reg
  | Msw of m_reg * int * m_reg
  | Mlw of m_reg * int * m_reg
  | Mmove of m_reg * m_reg

type m_instr_list = m_instr list
type reg_list = m_reg list

let nr_regs = ref (10)

let reg_to_str (reg : m_reg) : string =
  match reg with
  | ZE -> "$zero"
  | T n -> "$t" ^ string_of_int n
  | S0 -> "$s0"
  | S1 -> "$s1"
  | SP -> "$sp"
  | GP -> "$gp"
  | AT -> "$at"

let rrr rd rs rt = (reg_to_str rd) ^ ", " ^ (reg_to_str rs) ^ ", " ^ (reg_to_str rt)
let rrofs rs rt ofs = (reg_to_str rs) ^ ", " ^ (reg_to_str rt) ^ ", " ^ (string_of_int ofs)
let rrlab rs rt lab = (reg_to_str rs) ^ ", " ^ (reg_to_str rt) ^ ", " ^ lab
let rofsr rt ofs rs = (reg_to_str rt) ^ ", " ^ (string_of_int ofs) ^ "(" ^ (reg_to_str rs) ^ ")"
let rn rt n = (reg_to_str rt) ^ ", " ^ (string_of_int n)
let rr rs rt = (reg_to_str rs) ^ ", " ^ (reg_to_str rt)
let rl rs lab = (reg_to_str rs) ^ ", " ^ lab

let push reg = [Maddi (SP, SP, -4); Msw(reg, 0, SP)]
let pop reg = [Mlw (reg, 0, SP); Maddi (SP, SP, 4)]

 
(*let li n = [Mlui (T0, n / 65536); Mori (T0, T0, n)] (* 2^16 = 65536 *) *)

let first = function (x, y) -> x
let second = function (x, y) -> y
let cache_list : (int * int) list ref = ref [] (* [ (id1,reg1); (id2, reg2) ]  *)

let rec print_list = function 
  | [] -> ()
  | e::l -> print_string "("; print_int (first e) ; print_string "," ; print_int (second e) ; print_string ")"; print_list l

(*
let rec check_cached _id lst = 
    match lst with
    | [] -> false
    | ((a_id, a_reg) :: l) when a_id = _id -> true
    | (a :: l) -> check_cached _id l *)

let rec replace_var _id reg lst =
  match lst with
  | [] -> (_id, reg) :: []
  | ((a_id, a_reg) :: l) when a_id = _id -> (_id, reg) :: l
  | (a :: l) -> a :: (replace_var _id reg l)

let cache_var _id reg =
  cache_list := replace_var _id reg !cache_list

(* replace with option type *)
let rec find_var _id lst =
  match lst with
  | [] -> 1337
  | ((a_id, a_reg) :: l) when a_id = _id -> a_reg
  | (a :: l) -> (find_var _id l)

let get_cached _id =
  find_var _id !cache_list

let is_cached _id =
  List.exists (fun (a_id, _) -> a_id = _id) !cache_list
	

let rec write_back_cache lst =
	match lst with
	| [] -> [] 
  | (id, reg) :: l -> [Msw (T reg, id * 4 , GP)] @ write_back_cache l


let count = ref (0)
let newlabel () =
  incr count;
  !count

let alloc_regs = ref (-1)
let get_reg () = 
  if !alloc_regs < !nr_regs - 1 then
    alloc_regs := !alloc_regs + 1;
  !alloc_regs
    
let branch l = [Mbeq (AT, AT, l)]

let instr_to_str (instr : m_instr) : string =
  match instr with
  | Mlab label          -> label ^ ":"
  | Madd (rd, rs, rt)   -> "add  " ^ (rrr rd rs rt)
  | Maddi (rt, rs, n)   -> "addi " ^ (rrofs rt rs n)
  | Mand (rd, rs, rt)   -> "and  " ^ (rrr rd rs rt)
  | Mbeq (rs, rt, lab)  -> "beq  " ^ (rrlab rs rt lab)
  | Mbne (rs, rt, lab)  -> "bne  " ^ (rrlab rs rt lab)
  | Mblez (rs, lab)     -> "blez " ^ (rl rs lab)
  | Mbltz (rs, lab)     -> "bltz " ^ (rl rs lab)
  | Mlui (rt, n)        -> "lui  " ^ (rn rt n)
  | Mxori (rt, rs, n)   -> "xori " ^ (rrofs rt rs n)
  | Mli (rt, n)         -> "li   " ^ (rn rt n)
  | Mori (rs, rt, n)    -> "ori  " ^ (rrofs rs rt n)
  | Mslt (rd, rs, rt)   -> "slt  " ^ (rrr rd rs rt)
  | Msub (rd, rs , rt)  -> "sub  " ^ (rrr rd rs rt)
  | Msw (rt, ofs, rs)   -> "sw   " ^ (rofsr rt ofs rs)
  | Mlw (rt, ofs, rs)   -> "lw   " ^ (rofsr rt ofs rs)
  | Mmove (rt, rs)      -> "move " ^ (rr rt rs)



let rec compile_aexpr (exp : aexpr) (reg : int) : m_instr_list =
  match exp with
  | Anum n                          -> [Mli (T reg, to_int n)]
  | Avar (Id id)                    ->  if is_cached (to_int id) then 
                                          [Mmove (T reg, T (get_cached (to_int id)))]
																				else
																					[Mlw (T reg, (to_int id)*4, GP)]
																					
  | Aadd (Avar (Id x), Avar (Id y)) -> let reg_x = get_cached (to_int x) in
                                       let reg_y = get_cached (to_int y) in
                                         if (is_cached (to_int x) && is_cached (to_int y)) then
                                           [Madd (T reg, T reg_x, T reg_y)]
                                         else if is_cached (to_int x) then
                                           [Madd (T reg, T reg_x, T reg)] @ compile_aexpr (Anum y) reg
                                         else if is_cached (to_int y) then
                                           [Madd (T reg, T reg_y, T reg)] @ compile_aexpr (Anum x) reg
                                         else
                                           [Madd (T reg, T reg, T (reg + 1))] @ compile_aexpr (Anum y) (reg + 1) @ compile_aexpr (Anum x) reg
																						(*above is wrong? should be Avar y and Avar x???*)
  | Aadd (Avar (Id x), Anum n)      -> let reg_x = get_cached (to_int x) in
                                         if is_cached (to_int x) then
                                           [Maddi (T reg, T reg_x, to_int n)]
                                         else
                                           [Maddi (T reg, T reg, to_int n)] @ compile_aexpr (Avar (Id x)) reg
																				
  | Aadd ((Anum n), Avar (Id x))    -> let reg_x = get_cached (to_int x) in
                                         if is_cached (to_int x) then
                                           [Maddi (T reg, T reg_x, to_int n)]
                                         else
                                           [Maddi (T reg, T reg, to_int n)] @ compile_aexpr (Avar (Id x)) reg
                                   
  | Aadd ((Anum n1), (Anum n2))     -> [Madd (T reg, T reg, T (reg + 1))] @ [Mli (T (reg + 1), to_int n2)] @ [Mli (T reg, to_int n1)]
  
	| Aadd (a, b)                     -> [Madd (T reg, T reg, T (reg + 1))] @ compile_aexpr b (reg + 1) @ compile_aexpr a reg

  | Asub (Avar (Id x), Avar (Id y)) -> let reg_x = get_cached (to_int x) in
                                       let reg_y = get_cached (to_int y) in
                                         if (is_cached (to_int x) && is_cached (to_int y)) then
                                           [Msub (T reg, T reg_x, T reg_y)]
                                         else if is_cached (to_int x) then
                                           [Msub (T reg, T reg_x, T reg)] @ compile_aexpr (Anum y) reg
                                         else if is_cached (to_int y) then
                                           [Msub (T reg, T reg, T reg_y )] @ compile_aexpr (Anum x) reg
                                         else
                                           [Msub (T reg, T reg, T (reg + 1))] @ compile_aexpr (Anum y) (reg + 1) @ compile_aexpr (Anum x) reg
    (*| Asub (Avar (Id x), Avar (Id y)) -> let reg_x = get_cached (to_int x) in
                                         let reg_y = get_cached (to_int y) in
                                           [Msub (T reg, T reg_x, T reg_y)]*)
  | Asub (Avar (Id x), Anum n)      -> let reg_x = get_cached (to_int x) in
                                         if is_cached (to_int x) then
                                           [Maddi (T reg, T reg_x, -(to_int n))]
                                         else
                                           [Maddi (T reg, T reg, -(to_int n))] @ compile_aexpr (Avar (Id x)) reg
    (*| Asub (Avar (Id x), Anum n) -> let reg_x = get_cached (to_int x) in
                                      [Maddi (T reg, T reg_x, -(to_int n))](*; Mli (T reg, (to_int n))]*)*)
  | Asub ((Anum n), Avar (Id x))    -> let reg_x = get_cached (to_int x) in
                                         if is_cached (to_int x) then
                                           [Msub (T reg, T reg, T reg_x)] @ [Mli (T reg, (to_int n))]
                                         else
                                           [Msub (T reg, T reg, T (reg + 1))] @ compile_aexpr (Avar (Id x)) reg																
    (*| Asub ((Anum n), Avar (Id x)) -> let reg_x = get_cached (to_int x) in
                                        [Maddi (T reg, T reg_x, -(to_int n))](* @ [Mli (T reg, to_int n)]*)*)
																				
  | Asub ((Anum n1), (Anum n2))     -> [Msub (T reg, T reg, T (reg + 1))] @ [Mli (T (reg + 1), to_int n2)] @ [Mli (T reg, to_int n1)]
  | Asub (a, b)                     -> [Msub (T reg, T reg, T (reg + 1))] @ compile_aexpr b (reg + 1) @ compile_aexpr a reg  
  | Amul (a, b)                     -> raise (CompilerError "multiplication currently not supported") (* TODO *)

(*
let rec compile_aexpr (exp : aexpr) (reg : int) : m_instr_list =
    match exp with
    | Anum n       -> [Mli (T reg, to_int n)]
    | Avar (Id id) -> [Mlw (T reg, (to_int id)*4, GP)]
    | Aadd (a, b)  -> compile_aexpr a reg @ compile_aexpr b (reg + 1) @ [Madd (T reg, T reg, T (reg + 1))]
    | Asub (a, b)  -> compile_aexpr a reg @ compile_aexpr b (reg + 1) @ [Msub (T reg, T reg, T (reg + 1))]
    | Amul (a, b)  -> raise (CompilerError "multiplication currently not supported") (* TODO *)
*)

let rec compile_bexpr (exp : bexpr) (reg : int) (label : string) (not_op : bool) =
  match exp with
  | Btrue         -> if not_op then branch label else [] 
  | Bfalse        -> if not_op then [] else branch label 
  | Bnot b        -> compile_bexpr b reg label (not not_op)

  | Band (b1, b2) -> let labelnr = newlabel () in
                     let _and = "and" ^ string_of_int labelnr in
                     let _dest = if not_op then _and else label in 
                     let cond1 = compile_bexpr b1 reg _dest false in
                     let cond2 = compile_bexpr b2 (reg + 1) label not_op in
                       cond2 @ [Mlab _and] @ cond1
											
  | Beq (a, b)    -> let expr1 = compile_aexpr a reg in
                     let expr2 = compile_aexpr b (reg + 1) in
                       expr2 @ expr1 @ 
										   if not_op then
                         [Mbeq (T reg, T (reg + 1), label)] 
                       else 
                         [Mbne (T reg, T (reg + 1), label)]
								
	| Ble (Avar (Id x), Anum n) -> let reg_x = get_cached (to_int x) in
                                   if is_cached (to_int x) then
															       if not_op then 
                                       [Mblez (AT, label); Msub (AT, T reg_x, T reg); Mli (T reg, (to_int n))]
                                     else
                                       [Mbltz (AT, label); Msub (AT, T reg, T reg_x); Mli (T reg, (to_int n))]
                                   else
																     if not_op then 
                                       [Mblez (AT, label); Msub (AT, T reg, T (reg + 1)); Mlw (T reg, (to_int x)*4, GP); Mli (T (reg + 1), (to_int n))]
                                     else
                                       [Mbltz (AT, label); Msub (AT, T (reg + 1), T reg); Mlw (T reg, (to_int x)*4, GP); Mli (T (reg + 1), (to_int n))]
																			
  | Ble (a, b)    -> let expr1 = compile_aexpr a reg in
                     let expr2 = compile_aexpr b (reg + 1) in
                       if not_op then 
                         [Mblez (AT, label); Msub (AT, T reg, T (reg + 1))] @ expr2 @ expr1
                       else
                         [Mbltz (AT, label); Msub (AT, T (reg + 1), T reg)] @ expr2 @ expr1

let pre_str = "\t.data" ^ nl ^ 
  "\t.space 1024\t\t# just a placholder" ^ nl ^ 
  "\t.set noreorder\t\t# necessary to avoid code optimization" ^ nl ^
  "\t.set noat\t\t# necessary to avoid warning accessing $at" ^ nl ^
  "\t.text\t\t\t# .text segment (code)" ^ nl ^
  "\tla $gp, .data\t\t# set gp to accesible data area" ^ nl ^ 
    "\t\t\t\t# sp initialized to 0 x80000000 on reset" ^ nl ^ nl

let string_of_m_prog p =
  let rec prog_print = function
    | Mlab label :: ilist -> instr_to_str (Mlab label) ^ nl ^ prog_print ilist
    | instr :: ilist -> "\t" ^ instr_to_str instr ^ nl ^ prog_print ilist
    | [] -> ""
  in pre_str ^ prog_print(List.rev p @ Mlab "halt" :: [Mbeq (AT, AT, "halt")])

(* TODO: match all cases for Cassign, Aadd, Asub and so on to be able to get cached vars, that is, for e.g. Cassign (Id id, Anum n) Cassign (Id id, Avar x)*)
let cache_helper _id reg n=
	cache_var _id reg;
	compile_aexpr n reg

	
let rec m_compile_com (cmd : com) =
  match cmd with
  | Cskip                  -> []
        (*| Cassign (Id id, Anum n) ->*) 
  | Cassign (Id id, n)     -> let reg = get_reg () in 
		                          let cached = is_cached (to_int id) in
	  													let cached_reg = get_cached (to_int id) in
                                if cached = false then
                                  cache_helper (to_int id) reg n
                                else
                                  compile_aexpr n cached_reg (*@ [Msw (T reg, (to_int id)*4, GP)]*)
																	
  | Cif (cond, tcmd, fcmd) -> let labelnr = newlabel () in
                              let _else = "else" ^ string_of_int labelnr in 
                              let _endif = "endif" ^ string_of_int labelnr in
                              let true_code = m_compile_com tcmd @ branch _endif in
                              let false_code = m_compile_com fcmd in
                              let reg = get_reg () in
                                [Mlab _endif] @ false_code @ [Mlab _else] @ true_code @ compile_bexpr cond reg _else false
																	
  | Cseq (cmd1, cmd2)      -> m_compile_com cmd2 @ m_compile_com cmd1

  | Cwhile (cond, cmd)     -> let labelnr = newlabel () in
                              let _while = "while" ^ string_of_int labelnr in 
                              let _endwhile = "endwhile" ^ string_of_int labelnr in
                              let code = m_compile_com cmd in
                              let reg = get_reg () in
                              let w_cond = compile_bexpr cond reg _endwhile false in
                                [Mlab _endwhile] @ [Mbeq (AT, AT, _while)] @ code @ w_cond @ [Mlab _while]
																
let compile_mips (cmd : com) =
  let the_prog = m_compile_com cmd in
    write_back_cache !cache_list @ the_prog
