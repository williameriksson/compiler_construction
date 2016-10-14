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

let k = ref (10)

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

let push reg = [Maddi (SP, SP, -4); Msw(reg, 0, SP)]
let pop reg = [Mlw (reg, 0, SP); Maddi (SP, SP, 4)]

 
(*let li n = [Mlui (T0, n / 65536); Mori (T0, T0, n)] (* 2^16 = 65536 *) *)

let count = ref (0)
let newlabel () =
    incr count;
    !count


let alloc_regs = ref (-1)
let get_reg = incr alloc_regs; !alloc_regs

let branch l = [Mbeq (AT, AT, l)]

let instr_to_str (instr : m_instr) : string =
	match instr with
	| Mlab label          -> label ^ ":"
	| Madd (rd, rs, rt)   -> "add  " ^ (rrr rd rs rt)
	| Maddi (rt, rs, n)   -> "addi " ^ (rrofs rt rs n)
	| Mand (rd, rs, rt)   -> "and  " ^ (rrr rd rs rt)
	| Mbeq (rs, rt, lab)  -> "beq  " ^ (rrlab rs rt lab)
	| Mbne (rs, rt, lab)  -> "bne  " ^ (rrlab rs rt lab)
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
    | Anum n       -> [Mli (T reg, to_int n)]
    | Avar (Id id) -> [Mlw (T reg, (to_int id)*4, GP)]
    | Aadd (a, b)  -> compile_aexpr a reg @ compile_aexpr b (reg + 1) @ [Madd (T reg,T reg,T (reg + 1))]
    | Asub (a, b)  -> compile_aexpr a reg @ compile_aexpr b (reg + 1) @ [Msub (T reg, T reg, T (reg + 1))]
    | Amul (a, b)  -> raise (CompilerError "multiplication currently not supported") (* TODO *)
(*
let rec compile_aexpr (exp : aexpr) : m_instr_list =
	match exp with
	| Anum n       -> Mli (T0, to_int n) :: push T0
	| Avar (Id id) -> Mlw (T0, (to_int id)*4, GP) :: push T0
	| Aadd (a, b)  -> compile_aexpr a @ compile_aexpr b @ pop T0 @ pop T1 @ Madd (T0, T0, T1) :: push T0
	| Asub (a, b)  -> compile_aexpr a @ compile_aexpr b @ pop T0 @ pop T1 @ Msub (T0, T1, T0) :: push T0
	| Amul (a, b)  -> raise (CompilerError "multiplication currently not supported") (* TODO *)
*)

let rec compile_bexpr (exp : bexpr) (reg : int) (label : string) (not_op : bool) =
    match exp with
    | Btrue -> if not_op then branch label else [] 
    | Bfalse -> if not_op then [] else branch label 
    | Bnot b -> compile_bexpr b reg label (not not_op)
    (*| Bnot a -> if (a = Btrue) then (compile_bexpr Bfalse label) else (compile_bexpr Btrue label) *)
    (*| Band (b1, b2) -> if (b1 = Bfalse || b2 = Bfalse) then (branch label) else [] *)
    | Band (b1, b2) -> 
        let labelnr = newlabel () in
        let _and = "and" ^ string_of_int labelnr in
        let _dest = if not_op then _and else label in 
        let cond1 = compile_bexpr b1 reg _dest false in
        let cond2 = compile_bexpr b2 (reg + 1) label not_op in
        cond1 @ [Mlab _and] @ cond2
    | Beq (a, b) -> 
        let expr1 = compile_aexpr a reg in
        let expr2 = compile_aexpr b (reg + 1) in
        expr1 @ expr2 @ if not_op then
              [Mbeq (T reg, T (reg + 1), label)] 
            else 
                [Mbne (T reg, T (reg + 1), label)]
    | Ble (a, b) -> 
        let expr1 = compile_aexpr a reg in
        let expr2 = compile_aexpr b (reg + 1) in
          expr1 @ expr2 @ 
					if not_op then 
            [Mslt (AT, T reg, T (reg + 1)); Mbeq (AT, ZE, label)]
          else
            [Mslt (AT, T reg, T (reg + 1)); Mbne (AT, ZE, label)]


(*
let rec compile_bexpr (exp : bexpr) (label : string) (not_op : bool) =
	match exp with
	| Btrue -> if not_op then branch label else [] 
	| Bfalse -> if not_op then [] else branch label 
	| Bnot b -> compile_bexpr b label (not not_op)
	(*| Bnot a -> if (a = Btrue) then (compile_bexpr Bfalse label) else (compile_bexpr Btrue label) *)
	(*| Band (b1, b2) -> if (b1 = Bfalse || b2 = Bfalse) then (branch label) else [] *)
	| Band (b1, b2) -> 
		let labelnr = newlabel () in
		let _and = "and" ^ string_of_int labelnr in
		let _dest = if not_op then _and else label in 
		let cond1 = compile_bexpr b1 _dest false in
		let cond2 = compile_bexpr b2 label not_op in
		cond1 @ [Mlab _and] @ cond2
	| Beq (a, b) -> 
		let expr1 = compile_aexpr a in
		let expr2 = compile_aexpr b in
		expr1 @ expr2 @ pop T0 @ pop T1 @ if not_op then
			  [Mbeq (T0, T1, label)] 
			else 
				[Mbne (T0, T1, label)]
	| Ble (a, b) -> 
		let expr1 = compile_aexpr a in
    let expr2 = compile_aexpr b in
    expr1 @ expr2 @ pop T0 @ pop T1 @ if not_op then 
			  [Mslt (AT, T0, T1); Mbeq (AT, ZE, label)]
			else
        [Mslt (AT, T0, T1); Mbne (AT, ZE, label)] *)
				
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
	in pre_str ^ prog_print(p @ Mlab "halt" :: [Mbeq (AT, AT, "halt")])


let rec m_compile_com (cmd : com) =
    match cmd with
    | Cskip                  -> []
    | Cassign (Id id, n)     -> let reg = get_reg in compile_aexpr n reg @ [Msw (T reg, (to_int id)*4, GP)]
    | Cif (cond, tcmd, fcmd) -> 
        let labelnr = newlabel () in
        let _else = "else" ^ string_of_int labelnr in 
        let _endif = "endif" ^ string_of_int labelnr in
        let true_code = m_compile_com tcmd @ branch _endif in
        let false_code = m_compile_com fcmd in
         compile_bexpr cond get_reg _else false @ true_code @ [Mlab _else] @ false_code @ [Mlab _endif]
    | Cseq (cmd1, cmd2) -> m_compile_com cmd1 @ m_compile_com cmd2
    | Cwhile (cond, cmd) -> 
        let labelnr = newlabel () in
        let _while = "while" ^ string_of_int labelnr in 
        let _endwhile = "endwhile" ^ string_of_int labelnr in
        let code = m_compile_com cmd in
				let reg = get_reg in
        let w_cond = compile_bexpr cond reg _endwhile false in
        [Mlab _while] @ w_cond @ code @ [Mbeq(T reg, T reg, _while)] @ [Mlab _endwhile]
(*
let rec m_compile_com (cmd : com) =
	match cmd with
	| Cskip                  -> []
	| Cassign (Id id, n)     -> compile_aexpr n @ pop T0 @ [Msw (T0, (to_int id)*4, GP)]
	| Cif (cond, tcmd, fcmd) -> 
		let labelnr = newlabel () in
		let _else = "else" ^ string_of_int labelnr in 
		let _endif = "endif" ^ string_of_int labelnr in
		let true_code = m_compile_com tcmd @ branch _endif in
		let false_code = m_compile_com fcmd in
		 compile_bexpr cond _else false @ true_code @ [Mlab _else] @ false_code @ [Mlab _endif]
	| Cseq (cmd1, cmd2) -> m_compile_com cmd1 @ m_compile_com cmd2
	| Cwhile (cond, cmd) -> 
		let labelnr = newlabel () in
    let _while = "while" ^ string_of_int labelnr in 
    let _endwhile = "endwhile" ^ string_of_int labelnr in
		let code = m_compile_com cmd in
		let w_cond = compile_bexpr cond _endwhile false in
		[Mlab _while] @ w_cond @ code @ [Mbeq(T0, T0, _while)] @ [Mlab _endwhile]
*)
