open Common
open Why3extract.Why3__BigInt
open Vm__Vm
open Imp__Imp
open State__StateEx
open State__StateGen
open List

type m_reg =
	| ZE | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 | SP | GP | AT

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

type m_instr_list = m_instr list
type reg_list = m_reg list


let reg_to_str (reg : m_reg) : string =
	match reg with
	| ZE -> "$zero"
	| T0 -> "$t0"
	| T1 -> "$t1"
	| T2 -> "$t2"
	| T3 -> "$t3"
	| T4 -> "$t4"
	| T5 -> "$t5"
	| T6 -> "$t6"
	| T7 -> "$t7"
	| SP -> "$sp"
	| GP -> "$gp"
	| AT -> "$at"

let rrr rd rs rt = (reg_to_str rd) ^ ", " ^ (reg_to_str rs) ^ ", " ^ (reg_to_str rt)
let rrofs rs rt ofs = (reg_to_str rs) ^ ", " ^ (reg_to_str rt) ^ ", " ^ (string_of_int ofs)
let rrlab rs rt lab = (reg_to_str rs) ^ ", " ^ (reg_to_str rt) ^ ", " ^ lab
let rofsr rt ofs rs = (reg_to_str rt) ^ ", " ^ (string_of_int ofs) ^ "(" ^ (reg_to_str rs) ^ ")"
let rn rt n = (reg_to_str rt) ^ ", " ^ (string_of_int n)

let push reg = [Maddi (SP, SP, -4); Msw(reg, 0, SP)]
let pop reg = [Mlw (reg, 0, SP); Maddi (SP, SP, 4)]

type var_reg_tuple = int * m_reg
type tuple_list = var_reg_tuple list

let first = function (x, y) -> x;;
let second = function (x, y) -> y;;

let available_regs = ref [T0; T1; T2; T3; T4; T5; T6; T7]
let alloc_regs : tuple_list ref = ref []


let storeVar _id n =
  if List.length !available_regs >= 1 then
		let reg =	List.hd !available_regs in        (* Take the first available register *)
		let new_tuple = (_id, reg) in               (* Make a new tuple with the id and the register *)
		alloc_regs := new_tuple :: !alloc_regs;     (* Add the new  tuple to the list with allocated registers *)
    available_regs := List.tl !available_regs;  (* Remove the register from the list of available registers *)
		[Mli (reg, n)]                              (* Return the instruction *)                                        
	else
		let tuple = List.hd !alloc_regs in          (* Take the first tuple from the allocated registers list *)
		let oldId = first tuple in                  (* Take the id from the tuple which shall be stored to memory *)
		let reg = second tuple in                   (* Take the register from the tuple which holds the value that shall be stored to memory *)
		alloc_regs := List.tl !alloc_regs;          (* Remove the first tuple from the list *)
		alloc_regs := (_id, reg) :: !alloc_regs;    (* Add the new tuple to the list *)
		[Msw (reg, oldId*4, GP)] @ [Mli (reg, n)]   (* Save the old value to memory and load the new value to the register *)
		

let loadValue _id =
	if List.exists (fun (a_id, _) -> a_id = _id) !alloc_regs then
		let tuple = List.find (fun (a_id, _) -> a_id = _id) !alloc_regs in
		let reg = second tuple in
		reg
	else
		[]

 

(*let li n = [Mlui (T0, n / 65536); Mori (T0, T0, n)] (* 2^16 = 65536 *) *)

(*

let reg_stack : reg_list ref = ref []



let removeReg (l : reg_list) : reg_list =
	match l with
	(*| l -> List.tl l *)
  | [] -> []
  | hd :: l -> l

let restoreReg r = available_regs := r :: !available_regs

let retrieveReg () =
  let reg = getReg !available_regs in
    available_regs := removeReg !available_regs;
		reg_stack := reg :: !reg_stack;
		reg

let pop_reg () =
	let reg = getReg !reg_stack in
	  reg_stack := removeReg !reg_stack;
		restoreReg reg;
		reg
*)
let count = ref (0)
let newlabel () =
    incr count;
    !count

let branch l = [Mbeq (T0, T0, l)]

let instr_to_str (instr : m_instr) : string =
	match instr with
	| Mlab label         -> label ^ ":"
	| Madd (rd, rs, rt)  -> "add  " ^ (rrr rd rs rt)
	| Maddi (rt, rs, n)  -> "addi " ^ (rrofs rt rs n)
	| Mand (rd, rs, rt)  -> "and  " ^ (rrr rd rs rt)
	| Mbeq (rs, rt, lab) -> "beq  " ^ (rrlab rs rt lab)
	| Mbne (rs, rt, lab) -> "bne  " ^ (rrlab rs rt lab)
	| Mlui (rt, n)       -> "lui  " ^ (rn rt n)
	| Mxori (rt, rs, n)  -> "xori " ^ (rrofs rt rs n)
	| Mli (rt, n)        -> "li   " ^ (rn rt n)
	| Mori (rs, rt, n)   -> "ori  " ^ (rrofs rs rt n)
	| Mslt (rd, rs, rt)  -> "slt  " ^ (rrr rd rs rt)
	| Msub (rd, rs , rt) -> "sub  " ^ (rrr rd rs rt)
	| Msw (rt, ofs, rs)  -> "sw   " ^ (rofsr rt ofs rs)
	| Mlw (rt, ofs, rs)  -> "lw   " ^ (rofsr rt ofs rs)

(*let rec compile_aexpr (exp : aexpr) : m_instr_list =
    match exp with
    | Anum n       -> [Mli (retrieveReg (), to_int n)]
    | Avar (Id id) -> [Mlw (retrieveReg (), (to_int id)*4, GP)]
    | Aadd (a, b)  -> compile_aexpr a @ compile_aexpr b @ let dest_reg = pop_reg () in Madd (dest_reg, dest_reg, pop_reg ()) :: push dest_reg
    | Asub (a, b)  -> compile_aexpr a @ compile_aexpr b @ pop T0 @ pop T1 @ Msub (T0, T1, T0) :: push T0
    | Amul (a, b)  -> raise (CompilerError "multiplication currently not supported") (* TODO *) *)

let rec compile_aexpr (exp : aexpr) : m_instr_list =
	match exp with
	| Anum n       -> Mli (T0, to_int n) :: push T0
	| Avar (Id id) -> Mlw (T0, (to_int id)*4, GP) :: push T0
	| Aadd (a, b)  -> compile_aexpr a @ compile_aexpr b @ pop T0 @ pop T1 @ Madd (T0, T0, T1) :: push T0
	| Asub (a, b)  -> compile_aexpr a @ compile_aexpr b @ pop T0 @ pop T1 @ Msub (T0, T1, T0) :: push T0
	| Amul (a, b)  -> raise (CompilerError "multiplication currently not supported") (* TODO *)

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
        [Mslt (AT, T0, T1); Mbne (AT, ZE, label)]
				
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
	in pre_str ^ prog_print(p @ Mlab "halt" :: [Mbeq (T0, T0, "halt")])

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

