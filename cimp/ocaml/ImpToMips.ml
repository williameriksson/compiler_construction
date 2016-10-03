open Common
open Why3extract.Why3__BigInt (* of_int, to_string, etc. *)
open Vm__Vm
open Imp__Imp
open State__StateEx
open State__StateGen
open List

type m_reg =
	| ZE | T0 | T1 | T2 | SP | GP

type m_instr =
	| Mlab of string
	| Madd of m_reg * m_reg * m_reg
	| Maddi of m_reg * m_reg * int
	| Mand of m_reg * m_reg * m_reg
	| Mbeq of m_reg * m_reg * string
	| Mlui of m_reg * int
	| Mli of m_reg * int
	| Mori of m_reg * m_reg * int
	| Mxori of m_reg * m_reg * int
	| Msub of m_reg * m_reg * m_reg
	| Msw of m_reg * int * m_reg
	| Mlw of m_reg * int * m_reg

type m_instr_list = m_instr list

let reg_to_str (reg : m_reg) : string =
	match reg with
	| ZE -> "$zero"
	| T0 -> "$t0"
	| T1 -> "$t1"
	| T2 -> "$t2"
	| SP -> "$sp"
	| GP -> "$gp"

let rrr rd rs rt = (reg_to_str rd) ^ ", " ^ (reg_to_str rs) ^ ", " ^ (reg_to_str rt)
let rrofs rs rt ofs = (reg_to_str rs) ^ ", " ^ (reg_to_str rt) ^ ", " ^ (string_of_int ofs)
let rrlab rs rt lab = (reg_to_str rs) ^ ", " ^ (reg_to_str rt) ^ ", " ^ lab
let rofsr rt ofs rs = (reg_to_str rt) ^ ", " ^ (string_of_int ofs) ^ "(" ^ (reg_to_str rs) ^ ")"
let rn rt n = (reg_to_str rt) ^ ", " ^ (string_of_int n)

let push reg = [Maddi (SP, SP, -4); Msw(reg, 0, SP)]
let pop reg = [Mlw (reg, 0, SP); Maddi (SP, SP, 4)]
let li n = [Mlui (T0, n / 65536); Mori (T0, T0, n)] (* 2^16 = 65536 *)

(*let labelnr = ref 0*)
let count = ref (0)
let newlabel =
    incr count;
    !count
	
(*let newlabel = 
	labelnr := !labelnr + 1;
	!labelnr*)
let branch l = [Mbeq (T0, T0, l)]

let instr_to_str (instr : m_instr) : string =
	match instr with
	| Mlab label         -> label ^ ":"
	| Madd (rd, rs, rt)  -> "add  " ^ (rrr rd rs rt)
	| Maddi (rt, rs, n)  -> "addi " ^ (rrofs rt rs n)
	| Mand (rd, rs, rt)  -> "and  " ^ (rrr rd rs rt)
	| Mbeq (rs, rt, lab) -> "beq  " ^ (rrlab rs rt lab)
	| Mlui (rt, n)       -> "lui  " ^ (rn rt n)
	| Mxori (rt, rs, n)  -> "xori " ^ (rrofs rt rs n)
	| Mli (rt, n)        -> "li   " ^ (rn rt n)
	| Mori (rs, rt, n)   -> "ori  " ^ (rrofs rs rt n)
	| Msub (rd, rs , rt) -> "sub  " ^ (rrr rd rs rt)
	| Msw (rt, ofs, rs)  -> "sw   " ^ (rofsr rt ofs rs)
	| Mlw (rt, ofs, rs)  -> "lw   " ^ (rofsr rt ofs rs)

let rec compile_aexpr (exp : aexpr) : m_instr_list =
	match exp with
	| Anum n       -> li (to_int n) @ push T0
	| Avar (Id id) -> Mlw (T0, (to_int id)*4, GP) :: push T0
	| Aadd (a, b)  -> compile_aexpr a @ compile_aexpr b @ pop T0 @ pop T1 @ Madd (T0, T0, T1) :: push T0
	| Asub (a, b)  -> compile_aexpr a @ compile_aexpr b @ pop T0 @ pop T1 @ Msub (T0, T0, T1) :: push T0
	| Amul (a, b)  -> [] (* TODO *)

let rec compile_bexpr (exp : bexpr) (label : string) =
	match exp with
	| Btrue -> []
	| Bfalse -> branch label
	| Bnot a -> if (a = Btrue) then (compile_bexpr Bfalse label) else (compile_bexpr Btrue label) 
	| Band (a, b) -> if (a = Bfalse || b = Bfalse) then (branch label) else []
	| Beq (a, b) -> [] (* TODO *)
	| Ble (a, b) -> [] (* TODO *)

let rec string_of_m_prog = function
	| Mlab label :: ilist -> instr_to_str (Mlab label) ^ nl ^ string_of_m_prog ilist
	| instr :: ilist -> "\t" ^ instr_to_str instr ^ nl ^ string_of_m_prog ilist
	| [] -> ""

let rec m_compile_com (cmd : com) =
	match cmd with
	| Cskip                  -> []
	| Cassign (Id id, n)     -> compile_aexpr n @ pop T0 @ [Msw (T0, (to_int id)*4, GP)]
	| Cif (cond, tcmd, fcmd) -> 
		let labelnr = newlabel in
		let _else = "else" ^ string_of_int labelnr in 
		let _endif = "endif" ^ string_of_int labelnr in
		let true_code = m_compile_com tcmd @ branch _endif in
		let false_code = m_compile_com fcmd in
		 compile_bexpr cond _else @ true_code @ [Mlab _else] @ false_code @ [Mlab _endif]
	| Cseq (cmd1, cmd2) -> [] (* TODO *)
	| Cwhile (cond, cmd) -> [] (* TODO *)

