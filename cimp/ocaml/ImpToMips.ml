open Common
open Why3extract.Why3__BigInt (* of_int, to_string, etc. *)
open Vm__Vm
open Imp__Imp
open State__StateEx
open State__StateGen
open List

type m_reg =
	| ZE | T0 | T1 | T2

type m_instr =
	| Mlab of string
	| Madd of m_reg * m_reg * m_reg
	| Mand of m_reg * m_reg * m_reg
	| Mbeq of m_reg * m_reg * int
	| Mlui of m_reg * int
	| Mli of m_reg * int
	| Mxori of m_reg * m_reg * int

type m_instr_list = m_instr list

let reg_to_str (reg : m_reg) : string =
	match reg with
	| ZE -> "$zero"
	| T0 -> "$t0"
	| T1 -> "$t1"
	| T2 -> "$t2"

let rrr rd rs rt = (reg_to_str rd) ^ "," ^ (reg_to_str rs) ^ "," ^ (reg_to_str rt)
let rrofs rs rt ofs = (reg_to_str rs) ^ "," ^ (reg_to_str rt) ^ "," ^ (string_of_int ofs)
let rn rt n = (reg_to_str rt) ^ "," ^ (string_of_int n)

let li n = [Mlui (T0, n); Mxori (T0, T0, 65536)] (* 2^16 = 65536 *) 

let instr_to_str (instr : m_instr) : string =
	match instr with
	| Mlab label         -> label ^ ":"
	| Madd (rd, rs, rt)  -> "add" ^ (rrr rd rs rt)
	| Mand (rd, rs, rt)  -> "and" ^ (rrr rd rs rt)
	| Mbeq (rs, rt, ofs) -> "beq" ^ (rrofs rs rt ofs)
	| Mlui (rt, n)       -> "lui" ^ (rn rt n)
	| Mxori (rt, rs, n)  -> "xori" ^ (rrofs rt rs n)
	| Mli (rt, n)        -> "li" ^ (rn rt n)

let rec compile_aexpr (exp : aexpr) : m_instr_list =
	match exp with
	| Anum n       -> li (to_int n) 
	| Aadd (a, b)  -> compile_aexpr a @ compile_aexpr b

let compile_bexpr (exp : bexpr) : bool =
	match exp with
	| Btrue -> true
	| Bfalse -> false

let rec string_of_m_prog = function
	| i :: il -> "\t" ^ instr_to_str i ^ nl ^ string_of_m_prog il
	| [] -> ""

let rec m_compile_com (cmd : com) =
	match cmd with
	| Cskip                  -> []
	| Cassign (Id id, n)     -> compile_aexpr n
	| Cif (cond, tcmd, fcmd) -> if ((compile_bexpr cond) = true) then m_compile_com tcmd else m_compile_com fcmd

