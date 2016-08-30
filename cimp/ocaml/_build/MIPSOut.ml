(* Test of an approach to translate stack machine langauge into MIPS assembly *)

(*
    | Iconst int   (*   push n on stack                               *)
    | Ivar id      (*   push the value of variable                    *)
    | Isetvar id   (*   pop an integer, assign it to variable         *)
    | Ibranch ofs  (*   skip ofs instructions                         *)
    | Iadd         (*   pop two values, push their sum                *)
    | Isub         (*   pop two values, push their difference         *)
    | Imul         (*   pop two values, push their product            *)
    | Ibeq ofs     (*   pop n2, pop n1, skip ofs forward if n1 =  n2  *)
    | Ibne ofs     (*   pop n2, pop n1, skip ofs forward if n1 <> n2  *)
    | Ible ofs     (*   pop n2, pop n1, skip ofs forward if n1 <= n2  *)
    | Ibgt ofs     (*   pop n2, pop n1, skip ofs forward if n1 >  n2  *)
    | Ihalt        (*   end of program                                *)
    
*)

(*
Memory layout
GP -> Global memory, used to store variables
SP -> Stack memory, used for arithmetic stack

MIPS lanaguage assumptions
1. We use assume MIPS pseudo instructions (e.g., li of 32 bit intergers 

IMP language assumptions
1. We assume imp programs to have been verified in why3 with mach.int32, 
   hence execution will not be subject to overflow 
2. We assue imp programs to be free of "*", i.e., we do not generate code for multiplications
   

R is the register file (32 bit)
M is the memory (32 bit)

MIPS data types
int  is 32 bit integers
sint is 16 bit integers
*)

open Common
open Why3extract.Why3__BigInt (* of_int, to_string, etc. *)
open Vm__Vm
open Imp__Imp
open State__StateEx
open State__StateGen
open List

type m_reg =
  | ZE | AT | T0 | T1 | T2 | SP | GP | PC

let m_reg_to_string = function
  | ZE -> "$zero" (* writes do not update content (r0 always zero) *)
  | AT -> "$at"
  | T0 -> "$t0"
  | T1 -> "$t1"
  | T2 -> "$t2"
  | SP -> "$sp"
  | GP -> "$gp"
  | PC -> "$pc"
   
type m_instr =
  | Mcom  of string                             (* comment                                  *)
  | Mlab  of string                             (* label                                    *)
  | Mnop                                        (* Nop                                      *)
  | Mmove of m_reg * m_reg                      (* R[rd <- R[rs]],                          *) 
  | Mlui  of m_reg * int                        (* R[rd <- v:aint << 16],                   *)
  | Mori  of m_reg * m_reg * int                (* R[rd <- R[rs] || v: sint],               *)
  | Msw   of m_reg * int * m_reg                (* M[R[br] + ofs:sint <- R[rs],             *) 
  | Mlw   of m_reg * int * m_reg                (* R[rd <- M[R[br] + ofs:sint],             *) 
  | Maddi of m_reg * m_reg * int                (* R[rd <- R[rs] + v,                       *)
  | Madd  of m_reg * m_reg * m_reg              (* R[rd <- R[rs] + R[rt],                   *)
  | Msub  of m_reg * m_reg * m_reg              (* R[rd <- R[rs] - R[rt],                   *)
  | Mbeq  of m_reg * m_reg * string             (* if R[rs]=R[rt] then R[PC] <- "label"     *)
  | Mbne  of m_reg * m_reg * string             (* if R[rs]<>R[rt] then R[PC] <- "label"    *)
  | Mslt  of m_reg * m_reg * m_reg              (* R[rd] = R[rs] < R[rt] = 1 : 0            *) 
(*
  | Mble  of m_reg * m_reg * int                (* if R[rs]<=R[rt] then R[PC <- R[rs] + ofs *)
  | Mbgt  of m_reg * m_reg * int                (* if R[rs]>=R[rt] then R[PC <- R[rs] + ofs *)
*)

let m_r rd rs rt  = m_reg_to_string rd ^ ", " ^ m_reg_to_string rs ^ ", " ^ m_reg_to_string rt 
let m_i rd rs v   = m_reg_to_string rd ^ ", " ^ m_reg_to_string rs ^ ", " ^ string_of_int v
let m_b rd rs l   = m_reg_to_string rd ^ ", " ^ m_reg_to_string rs ^ ", " ^ l
let m_o rd ofs rb = m_reg_to_string rd ^ ", " ^ string_of_int ofs ^ "(" ^ m_reg_to_string rb ^ ")" 
let m_iv rd v     = m_reg_to_string rd ^ ", " ^ string_of_int v  
       
let m_instr_to_string = function
  | Mcom  s             -> "# " ^ s
  | Mlab  l             -> l ^ ":"
  | Mnop                -> "nop  "
  | Mmove (rd, rs)      -> "move " ^ m_reg_to_string rd ^ ", " ^ m_reg_to_string rd 
  | Mlui  (rd, v)       -> "lui  " ^ m_iv rd  v
  | Mori  (rd, rs, v)   -> "ori  " ^ m_i rd rs v
  | Msw   (rs, ofs, rb) -> "sw   " ^ m_o rs ofs rb 
  | Mlw   (rd, ofs, rb) -> "lw   " ^ m_o rd ofs rb   
  | Maddi (rd, rs, v)   -> "addi " ^ m_i rd rs v  
  | Madd  (rd, rs, rt)  -> "add  " ^ m_r rd rs rt
  | Msub  (rd, rs, rt)  -> "sub  " ^ m_r rd rs rt
  | Mbeq  (rs, rt, l)   -> "beq  " ^ m_b rs rt l
  | Mbne  (rs, rt, l)   -> "bne  " ^ m_b rs rt l
  | Mslt  (rd, rs, rt)  -> "slt  " ^ m_r rd rs rt
(*
  | Mbeq  (rs, rt, ofs) -> "beq  " ^ m_i rs rt ofs
  | Mbne  (rs, rt, ofs) -> "bne  " ^ m_i rs rt ofs
  | Mble  (rs, rt, ofs) -> "ble  " ^ m_i rs rt ofs
  | Mbgt  (rs, rt, ofs) -> "bgt  " ^ m_i rs rt ofs
*)
type m_code = m_instr list

(* Assembly macros *)
let push r  = [Mcom "push "; Maddi (SP, SP, -4); Msw (r, 0, SP)]
let pop r   = [Mcom "pop "; Mlw (r, 0, SP); Maddi (SP, SP, 4)]
let pop2    = pop T0 @ pop T1
let li n    = [Mcom "li "; Mlui (T0, n / 65536); Mori (T0, T0, n mod 65536)]
let ble ofs = [Mslt (AT, T0, T1); Mbeq (AT, ZE, ofs)]
let bgt ofs = [Mslt (AT, T0, T1); Mbne (AT, ZE, ofs)]

  
(*  
let gInil        = [Mnop]                                 (* skip                                          *)
let gIconst n    = li n @ push T0                         (* push n on stack                               *)
let gIvar id     = Mlw (T0, id*4, GP) :: push T0          (* push the value of variable                    *)
let gIsetvar id  = pop T0 @ [Msw (T0, id*4, GP)]          (* pop an integer, assign it to variable         *)
let gIbranch ofs = [Mbeq (T0, T0, ofs)]                   (* skip ofs instructions                         *)
let gIadd        = pop2 @ Madd (T0, T0, T1) :: push T0    (* pop two values, push their sum                *)
let gIsub        = pop2 @ Msub (T0, T0, T1) :: push T0    (* pop two values, push their difference         *)
let gIbeq ofs    = pop2 @ [Mbeq (T0, T1, ofs)]            (* pop n2, pop n1, skip ofs forward if n1 =  n2  *)
let gIbne ofs    = pop2 @ [Mbne (T0, T1, ofs)]            (* pop n2, pop n1, skip ofs forward if n1 <> n2  *)
let gIble ofs    = pop2 @ ble ofs                         (* pop n2, pop n1, skip ofs forward if n1 <= n2  *)
let gIbgt ofs    = pop2 @ bgt ofs                         (* pop n2, pop n1, skip ofs forward if n1 >  n2  *)
let gIhalt       = ["halt", gIbranch "halt"]              (* end of program (endless loop)                 *)
*)

let gInil        = [Mnop]                                 (* skip                                          *)
let gIconst n    = li n @ push T0                         (* push n on stack                               *)
let gIvar id     = Mlw (T0, id*4, GP) :: push T0          (* push the value of variable                    *)
let gIsetvar id  = pop T0 @ [Msw (T0, id*4, GP)]          (* pop an integer, assign it to variable         *)
let gIbranch l   = [Mbeq (T0, T0, l)]                     (* skip ofs instructions                         *)
let gIadd        = pop2 @ (Madd (T0, T0, T1) :: push T0)  (* pop two values, push their sum                *)
let gIsub        = pop2 @ (Msub (T0, T0, T1) :: push T0)  (* pop two values, push their difference         *)
let gIbeq ofs    = pop2 @ [Mbeq (T0, T1, ofs)]            (* pop n2, pop n1, skip ofs forward if n1 =  n2  *)
let gIbne ofs    = pop2 @ [Mbne (T0, T1, ofs)]            (* pop n2, pop n1, skip ofs forward if n1 <> n2  *)
let gIble ofs    = pop2 @ ble ofs                         (* pop n2, pop n1, skip ofs forward if n1 <= n2  *)
let gIbgt ofs    = pop2 @ bgt ofs                         (* pop n2, pop n1, skip ofs forward if n1 >  n2  *)
let gIhalt       = Mlab "halt" :: gIbranch "halt"        (* end of program (endless loop)                 *)

let string_of_m_prog p =
  
  let rec asm = function
    | [] -> ""
    | Mlab l :: il -> m_instr_to_string (Mlab l) ^ nl ^ asm il
    | Mcom s :: il -> "\t\t\t\t" ^ m_instr_to_string (Mcom s) ^ nl ^ asm il
    | i :: il -> "\t" ^ m_instr_to_string i ^ nl ^ asm il
  in
  "\t.data" ^ nl ^ 
  "\t.space 1024\t\t# just a placholder" ^ nl ^ 
  "\t.set noreorder\t\t# necessary to avoid code optimization" ^ nl ^
  "\t.set noat\t\t# necessary to avoid warning accessing $at" ^ nl ^
  "\t.text\t\t\t# .text segment (code)" ^ nl ^
  "\tla $gp, .data\t\t# set gp to accesible data area" ^ nl ^
  asm (Mcom "sp initialized to 0x80000000 on reset" :: p @ gIhalt) ^ nl

      
let rec m_compile_aexpr (a:aexpr) : m_code =
  match a with 
  | Anum n        -> gIconst (to_int n)
  | Avar (Id x)   -> gIvar (to_int x) (* x being a positive integer x >= 0 *)
  | Aadd (a1, a2) -> m_compile_aexpr a1 @ m_compile_aexpr a2 @ gIadd 
  | Asub (a1, a2) -> m_compile_aexpr a1 @ m_compile_aexpr a2 @ gIsub 
  | Amul (a1, a2) -> raise (CompilerError "multiplication not supported")
 
let lnr = ref 0
let lnew () = lnr := !lnr + 1; !lnr 
   
let rec m_compile_bexpr (b:bexpr) (negexp:bool) (btarget:string) =
  match b with
  | Btrue         -> if negexp then gIbranch btarget else [] 
  | Bfalse        -> if negexp then [] else gIbranch btarget 
  | Bnot b1       -> m_compile_bexpr b1 (not negexp) btarget
  | Band (b1, b2) ->
      let l_and = "and" ^ string_of_int (lnew ()) in 
      let c2  = m_compile_bexpr b2 negexp btarget in
      let l_target = if negexp then l_and else btarget in
      [Mcom "and expression"] @ m_compile_bexpr b1 false l_target @ [Mlab l_and] @ c2
  | Beq (a1, a2)  -> m_compile_aexpr a1 @ m_compile_aexpr a2 @ if negexp then gIbeq btarget else gIbne btarget
  | Ble (a1, a2)  -> m_compile_aexpr a1 @ m_compile_aexpr a2 @ if negexp then gIble btarget else gIbgt btarget
                  

let rec m_compile_com (cmd: com) = 
  match cmd with
  | Cskip                  -> [Mcom "skip"] 
  | Cassign (Id x, a)      -> m_compile_aexpr a @ gIsetvar (to_int x)
  | Cseq (cmd1, cmd2)      -> m_compile_com cmd1 @ m_compile_com cmd2
  | Cif (cond, cmd1, cmd2) -> 
    let ln = lnew() in
    let l_endif = "endif" ^ string_of_int ln in
    let l_else = "else" ^ string_of_int ln in
    let code_false = m_compile_com cmd2 in
    let code_true = m_compile_com cmd1 @ gIbranch l_endif in
    [Mcom "if command"] @ [Mcom "condition"] @ m_compile_bexpr cond false l_else @ 
    [Mcom "true branch"] @ code_true @
    [Mcom "false branch"] @ [Mlab l_else] @ code_false @ [Mlab l_endif]   
  | Cwhile (test, body)    ->
    let ln = lnew () in
    let l_while = "while" ^ string_of_int ln in
    let l_done = "done" ^ string_of_int ln in  
    let code_body = m_compile_com body in
    let code_test = m_compile_bexpr test false l_done in
    [Mcom "while command"] @
    [Mlab l_while] @ 
    [Mcom "condition"] @ 
    code_test @ code_body @ gIbranch l_while @
    [Mlab l_done] 
 

(*
 let rec m_compile_aexpr (a:aexpr) : m_code =
  match a with 
  | Anum n        -> gIconst (to_int n)
  | Avar (Id x)   -> gIvar (to_int x) (* x being a positive integer x >= 0 *)
  | Aadd (a1, a2) -> m_compile_aexpr a1 @ m_compile_aexpr a2 @ gIadd 
  | Asub (a1, a2) -> m_compile_aexpr a1 @ m_compile_aexpr a2 @ gIsub 
  | Amul (a1, a2) -> raise (CompilerError "multimplication not supported")
 

let rec m_compile_bexpr (b:bexpr) (cond:bool) (ofs:int) =
  match b with
  | Btrue         -> if cond then gIbranch ofs else gInil 
  | Bfalse        -> if cond then gInil else gIbranch ofs 
  | Bnot b1       -> m_compile_bexpr b1 (not cond) ofs
  | Band (b1, b2) ->
      let c2  = m_compile_bexpr b2 cond ofs in
      let ofs = if cond then length c2 else ofs + length c2 in
      m_compile_bexpr b1 false ofs @ c2
  | Beq (a1, a2)  -> m_compile_aexpr a1 @ m_compile_aexpr a2 @ if cond then gIbeq ofs else gIbne ofs
  | Ble (a1, a2)  -> m_compile_aexpr a1 @ m_compile_aexpr a2 @ if cond then gIble ofs else gIbgt ofs
                  

let rec m_compile_com (cmd: com) = 
  match cmd with
  | Cskip                  -> gInil 
  | Cassign (Id x, a)      -> m_compile_aexpr a @ gIsetvar (to_int x)
  | Cseq (cmd1, cmd2)      -> m_compile_com cmd1 @ m_compile_com cmd2
  | Cif (cond, cmd1, cmd2) -> 
    let code_false = m_compile_com cmd2 in
    let code_true = m_compile_com cmd1 @ gIbranch (length code_false) in
    m_compile_bexpr cond false (length code_true) @ code_true @ code_false 
    
  | Cwhile (test, body)    -> 
    let code_body = m_compile_com body in
    let body_length = (length code_body) + 1 in
    let code_test = m_compile_bexpr test false body_length in
    let ofs = (length code_test) + body_length in
    code_test @ code_body @ gIbranch (- ofs) 
 
 *)