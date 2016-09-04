open Why3extract.Why3__BigInt (* of_int, to_string, etc. *)
open Imp__Imp
open State__StateEx
open State__StateGen
open Vm__Vm
open Common
open Dump
open Env

let rec why_of_aexpr = function
  | Anum v          -> to_string v 
  | Avar id         -> "!" ^ pretty_of_id id 
  | Aadd (e1, e2)   -> "(" ^ why_of_aexpr e1 ^ " + " ^ why_of_aexpr e2 ^ ")"
  | Asub (e1, e2)   -> "(" ^ why_of_aexpr e1 ^ " - " ^ why_of_aexpr e2 ^ ")"
  | Amul (e1, e2)   -> "(" ^ why_of_aexpr e1 ^ " * " ^ why_of_aexpr e2 ^ ")"

let rec why_of_bexpr = function
  | Btrue           -> "true"
  | Bfalse          -> "false"
  | Band (b1, b2)   -> "(" ^ why_of_bexpr b1 ^ " && " ^ why_of_bexpr b2 ^ ")"
  | Bnot b          -> "not " ^ why_of_bexpr b 
  | Beq (e1, e2)    -> "(" ^ why_of_aexpr e1 ^ " = " ^ why_of_aexpr e2 ^ ")"
  | Ble (e1, e2)    -> "(" ^ why_of_aexpr e1 ^ " <= " ^ why_of_aexpr e2 ^ ")"

let rec why_of_com t = function 
  | Cskip           -> ""
  | Cassign (id, a) -> tab t ^ pretty_of_id id ^ " := " ^ why_of_aexpr a
  | Cseq (c1, c2)   -> why_of_com t c1 ^ ";" ^ nl ^ why_of_com t c2 
  | Cif (b, c1, c2) -> tab t ^ "if " ^ why_of_bexpr b ^ " then" ^ nl ^
                          why_of_com (t + 1) c1 ^ nl ^ 
                       tab t ^ "else" ^ nl ^ 
                          why_of_com (t + 1) c2 ^ nl ^ 
                       tab t ^ "end"
  | Cwhile (b, c)   -> tab t ^ "while " ^ why_of_bexpr b ^ " do" ^ nl ^ 
                          why_of_com (t + 1) c ^ nl ^ 
                       tab t ^ "done"
let declarations_of_map () = 
  let rec str = function 
  | [] -> ""
  | (s, _) :: l' -> str l' ^ "  let " ^ s ^ " = ref 0 in" ^ nl
  in
  "  (* variable declarations *)" ^ nl ^ 
  str !id_map 
  
let why_of_prog  com =
  "module Cimp" ^ nl ^
  "use import int.Int" ^ nl ^
  "use import ref.Ref" ^ nl ^
  "let prog () = " ^ nl ^
  declarations_of_map () ^ nl ^ 
  "  (* commands *)" ^ nl ^ 
  why_of_com 1 com ^ nl ^
  "end"
  
  
  
  