(* This file has been generated from Why3 module Compile_aexpr *)

open Why3extract

let rec compile_aexpr (a: Imp__Imp.aexpr) =
  let c =
    begin match a with
    | Imp__Imp.Anum n -> let o = Specs__VM_instr_spec.iconstf n in
        Logic__Compiler_logic.prefix_dl o
    | Imp__Imp.Avar x -> let o = Specs__VM_instr_spec.ivarf x in
        Logic__Compiler_logic.prefix_dl o
    | Imp__Imp.Aadd (a1, a2) ->
        let o =
          let o1 = let o2 = () in Specs__VM_instr_spec.iaddf o2 in
          Logic__Compiler_logic.prefix_dl o1 in
        let o1 =
          let o2 =
            let o3 = compile_aexpr a2 in Logic__Compiler_logic.prefix_dl o3 in
          let o3 =
            let o4 = compile_aexpr a1 in Logic__Compiler_logic.prefix_dl o4 in
          Logic__Compiler_logic.infix_tl o3 o2 in
        Logic__Compiler_logic.infix_tl o1 o
    | Imp__Imp.Asub (a1, a2) ->
        let o =
          let o1 = let o2 = () in Specs__VM_instr_spec.isubf o2 in
          Logic__Compiler_logic.prefix_dl o1 in
        let o1 =
          let o2 =
            let o3 = compile_aexpr a2 in Logic__Compiler_logic.prefix_dl o3 in
          let o3 =
            let o4 = compile_aexpr a1 in Logic__Compiler_logic.prefix_dl o4 in
          Logic__Compiler_logic.infix_tl o3 o2 in
        Logic__Compiler_logic.infix_tl o1 o
    | Imp__Imp.Amul (a1, a2) ->
        let o =
          let o1 = let o2 = () in Specs__VM_instr_spec.imulf o2 in
          Logic__Compiler_logic.prefix_dl o1 in
        let o1 =
          let o2 =
            let o3 = compile_aexpr a2 in Logic__Compiler_logic.prefix_dl o3 in
          let o3 =
            let o4 = compile_aexpr a1 in Logic__Compiler_logic.prefix_dl o4 in
          Logic__Compiler_logic.infix_tl o3 o2 in
        Logic__Compiler_logic.infix_tl o1 o end in
  Logic__Compiler_logic.hoare  c

let compile_aexpr_natural (a: Imp__Imp.aexpr) = let res = compile_aexpr a in
  res.Logic__Compiler_logic.code


