(* This file has been generated from Why3 module Compile_bexpr *)

open Why3extract

let rec compile_bexpr (b: Imp__Imp.bexpr) (cond: bool)
  (ofs: Why3extract.Why3__BigInt.t) =
  let c =
    begin match b with
    | Imp__Imp.Btrue ->
        let o =
          if cond
          then
            Specs__VM_instr_spec.ibranchf ofs
          else
            let o1 = () in Specs__VM_instr_spec.inil o1 in
        Logic__Compiler_logic.prefix_dl o
    | Imp__Imp.Bfalse ->
        let o =
          if cond
          then
            let o1 = () in Specs__VM_instr_spec.inil o1
          else
            Specs__VM_instr_spec.ibranchf ofs in
        Logic__Compiler_logic.prefix_dl o
    | Imp__Imp.Bnot b1 ->
        let o = let o1 = not (cond = true) in compile_bexpr b1 o1 ofs in
        Logic__Compiler_logic.prefix_dl o
    | Imp__Imp.Band (b1, b2) ->
        let c2 =
          let o =
            let o1 = compile_bexpr b2 cond ofs in
            Logic__Compiler_logic.prefix_dl o1 in
          Logic__Compiler_logic.infix_pc o in
        let ofs1 =
          if cond
          then
            List__Length.length c2.Logic__Compiler_logic.wcode
          else
            (Why3__BigInt.add ofs (List__Length.length
                                     c2.Logic__Compiler_logic.wcode)) in
        let o =
          let o1 = let o2 = false in compile_bexpr b1 o2 ofs1 in
          Logic__Compiler_logic.prefix_dl o1 in
        Logic__Compiler_logic.infix_tl o c2
    | Imp__Imp.Beq (a1, a2) ->
        let o =
          let o1 =
            if cond
            then
              Specs__VM_instr_spec.ibeqf ofs
            else
              Specs__VM_instr_spec.ibnef ofs in
          Logic__Compiler_logic.prefix_dl o1 in
        let o1 =
          let o2 =
            let o3 = Compiler__Compile_aexpr.compile_aexpr a2 in
            Logic__Compiler_logic.prefix_dl o3 in
          let o3 =
            let o4 = Compiler__Compile_aexpr.compile_aexpr a1 in
            Logic__Compiler_logic.prefix_dl o4 in
          Logic__Compiler_logic.infix_tl o3 o2 in
        Logic__Compiler_logic.infix_tl o1 o
    | Imp__Imp.Ble (a1, a2) ->
        let o =
          let o1 =
            if cond
            then
              Specs__VM_instr_spec.iblef ofs
            else
              Specs__VM_instr_spec.ibgtf ofs in
          Logic__Compiler_logic.prefix_dl o1 in
        let o1 =
          let o2 =
            let o3 = Compiler__Compile_aexpr.compile_aexpr a2 in
            Logic__Compiler_logic.prefix_dl o3 in
          let o3 =
            let o4 = Compiler__Compile_aexpr.compile_aexpr a1 in
            Logic__Compiler_logic.prefix_dl o4 in
          Logic__Compiler_logic.infix_tl o3 o2 in
        Logic__Compiler_logic.infix_tl o1 o end in
  Logic__Compiler_logic.hoare (* () *) c

let compile_bexpr_natural (b: Imp__Imp.bexpr) (cond: bool)
  (ofs: Why3extract.Why3__BigInt.t) = let res = compile_bexpr b cond ofs in
  res.Logic__Compiler_logic.code


