(* This file has been generated from Why3 module Compile_com *)

open Why3extract

let rec compile_com (cmd: Imp__Imp.com) =
  let res =
    begin match cmd with
    | Imp__Imp.Cskip ->
        let o = let o1 = () in Specs__VM_instr_spec.inil o1 in
        Logic__Compiler_logic.prefix_dl o
    | Imp__Imp.Cassign (x1, a) ->
        let o =
          let o1 = Specs__VM_instr_spec.isetvarf x1 in
          Logic__Compiler_logic.prefix_dl o1 in
        let o1 =
          let o2 = Compiler__Compile_aexpr.compile_aexpr a in
          Logic__Compiler_logic.prefix_dl o2 in
        Logic__Compiler_logic.infix_tl o1 o
    | Imp__Imp.Cseq (cmd1, cmd2) ->
        let o =
          let o1 = compile_com cmd2 in Logic__Compiler_logic.prefix_dl o1 in
        let o1 =
          let o2 = compile_com cmd1 in Logic__Compiler_logic.prefix_dl o2 in
        Logic__Compiler_logic.infix_tl o1 o
    | Imp__Imp.Cif (cond, cmd1, cmd2) -> let code_false = compile_com cmd2 in
        let code_true =
          let o =
            let o1 =
              let o2 =
                List__Length.length code_false.Logic__Compiler_logic.code in
              Specs__VM_instr_spec.ibranchf o2 in
            Logic__Compiler_logic.prefix_dl o1 in
          let o1 =
            let o2 = compile_com cmd1 in Logic__Compiler_logic.prefix_dl o2 in
          Logic__Compiler_logic.infix_tl o1 o in
        let o =
          let o1 = Logic__Compiler_logic.prefix_dl code_false in
          Logic__Compiler_logic.infix_pc o1 in
        let o1 =
          let o2 = Logic__Compiler_logic.infix_pc code_true in
          let o3 =
            let o4 =
              let o5 =
                List__Length.length code_true.Logic__Compiler_logic.wcode in
              let o6 = false in
              Compiler__Compile_bexpr.compile_bexpr cond o6 o5 in
            Logic__Compiler_logic.prefix_dl o4 in
          Logic__Compiler_logic.infix_tl o3 o2 in
        Logic__Compiler_logic.infix_tl o1 o
    | Imp__Imp.Cwhile (test, body) -> let code_body = compile_com body in
        let body_length =
          (Why3__BigInt.add (List__Length.length
                               code_body.Logic__Compiler_logic.code) Why3extract.Why3__BigInt.one) in
        let code_test =
          let o = false in
          Compiler__Compile_bexpr.compile_bexpr test o body_length in
        let ofs =
          (Why3__BigInt.add (List__Length.length
                               code_test.Logic__Compiler_logic.code) body_length) in
        let wp_while =
          let o =
            let o1 =
              let o2 =
                let o3 =
                  let o4 = (Why3__BigInt.minus ofs) in
                  Specs__VM_instr_spec.ibranchf o4 in
                Logic__Compiler_logic.prefix_dl o3 in
              let o3 = Logic__Compiler_logic.prefix_dl code_body in
              Logic__Compiler_logic.infix_tl o3 o2 in
            Logic__Compiler_logic.infix_pc o1 in
          let o1 = Logic__Compiler_logic.prefix_dl code_test in
          Logic__Compiler_logic.infix_tl o1 o in
        let hl_while = Logic__Compiler_logic.hoare wp_while in
        let o =
          let o1 = Logic__Compiler_logic.make_loop_hl hl_while in
          Logic__Compiler_logic.prefix_dl o1 in
        let o1 =
          let o2 = let o3 = () in Specs__VM_instr_spec.inil o3 in
          Logic__Compiler_logic.prefix_dl o2 in
        Logic__Compiler_logic.infix_tl o1 o end in
  Logic__Compiler_logic.hoare (* () *) res

let compile_com_natural (com: Imp__Imp.com) = let res = compile_com com in
  res.Logic__Compiler_logic.code

let compile_program (prog: Imp__Imp.com) =
  let o = compile_com_natural prog in (List.append o (Vm__Vm.ihalt ))

let test (us: unit) =
  let x1 = State__StateGen.Id Why3extract.Why3__BigInt.zero in
  let y1 = State__StateGen.Id Why3extract.Why3__BigInt.one in
  let cond =
    Imp__Imp.Bnot (Imp__Imp.Ble (Imp__Imp.Avar y1,
                    Imp__Imp.Anum Why3extract.Why3__BigInt.zero)) in
  let body1 =
    Imp__Imp.Cassign (x1, Imp__Imp.Amul (Imp__Imp.Avar x1, Imp__Imp.Avar y1)) in
  let body2 =
    Imp__Imp.Cassign (y1,
     Imp__Imp.Asub (Imp__Imp.Avar y1,
      Imp__Imp.Anum Why3extract.Why3__BigInt.one)) in
  let lp = Imp__Imp.Cwhile (cond, Imp__Imp.Cseq (body1, body2)) in
  let code =
    Imp__Imp.Cseq
     (Imp__Imp.Cassign (x1, Imp__Imp.Anum Why3extract.Why3__BigInt.one), lp) in
  compile_program code

let test2 (us: unit) =
  let o = Imp__Imp.Cwhile (Imp__Imp.Btrue, Imp__Imp.Cskip) in
  compile_program o


