(* This file has been generated from Why3 module specs.VM_instr_spec *)

open Why3extract

let ifun_post (f: (Vm__Vm.machine_state -> Vm__Vm.machine_state)) =
  fun (us: 'a2) (us1: Why3extract.Why3__BigInt.t) (ms: Vm__Vm.machine_state)
    (msqt: Vm__Vm.machine_state) -> (msqt = (f ms))

let iconst_fun (n: Why3extract.Why3__BigInt.t) =
  fun (ms: Vm__Vm.machine_state) -> let Vm__Vm.VMS (p, s, m) = ms in
    Vm__Vm.VMS ((Why3__BigInt.add p Why3extract.Why3__BigInt.one),
     Vm__Vm.push n s, m)

type binop =
  (Why3extract.Why3__BigInt.t -> (Why3extract.Why3__BigInt.t -> Why3extract.Why3__BigInt.t))

let ibinop_fun
  (op:
  (Why3extract.Why3__BigInt.t -> (Why3extract.Why3__BigInt.t -> Why3extract.Why3__BigInt.t)))
  =
  fun (ms: Vm__Vm.machine_state) ->
    begin match ms with
    | Vm__Vm.VMS (p1, (n2 :: (n1 :: s1)), m1) ->
        Vm__Vm.VMS ((Why3__BigInt.add p1 Why3extract.Why3__BigInt.one),
         Vm__Vm.push ((op n1) n2) s1, m1)
    | _ -> ms end

let plus  =
  fun (x1: Why3extract.Why3__BigInt.t) (y1: Why3extract.Why3__BigInt.t) ->
    (Why3__BigInt.add x1 y1)

let sub  =
  fun (x1: Why3extract.Why3__BigInt.t) (y1: Why3extract.Why3__BigInt.t) ->
    (Why3__BigInt.sub x1 y1)

let mul  =
  fun (x1: Why3extract.Why3__BigInt.t) (y1: Why3extract.Why3__BigInt.t) ->
    (Why3__BigInt.mul x1 y1)

let inil_post  =
  fun (us: 'a) (us1: Why3extract.Why3__BigInt.t) (ms: Vm__Vm.machine_state)
    (msqt: Vm__Vm.machine_state) -> (ms = msqt)

let ibranch_fun (ofs: Why3extract.Why3__BigInt.t) =
  fun (ms: Vm__Vm.machine_state) -> let Vm__Vm.VMS (p1, s1, m1) = ms in
    Vm__Vm.VMS
     ((Why3__BigInt.add (Why3__BigInt.add p1 Why3extract.Why3__BigInt.one) ofs),
     s1, m1)

type cond =
  (Why3extract.Why3__BigInt.t -> (Why3extract.Why3__BigInt.t -> bool))

let icjump_fun
  (cond:
  (Why3extract.Why3__BigInt.t -> (Why3extract.Why3__BigInt.t -> bool)))
  (ofs: Why3extract.Why3__BigInt.t) =
  fun (ms: Vm__Vm.machine_state) ->
    begin match ms with
    | Vm__Vm.VMS (p2, (n2 :: (n1 :: s2)), m2) ->
        if (((cond n1) n2) = true)
        then
          Vm__Vm.VMS
           ((Why3__BigInt.add (Why3__BigInt.add p2 ofs) Why3extract.Why3__BigInt.one),
           s2, m2)
        else
          Vm__Vm.VMS ((Why3__BigInt.add p2 Why3extract.Why3__BigInt.one), s2,
           m2)
    | _ -> ms end

let beq  =
  fun (x1: Why3extract.Why3__BigInt.t) (y1: Why3extract.Why3__BigInt.t) ->
    (Why3extract.Why3__BigInt.eq x1 y1)

let bne  =
  fun (x1: Why3extract.Why3__BigInt.t) (y1: Why3extract.Why3__BigInt.t) ->
    not (Why3extract.Why3__BigInt.eq x1 y1)

let ble  =
  fun (x1: Why3extract.Why3__BigInt.t) (y1: Why3extract.Why3__BigInt.t) ->
    (Why3__BigInt.le x1 y1)

let bgt  =
  fun (x1: Why3extract.Why3__BigInt.t) (y1: Why3extract.Why3__BigInt.t) ->
    (Why3__BigInt.gt x1 y1)

let ifunf (code_f: Vm__Vm.instr list) =
  let res =
    { Logic__Compiler_logic.code = code_f; Logic__Compiler_logic.pre = ();
     Logic__Compiler_logic.post = () } in res

let iconstf (n: Why3extract.Why3__BigInt.t) =
  let o =
    let o1 = let o2 = Vm__Vm.iconst n in ifunf  o2 in
    Logic__Compiler_logic.prefix_dl o1 in Logic__Compiler_logic.hoare  o

let ivarf (x1: State__StateGen.id) =
  let o =
    let o1 = let o2 = Vm__Vm.ivar x1 in ifunf  o2 in
    Logic__Compiler_logic.prefix_dl o1 in Logic__Compiler_logic.hoare o

let create_binop (code_b: Vm__Vm.instr list) =
  let o = let o1 = ifunf  code_b in Logic__Compiler_logic.prefix_dl o1 in
  Logic__Compiler_logic.hoare  o

let iaddf (us: unit) = let o = Vm__Vm.iadd  in create_binop o

let isubf (us: unit) = let o = Vm__Vm.isub  in create_binop o

let imulf (us: unit) = let o = Vm__Vm.imul  in create_binop o

let inil (us: unit) =
  { Logic__Compiler_logic.code = []; Logic__Compiler_logic.pre = ();
   Logic__Compiler_logic.post = () }

let ibranchf (ofs: Why3extract.Why3__BigInt.t) =
  let cf =
    let o = let o1 = Vm__Vm.ibranch ofs in ifunf  o1 in
    Logic__Compiler_logic.prefix_dl o in Logic__Compiler_logic.hoare  cf

let create_cjump (code_cd: Vm__Vm.instr list) =
  let c = let o = ifunf  code_cd in Logic__Compiler_logic.prefix_dl o in
  Logic__Compiler_logic.hoare c

let ibeqf (ofs: Why3extract.Why3__BigInt.t) = let o = Vm__Vm.ibeq ofs in
  create_cjump o

let ibnef (ofs: Why3extract.Why3__BigInt.t) = let o = Vm__Vm.ibne ofs in
  create_cjump o

let iblef (ofs: Why3extract.Why3__BigInt.t) = let o = Vm__Vm.ible ofs in
  create_cjump o

let ibgtf (ofs: Why3extract.Why3__BigInt.t) = let o = Vm__Vm.ibgt ofs in
  create_cjump o

let isetvarf (x1: State__StateGen.id) =
  let c =
    let o = let o1 = Vm__Vm.isetvar x1 in ifunf o1 in
    Logic__Compiler_logic.prefix_dl o in Logic__Compiler_logic.hoare  c


