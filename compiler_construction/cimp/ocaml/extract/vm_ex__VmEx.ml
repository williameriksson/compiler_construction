(* This file has been generated from Why3 module VmEx *)

open Why3extract

type machine_state_ex =
  | VMS_EX of Why3extract.Why3__BigInt.t * Why3extract.Why3__BigInt.t list *
      ((State__StateGen.id, Why3extract.Why3__BigInt.t)
      Listmap__ListMap.lm_map)

exception VmErr

exception VmEnd

let pop (s: Why3extract.Why3__BigInt.t list) =
  begin match s with
  | [] -> raise VmErr
  | (rv :: rs) -> (rv, rs) end

let vm_ex_one (c: Vm__Vm.instr list) (pos: Why3extract.Why3__BigInt.t)
  (stack: Why3extract.Why3__BigInt.t list)
  (state: (State__StateGen.id, Why3extract.Why3__BigInt.t)
  Listmap__ListMap.lm_map) =
  begin match List__Nth.nth pos c with
  | None -> raise VmErr
  | (Some instr) ->
      begin match instr with
      | Vm__Vm.Iconst n ->
          ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one),
          Vm__Vm.push n stack, state)
      | Vm__Vm.Ivar id ->
          let o =
            let o1 = Listmap__ListMap.mixfix_lbrb state id in
            Vm__Vm.push o1 stack in
          ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one), o, state)
      | Vm__Vm.Isetvar id -> let (rv, rs) = pop stack in
          let o = Listmap__ListMap.mixfix_lblsmnrb state id rv in
          ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one), rs, o)
      | Vm__Vm.Ibranch ofs ->
          ((Why3__BigInt.add (Why3__BigInt.add pos Why3extract.Why3__BigInt.one) ofs),
          stack, state)
      | Vm__Vm.Iadd -> let (rv1, rs1) = pop stack in
          let (rv2, rs2) = pop rs1 in
          ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one),
          Vm__Vm.push (Why3__BigInt.add rv1 rv2) rs2, state)
      | Vm__Vm.Isub -> let (rv11, rs11) = pop stack in
          let (rv21, rs21) = pop rs11 in
          ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one),
          Vm__Vm.push (Why3__BigInt.sub rv21 rv11) rs21, state)
      | Vm__Vm.Imul -> let (rv12, rs12) = pop stack in
          let (rv22, rs22) = pop rs12 in
          ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one),
          Vm__Vm.push (Why3__BigInt.mul rv12 rv22) rs22, state)
      | Vm__Vm.Ibeq ofs -> let (rv13, rs13) = pop stack in
          let (rv23, rs23) = pop rs13 in
          begin match (Why3extract.Why3__BigInt.eq rv13 rv23) with
          | true ->
              ((Why3__BigInt.add (Why3__BigInt.add pos Why3extract.Why3__BigInt.one) ofs),
              rs23, state)
          | _ -> ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one), rs23,
              state) end
      | Vm__Vm.Ibne ofs -> let (rv14, rs14) = pop stack in
          let (rv24, rs24) = pop rs14 in
          begin match not (Why3extract.Why3__BigInt.eq rv14 rv24) with
          | true ->
              ((Why3__BigInt.add (Why3__BigInt.add pos Why3extract.Why3__BigInt.one) ofs),
              rs24, state)
          | _ -> ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one), rs24,
              state) end
      | Vm__Vm.Ible ofs -> let (rv15, rs15) = pop stack in
          let (rv25, rs25) = pop rs15 in
          begin match (Why3__BigInt.le rv25 rv15) with
          | true ->
              ((Why3__BigInt.add (Why3__BigInt.add pos Why3extract.Why3__BigInt.one) ofs),
              rs25, state)
          | _ -> ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one), rs25,
              state) end
      | Vm__Vm.Ibgt ofs -> let (rv16, rs16) = pop stack in
          let (rv26, rs26) = pop rs16 in
          begin match (Why3__BigInt.gt rv26 rv16) with
          | true ->
              ((Why3__BigInt.add (Why3__BigInt.add pos Why3extract.Why3__BigInt.one) ofs),
              rs26, state)
          | _ -> ((Why3__BigInt.add pos Why3extract.Why3__BigInt.one), rs26,
              state) end
      | Vm__Vm.Ihalt -> raise VmEnd end end

exception VmNotEnd

let rec vm_ex_mult (c: Vm__Vm.instr list) (pos: Why3extract.Why3__BigInt.t)
  (stack: Why3extract.Why3__BigInt.t list)
  (state: (State__StateGen.id, Why3extract.Why3__BigInt.t)
  Listmap__ListMap.lm_map) (n: Nat__Nat.nat) =
  begin match n with
  | Nat__Nat.O -> raise VmNotEnd
  | Nat__Nat.S nqt ->
      begin try let (p, s, m) = vm_ex_one c pos stack state in
        vm_ex_mult c p s m nqt
      with
      | VmEnd -> (pos, stack, state, nqt)
      end end


