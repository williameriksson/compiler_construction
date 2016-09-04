(* This file has been generated from Why3 module ImpEx *)

open Why3extract

exception CErr

let rec aeval
  (st: (State__StateGen.id, Why3extract.Why3__BigInt.t)
  Listmap__ListMap.lm_map) (e: Imp__Imp.aexpr) =
  begin match e with
  | Imp__Imp.Anum n -> n
  | Imp__Imp.Avar x -> Listmap__ListMap.mixfix_lbrb st x
  | Imp__Imp.Aadd (e1, e2) -> let o = aeval st e2 in let o1 = aeval st e1 in
      (Why3__BigInt.add o1 o)
  | Imp__Imp.Asub (e1, e2) -> let o = aeval st e2 in let o1 = aeval st e1 in
      (Why3__BigInt.sub o1 o)
  | Imp__Imp.Amul (e1, e2) -> let o = aeval st e2 in let o1 = aeval st e1 in
      (Why3__BigInt.mul o1 o) end

let rec beval
  (st: (State__StateGen.id, Why3extract.Why3__BigInt.t)
  Listmap__ListMap.lm_map) (b: Imp__Imp.bexpr) =
  begin match b with
  | Imp__Imp.Btrue -> true
  | Imp__Imp.Bfalse -> false
  | Imp__Imp.Bnot bqt -> let o = beval st bqt in (not o)
  | Imp__Imp.Band (b1, b2) -> let o = beval st b2 in let o1 = beval st b1 in
      (o1 && o)
  | Imp__Imp.Beq (a1, a2) -> let o = aeval st a2 in let o1 = aeval st a1 in
      (Why3extract.Why3__BigInt.eq o1 o)
  | Imp__Imp.Ble (a1, a2) -> let o = aeval st a2 in let o1 = aeval st a1 in
      (Why3__BigInt.le o1 o) end

let rec ceval_ex
  (st: (State__StateGen.id, Why3extract.Why3__BigInt.t)
  Listmap__ListMap.lm_map) (com: Imp__Imp.com) (n: Nat__Nat.nat) =
  if (n = Nat__Nat.O)
  then
    raise CErr
  else
    begin match com with
    | Imp__Imp.Cskip -> st
    | Imp__Imp.Cassign (x, a) -> let o = aeval st a in
        Listmap__ListMap.mixfix_lblsmnrb st x o
    | Imp__Imp.Cseq (cmd1, cmd2) ->
        let mqt = let o = Nat__Nat.pred n in ceval_ex st cmd1 o in
        let o = Nat__Nat.pred n in ceval_ex mqt cmd2 o
    | Imp__Imp.Cif (cond, cmd1, cmd2) ->
        begin match beval st cond with
        | true -> let o = Nat__Nat.pred n in ceval_ex st cmd1 o
        | false -> let o = Nat__Nat.pred n in ceval_ex st cmd2 o end
    | Imp__Imp.Cwhile (cond, body) ->
        begin match beval st cond with
        | true -> let o = Nat__Nat.pred n in
            let o1 = Imp__Imp.Cwhile (cond, body) in
            let o2 = let o3 = Nat__Nat.pred n in ceval_ex st body o3 in
            ceval_ex o2 o1 o
        | false -> st end end


