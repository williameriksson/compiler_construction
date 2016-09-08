(* This file has been generated from Why3 theory imp.Imp *)

open Why3extract

type aexpr =
  | Anum of Why3extract.Why3__BigInt.t
  | Avar of State__StateGen.id
  | Aadd of aexpr * aexpr
  | Asub of aexpr * aexpr
  | Amul of aexpr * aexpr

type bexpr =
  | Btrue
  | Bfalse
  | Band of bexpr * bexpr
  | Bnot of bexpr
  | Beq of aexpr * aexpr
  | Ble of aexpr * aexpr

type com =
  | Cskip
  | Cassign of State__StateGen.id * aexpr
  | Cseq of com * com
  | Cif of bexpr * com * com
  | Cwhile of bexpr * com


