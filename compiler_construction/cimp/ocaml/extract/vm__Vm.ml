(* This file has been generated from Why3 theory vm.Vm *)

open Why3extract

type pos = Why3extract.Why3__BigInt.t

type stack = Why3extract.Why3__BigInt.t list

type machine_state =
  | VMS of Why3extract.Why3__BigInt.t * Why3extract.Why3__BigInt.t list *
      ((State__StateGen.id, Why3extract.Why3__BigInt.t) Map__Map.map)

type ofs = Why3extract.Why3__BigInt.t

type instr =
  | Iconst of Why3extract.Why3__BigInt.t
  | Ivar of State__StateGen.id
  | Isetvar of State__StateGen.id
  | Ibranch of Why3extract.Why3__BigInt.t
  | Iadd
  | Isub
  | Imul
  | Ibeq of Why3extract.Why3__BigInt.t
  | Ibne of Why3extract.Why3__BigInt.t
  | Ible of Why3extract.Why3__BigInt.t
  | Ibgt of Why3extract.Why3__BigInt.t
  | Ihalt

type code = instr list

let push (n: Why3extract.Why3__BigInt.t) (s: Why3extract.Why3__BigInt.t list)
  = (n :: s)

let iconst (n: Why3extract.Why3__BigInt.t) = ((Iconst n) :: [])

let ivar (x: State__StateGen.id) = ((Ivar x) :: [])

let isetvar (x: State__StateGen.id) = ((Isetvar x) :: [])

let iadd  = (Iadd :: [])

let isub  = (Isub :: [])

let imul  = (Imul :: [])

let ibeq (ofs: Why3extract.Why3__BigInt.t) = ((Ibeq ofs) :: [])

let ible (ofs: Why3extract.Why3__BigInt.t) = ((Ible ofs) :: [])

let ibne (ofs: Why3extract.Why3__BigInt.t) = ((Ibne ofs) :: [])

let ibgt (ofs: Why3extract.Why3__BigInt.t) = ((Ibgt ofs) :: [])

let ibranch (ofs: Why3extract.Why3__BigInt.t) = ((Ibranch ofs) :: [])

let ihalt  = (Ihalt :: [])


