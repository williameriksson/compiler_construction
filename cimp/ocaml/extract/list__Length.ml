(* This file has been generated from Why3 theory list.Length *)

open Why3extract

let rec length (l: 'a list) =
  begin match l with
  | [] -> Why3extract.Why3__BigInt.zero
  | (_ :: r) -> (Why3__BigInt.add Why3extract.Why3__BigInt.one (length r)) end


