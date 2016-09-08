(* This file has been generated from Why3 theory list.Nth *)

open Why3extract

let rec nth (n: Why3extract.Why3__BigInt.t) (l: 'a list) =
  begin match l with
  | [] -> None
  | (x :: r) ->
      if (Why3extract.Why3__BigInt.eq n Why3extract.Why3__BigInt.zero)
      then
        (Some x)
      else
        nth (Why3__BigInt.sub n Why3extract.Why3__BigInt.one) r end


