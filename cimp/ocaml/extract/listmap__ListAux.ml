(* This file has been generated from Why3 module ListAux *)

open Why3extract

let rec fst_occ (x: 'a) (l: 'a list) =
  begin match l with
  | [] -> assert false (* absurd *)
  | (y :: ll) ->
      if (x = y)
      then
        Why3extract.Why3__BigInt.zero
      else
        let o = fst_occ x ll in
        (Why3__BigInt.add Why3extract.Why3__BigInt.one o) end

let rec write_nth (n: Why3extract.Why3__BigInt.t) (l: 'a list) (x: 'a) =
  begin match (Why3extract.Why3__BigInt.eq n Why3extract.Why3__BigInt.zero) with
  | true ->
      begin match l with
      | [] -> assert false (* absurd *)
      | (_ :: l1) -> (x :: l1) end
  | false ->
      begin match l with
      | [] -> assert false (* absurd *)
      | (e :: l1) ->
          let o =
            let o1 = (Why3__BigInt.sub n Why3extract.Why3__BigInt.one) in
            write_nth o1 l1 x in (e :: o) end end

let rec nth (n: Why3extract.Why3__BigInt.t) (l: 'a list) =
  begin match (Why3extract.Why3__BigInt.eq n Why3extract.Why3__BigInt.zero) with
  | true ->
      begin match l with
      | [] -> assert false (* absurd *)
      | (x :: _) -> x end
  | false ->
      begin match l with
      | [] -> assert false (* absurd *)
      | (_ :: l1) ->
          let o = (Why3__BigInt.sub n Why3extract.Why3__BigInt.one) in
          nth o l1 end end


