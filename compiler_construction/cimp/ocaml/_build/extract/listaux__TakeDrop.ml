(* This file has been generated from Why3 module TakeDrop *)

open Why3extract

let rec take (n: Nat__Nat.nat) (l: 'a list) =
  begin match n with
  | Nat__Nat.S nqt ->
      begin match l with
      | [] -> []
      | (e :: ll) -> (e :: (take nqt ll)) end
  | Nat__Nat.O -> [] end

let rec drop (n: Nat__Nat.nat) (l: 'a list) =
  begin match n with
  | Nat__Nat.S nqt ->
      begin match l with
      | [] -> []
      | (_ :: ll) -> drop nqt ll end
  | Nat__Nat.O -> l end

let rec to_int (n: Nat__Nat.nat) =
  begin match n with
  | Nat__Nat.O -> Why3extract.Why3__BigInt.zero
  | Nat__Nat.S nqt ->
      (Why3__BigInt.add Why3extract.Why3__BigInt.one (to_int nqt)) end

let rec length_n (l: 'a list) =
  begin match l with
  | [] -> Nat__Nat.O
  | (_ :: ll) -> Nat__Nat.S (length_n ll) end

let rec of_int (i: Why3extract.Why3__BigInt.t) =
  begin match (Why3__BigInt.gt i Why3extract.Why3__BigInt.zero) with
  | true ->
      let o =
        let o1 = (Why3__BigInt.sub i Why3extract.Why3__BigInt.one) in
        of_int o1 in Nat__Nat.S o
  | _ -> Nat__Nat.O end

let rec take (i: Why3extract.Why3__BigInt.t) (l: 'a list) =
  begin match (Why3__BigInt.lt Why3extract.Why3__BigInt.zero i) with
  | true ->
      begin match l with
      | [] -> []
      | (e :: ll) ->
          let o =
            let o1 = (Why3__BigInt.sub i Why3extract.Why3__BigInt.one) in
            take o1 ll in (e :: o) end
  | false -> [] end

let rec drop (i: Why3extract.Why3__BigInt.t) (l: 'a list) =
  begin match (Why3__BigInt.lt Why3extract.Why3__BigInt.zero i) with
  | true ->
      begin match l with
      | [] -> []
      | (_ :: ll) ->
          let o = (Why3__BigInt.sub i Why3extract.Why3__BigInt.one) in
          drop o ll end
  | false -> l end


