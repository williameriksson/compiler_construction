(* This file has been generated from Why3 theory nat.Nat *)


type nat =
  | O
  | S of nat

let rec add (n: nat) (m: nat) =
  begin match n with
  | O -> m
  | S p -> S (add p m) end

let pred (n: nat) = begin match n with
                    | O -> n
                    | S u -> u end

let rec sub (n: nat) (m: nat) =
  begin match (n, m) with
  | (S k, S l) -> sub k l
  | (_, _) -> n end

let rec mul (n: nat) (m: nat) =
  begin match n with
  | O -> O
  | S p -> add m (mul p m) end


