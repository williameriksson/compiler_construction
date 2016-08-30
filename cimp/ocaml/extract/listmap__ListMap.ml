(* This file has been generated from Why3 module ListMap *)

open Why3extract

type ('k, 'v) lm_map = {
  view: unit;
  c: 'v;
  kl: 'k list;
  vl: 'v list;
  }

let get (m: ('k, 'v) lm_map) (k: 'k) =
  begin match (List.mem k m.kl) with
  | true -> let o = m.vl in
      let o1 = let o2 = m.kl in Listmap__ListAux.fst_occ k o2 in
      Listmap__ListAux.nth o1 o
  | false -> m.c end

let mixfix_lbrb (m: ('k, 'v) lm_map) (k: 'k) = get m k

let add_unmapped (m: ('k, 'v) lm_map) (k: 'k) (v: 'v) =
  let kl = (k :: m.kl) in let vl = (v :: m.vl) in
  { view = (); c = m.c; kl = kl; vl = vl }

let add_mapped (m: ('k, 'v) lm_map) (k: 'k) (v: 'v) =
  let n = let o = m.kl in Listmap__ListAux.fst_occ k o in
  let o = let o1 = m.vl in Listmap__ListAux.write_nth n o1 v in
  { view = (); c = m.c; kl = m.kl; vl = o }

let set (m: ('k, 'v) lm_map) (k: 'k) (v: 'v) =
  begin match (List.mem k m.kl) with
  | true -> add_mapped m k v
  | false -> add_unmapped m k v end

let mixfix_lblsmnrb (m: ('k, 'v) lm_map) (k: 'k) (v: 'v) = set m k v


