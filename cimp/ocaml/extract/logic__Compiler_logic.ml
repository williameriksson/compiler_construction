(* This file has been generated from Why3 module logic.Compiler_logic *)

open Why3extract

let fst (p: 'a * 'b) = let (x, _) = p in x

let snd (p: 'a * 'b) = let (_, y) = p in y

type pred = (Vm__Vm.machine_state -> bool)

type rel = (Vm__Vm.machine_state -> (Vm__Vm.machine_state -> bool))

type 'a pre =
  ('a -> (Why3extract.Why3__BigInt.t -> (Vm__Vm.machine_state -> bool)))

type 'a post =
  ('a -> (Why3extract.Why3__BigInt.t -> (Vm__Vm.machine_state -> (Vm__Vm.machine_state -> bool))))

type 'a wp_trans =
  ('a -> (Why3extract.Why3__BigInt.t -> ((Vm__Vm.machine_state -> bool) -> (Vm__Vm.machine_state -> bool))))

let seq_wp (l1: Why3extract.Why3__BigInt.t)
  (w1:
  ('a -> (Why3extract.Why3__BigInt.t -> ((Vm__Vm.machine_state -> bool) -> (Vm__Vm.machine_state -> bool)))))
  (w2: ('a *
  Vm__Vm.machine_state -> (Why3extract.Why3__BigInt.t -> ((Vm__Vm.machine_state -> bool) -> (Vm__Vm.machine_state -> bool)))))
  =
  fun (x1: 'a) (p: Why3extract.Why3__BigInt.t)
    (q: (Vm__Vm.machine_state -> bool)) (ms: Vm__Vm.machine_state) ->
    ((((w1 x1) p) (((w2 (x1, ms)) (Why3__BigInt.add p l1)) q)) ms)

let fork_wp
  (w:
  ('a -> (Why3extract.Why3__BigInt.t -> ((Vm__Vm.machine_state -> bool) -> (Vm__Vm.machine_state -> bool)))))
  (cond:
  ('a -> (Why3extract.Why3__BigInt.t -> (Vm__Vm.machine_state -> bool)))) =
  fun (x1: 'a) (p: Why3extract.Why3__BigInt.t)
    (q: (Vm__Vm.machine_state -> bool)) (ms: Vm__Vm.machine_state) ->
    ((not (not ((((cond x1) p) ms) = true))) || ((q ms) = true)) &&
     ((not ((((cond x1) p) ms) = true)) || (((((w x1) p) q) ms) = true))

let trivial_pre  =
  fun (us: 'a) (p: Why3extract.Why3__BigInt.t) (ms: Vm__Vm.machine_state) ->
    let Vm__Vm.VMS (pqt, _, _) = ms in (Why3extract.Why3__BigInt.eq p pqt)

let loop_progress
  (inv:
  ('a -> (Why3extract.Why3__BigInt.t -> (Vm__Vm.machine_state -> bool))))
  (post:
  ('a -> (Why3extract.Why3__BigInt.t -> (Vm__Vm.machine_state -> bool))))
  (var:
  ('a -> (Why3extract.Why3__BigInt.t -> (Vm__Vm.machine_state -> (Vm__Vm.machine_state -> bool)))))
  =
  fun (x1: 'a) (p: Why3extract.Why3__BigInt.t) (ms: Vm__Vm.machine_state)
    (msqt: Vm__Vm.machine_state) ->
    (((((inv x1) p) msqt) = true) && (((((var x1) p) msqt) ms) = true)) ||
     ((((post x1) p) msqt) = true)

let forget_old
  (post:
  ('a -> (Why3extract.Why3__BigInt.t -> (Vm__Vm.machine_state -> bool)))) =
  fun (x1: 'a) (p: Why3extract.Why3__BigInt.t) (us: Vm__Vm.machine_state) ->
    ((post x1) p)

type 'a hl = {
  code: Vm__Vm.instr list;
  pre: unit;
  post: unit;
  }

type 'a wp = {
  wcode: Vm__Vm.instr list;
  wp: unit;
  }


let infix_tl (s1: 'a wp) (s2: Vm__Vm.machine_state wp) =
  let code = (List.append s1.wcode s2.wcode) in
  let res = { wcode = code; wp = () } in res


let infix_pc (s: 'a wp) = { wcode = s.wcode; wp = () }

let prefix_dl (c: 'a hl) = { wcode = c.code; wp = () }

let hoare (c: 'a wp) = { code = c.wcode; pre = (); post = () }

let make_loop_hl (c: 'a hl) =
  let res = { code = c.code; pre = (); post = () } in res


