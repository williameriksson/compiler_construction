open Common
open Why3extract.Why3__BigInt
open Vm__Vm
open Imp__Imp
open State__StateEx
open State__StateGen
open List

type m_reg =
  | ZE | T of int | S0 | S1 | SP | GP | AT

type m_instr =
  | Mlab of string
  | Madd of m_reg * m_reg * m_reg
  | Maddi of m_reg * m_reg * int
  | Mand of m_reg * m_reg * m_reg
  | Mandi of m_reg * m_reg * int
  | Mbeq of m_reg * m_reg * string
  | Mbne of m_reg * m_reg * string
  | Mblez of m_reg * string
  | Mbltz of m_reg * string
  | Mlui of m_reg * int
  | Mli of m_reg * int
  | Mori of m_reg * m_reg * int
  | Mxori of m_reg * m_reg * int
  | Mslt of  m_reg * m_reg * m_reg
  | Msub of m_reg * m_reg * m_reg
  | Msw of m_reg * int * m_reg
  | Mlw of m_reg * int * m_reg
  | Mmove of m_reg * m_reg
  | Msll of m_reg * m_reg * int
  | Msrl of m_reg * m_reg * int

type m_instr_list = m_instr list
type reg_list = m_reg list
type int_tuple = int * int
type c_list = int_tuple list
type tuple = m_instr_list * c_list

let nr_regs = ref (10)

let reg_to_str (reg : m_reg) : string =
  match reg with
  | ZE -> "$zero"
  | T n -> "$t" ^ string_of_int n
  | S0 -> "$s0"
  | S1 -> "$s1"
  | SP -> "$sp"
  | GP -> "$gp"
  | AT -> "$at"

let rrr rd rs rt = (reg_to_str rd) ^ ", " ^ (reg_to_str rs) ^ ", " ^ (reg_to_str rt)
let rrofs rs rt ofs = (reg_to_str rs) ^ ", " ^ (reg_to_str rt) ^ ", " ^ (string_of_int ofs)
let rrlab rs rt lab = (reg_to_str rs) ^ ", " ^ (reg_to_str rt) ^ ", " ^ lab
let rofsr rt ofs rs = (reg_to_str rt) ^ ", " ^ (string_of_int ofs) ^ "(" ^ (reg_to_str rs) ^ ")"
let rn rt n = (reg_to_str rt) ^ ", " ^ (string_of_int n)
let rr rs rt = (reg_to_str rs) ^ ", " ^ (reg_to_str rt)
let rl rs lab = (reg_to_str rs) ^ ", " ^ lab

let push reg = [Maddi (SP, SP, -4); Msw(reg, 0, SP)]
let pop reg = [Mlw (reg, 0, SP); Maddi (SP, SP, 4)]


let first = function (x, y) -> x;;
let second = function (x, y) -> y;;

let rec cache_var _id reg lst =
  match lst with
  | [] -> (_id, reg) :: []
  | ((a_id, a_reg) :: l) when a_id = _id -> (_id, reg) :: l
  | (a :: l) -> a :: (cache_var _id reg l)


(* replace with option type *)
let rec get_cached _id lst =
  match lst with
  | [] -> 1337
  | ((a_id, a_reg) :: l) when a_id =_id -> a_reg
  | (a :: l) -> (get_cached _id l)


let is_cached _id lst =
  List.exists (fun (a_id, _) -> a_id = _id) lst

(*let li n = [Mlui (T0, n / 65536); Mori (T0, T0, n)] (* 2^16 = 65536 *) *)

let rec write_back_cache lst =
  match lst with
  | [] -> []
  | (id, reg) :: l -> [Msw (T reg, id * 4 , GP)] @ write_back_cache l

let alloc_regs = ref (-1)
let get_reg () =
  if !alloc_regs < !nr_regs - 1 then
    alloc_regs := !alloc_regs + 1;
  !alloc_regs

let count = ref (0)
let newlabel () =
  incr count;
  !count

let branch l = [Mbeq (AT, AT, l)]

let instr_to_str (instr : m_instr) : string =
  match instr with
    | Mlab label          -> label ^ ":"
    | Madd (rd, rs, rt)   -> "add  " ^ (rrr rd rs rt)
    | Maddi (rt, rs, n)   -> "addi " ^ (rrofs rt rs n)
    | Mand (rd, rs, rt)   -> "and  " ^ (rrr rd rs rt)
    | Mandi (rd, rs, n)   -> "andi " ^ (rrofs rd rs n)
    | Mbeq (rs, rt, lab)  -> "beq  " ^ (rrlab rs rt lab)
    | Mbne (rs, rt, lab)  -> "bne  " ^ (rrlab rs rt lab)
    | Mblez (rs, lab)     -> "blez " ^ (rl rs lab)
    | Mbltz (rs, lab)     -> "bltz " ^ (rl rs lab)
    | Mlui (rt, n)        -> "lui  " ^ (rn rt n)
    | Mxori (rt, rs, n)   -> "xori " ^ (rrofs rt rs n)
    | Mli (rt, n)         -> "li   " ^ (rn rt n)
    | Mori (rs, rt, n)    -> "ori  " ^ (rrofs rs rt n)
    | Mslt (rd, rs, rt)   -> "slt  " ^ (rrr rd rs rt)
    | Msub (rd, rs , rt)  -> "sub  " ^ (rrr rd rs rt)
    | Msw (rt, ofs, rs)   -> "sw   " ^ (rofsr rt ofs rs)
    | Mlw (rt, ofs, rs)   -> "lw   " ^ (rofsr rt ofs rs)
    | Mmove (rt, rs)      -> "move " ^ (rr rt rs)
    | Msll (rd, rs, n)    -> "sll  " ^ (rrofs rd rs n)
    | Msrl (rd, rs, n)    -> "srl  " ^ (rrofs rd rs n)



let rec compile_aexpr (exp : aexpr) (reg : int) (cache_list : c_list) : tuple =
  match exp with
    | Anum n       -> [Mli (T reg, to_int n)], cache_list

    | Avar (Id id) -> if is_cached (to_int id) cache_list then
                        [Mmove (T reg, T (get_cached (to_int id) cache_list))], cache_var (to_int id) reg cache_list
                      else
                        [Mlw (T reg, (to_int id)*4, GP)], cache_list (*cache_var (to_int id) reg cache_list*)

    | Aadd (Avar (Id x), Avar (Id y)) ->  let reg_x = get_cached (to_int x) cache_list in (* Need new_reg here? *)
                                          let reg_y = get_cached (to_int y) cache_list in

                                          if (is_cached (to_int x) cache_list && is_cached (to_int y) cache_list) then
                                            [Madd (T reg, T reg_x, T reg_y)], cache_list

                                          else if is_cached (to_int x) cache_list then
                                            let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                              l @ [Madd (T reg, T reg_x, T reg)], c

                                          else if is_cached (to_int y) cache_list then
                                            let (l, c) = compile_aexpr (Avar (Id x)) reg cache_list in
                                              l @ [Madd (T reg, T reg_y, T reg)], c

                                          else
                                            let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                            let (l2, c2) = compile_aexpr (Avar (Id y)) (reg + 1) c1 in
                                              l1 @ l2 @ [Madd (T reg, T reg, T (reg + 1))], c2


    | Aadd (Avar (Id x), a) -> let new_reg = get_reg () in
                               let (l1, c1) = compile_aexpr a new_reg cache_list in
                               let reg_x = get_cached (to_int x) c1 in

                                 if is_cached (to_int x) c1 then
                                   l1 @ [Madd (T reg, T reg_x, T new_reg)], c1

                                 else
                                   let (l2, c2) = (compile_aexpr (Avar (Id x)) (new_reg + 1) c1) in
                                    l1 @ l2 @ [Madd (T reg, T (new_reg + 1), T new_reg)], c2

    | Aadd (a, Avar (Id x)) -> let new_reg = get_reg () in
                               let (l1, c1) = compile_aexpr a new_reg cache_list in
                               let reg_x = get_cached (to_int x) c1 in

                                 if is_cached (to_int x) c1 then
                                   l1 @ [Madd (T reg, T new_reg, T reg_x)], c1

                                 else
                                   let (l2, c2) = (compile_aexpr (Avar (Id x)) (new_reg + 1) c1) in
                                    l1 @ l2 @ [Madd (T reg, T new_reg, T (new_reg + 1))], c2



    | Aadd (a, b)  -> let (l1, c1) = compile_aexpr a reg cache_list in (* Need new_reg here? *)
                      let (l2, c2) = compile_aexpr b (reg + 1) c1 in
                        l1 @ l2 @ [Madd (T reg, T reg, T (reg + 1))], c2

    | Asub (Avar (Id x), Avar (Id y)) -> let reg_x = get_cached (to_int x) cache_list in (* Need new_reg here? *)
                                         let reg_y = get_cached (to_int y) cache_list in

                                           if (is_cached (to_int x) cache_list && is_cached (to_int y) cache_list) then
                                             [Msub (T reg, T reg_x, T reg_y)], cache_list

                                           else if is_cached (to_int x) cache_list then
                                             let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                               l @ [Msub (T reg, T reg_x, T reg)], c

                                           else if is_cached (to_int y) cache_list then
                                             let (l, c) = compile_aexpr (Avar (Id x)) reg cache_list in
                                               l @ [Msub (T reg, T reg, T reg_y)], c

                                           else
                                            let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                            let (l2, c2) = compile_aexpr (Avar (Id y)) (reg + 1) c1 in
                                              l1 @ l2 @ [Msub (T reg, T reg, T (reg + 1))], c2

    | Asub (Avar (Id x), a) -> let new_reg = get_reg () in
                               let (l1, c1) = compile_aexpr a new_reg cache_list in
                               let reg_x = get_cached (to_int x) c1 in
                                 if is_cached (to_int x) c1 then
                                   l1 @ [Msub (T reg, T reg_x, T new_reg)], c1
                                 else
                                   let (l2, c2) = (compile_aexpr (Avar (Id x)) (new_reg + 1) c1) in
                                    l1 @ l2 @ [Msub (T reg, T (new_reg + 1), T new_reg)], c2

    | Asub (a, Avar (Id x)) -> let new_reg = get_reg () in
                               let (l1, c1) = compile_aexpr a new_reg cache_list in
                               let reg_x = get_cached (to_int x) c1 in
                                 if is_cached (to_int x) c1 then
                                   l1 @ [Msub (T reg, T new_reg, T reg_x)], c1
                                 else
                                   let (l2, c2) = (compile_aexpr (Avar (Id x)) (new_reg + 1) c1) in
                                    l1 @ l2 @ [Msub (T reg, T new_reg, T (new_reg + 1))], c2

    | Asub (a, b)  -> let (l1, c1) = compile_aexpr a reg cache_list in (* Need new_reg here? *)
                      let (l2, c2) = compile_aexpr b (reg + 1) c1 in
                        l1 @ l2 @ [Msub (T reg, T reg, T (reg + 1))], c2

    (*currently only works properly for non variable multiplication i.e. a = Anum*Anum *)
    | Amul (a, b)  -> let reg1 = get_reg () in
                      let reg2 = get_reg () in
                      let reg3 = get_reg () in
                      let reg4 = get_reg () in
                      let (l1, c1) = compile_aexpr a reg1 cache_list in
                      let (l2, c2) = compile_aexpr b reg2 c1 in
                      let labelnr = newlabel () in
                      let label_multi = "multi" ^ string_of_int labelnr in
                      let label_bitzero = "bitzero" ^ string_of_int labelnr in
                      let label_zeromulti = "zeromulti" ^ string_of_int labelnr in
                      let label_done = "done" ^ string_of_int labelnr in
                      let z1 = [Mbeq (T reg1, ZE, label_zeromulti)] in
                      let z2 = [Mbeq (T (reg2), ZE, label_zeromulti)] in
                      l1 @ l2 @ z1 @ z2 @ [Mlab label_multi] @ [Mbeq (T (reg2), ZE, label_done )] @
                      [Mandi (T (reg3), T (reg2), 1)] @ [Mbeq (T (reg3), ZE, label_bitzero)] @ [Madd (T (reg4), T (reg4), T (reg1))] @
                      [Mlab label_bitzero] @ [Msrl (T (reg2), T(reg2), 1)] @ [Msll (T reg1, T reg1, 1)] @
                      [Mbeq (ZE, ZE, label_multi)] @ [Mlab label_zeromulti] @ [Mli (T (reg4), 0)] @ [Mlab label_done] @
                      [Maddi (T (reg1-1), T (reg4), 0)], c2


let rec compile_bexpr (exp : bexpr) (reg : int) (label : string) (not_op : bool) (cache_list : c_list) : tuple =
  match exp with
    | Btrue -> if not_op then branch label, cache_list else [], cache_list
    | Bfalse -> if not_op then [], cache_list else branch label, cache_list
    | Bnot b -> compile_bexpr b reg label (not not_op) cache_list


    | Band (b1, b2) -> let labelnr = newlabel () in
                       let _and = "and" ^ string_of_int labelnr in
                       let _dest = if not_op then _and else label in
                       let (cond1, c1) = compile_bexpr b1 reg _dest false cache_list in
                       let (cond2, c2) = compile_bexpr b2 (reg + 1) label not_op c1 in
                         cond1 @ [Mlab _and] @ cond2, c2

    | Beq (Avar (Id x), Avar (Id y)) -> let reg_x = get_cached (to_int x) cache_list in
                                        let reg_y = get_cached (to_int y) cache_list in
                                        if is_cached (to_int x) cache_list && is_cached (to_int y) cache_list then
                                          if not_op then
                                            [Mbeq (T reg_x, T reg_y, label)], cache_list
                                          else
                                            [Mbne (T reg_x, T reg_y, label)], cache_list

                                        else if is_cached (to_int x) cache_list then
                                          let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                          if not_op then
                                            l @ [Mbeq (T reg_x, T reg, label)], c
                                          else
                                            l @ [Mbne (T reg_x, T reg, label)], c

                                        else if is_cached (to_int y) cache_list then
                                          let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                          if not_op then
                                            l @ [Mbeq (T reg_y, T reg, label)], c
                                          else
                                            l @ [Mbne (T reg_y, T reg, label)], c

                                        else
                                          let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                          let (l2, c2) = compile_aexpr (Avar (Id y)) (reg + 1) c1 in
                                          if not_op then
                                            l1 @ l2 @ [Mbeq (T reg, T (reg + 1), label)], c2
                                          else
                                            l1 @ l2 @ [Mbne (T reg, T (reg + 1), label)], c2

    | Beq (Avar (Id x), a) -> let new_reg = get_reg () in
                              let (l1, c1) = compile_aexpr a new_reg cache_list in
                              let reg_x = get_cached (to_int x) c1 in

                                if is_cached (to_int x) c1 then
                                  if not_op then
                                    l1 @ [Mbeq (T reg_x, T new_reg, label)], c1
                                  else
                                    l1 @ [Mbne (T reg_x, T new_reg, label)], c1
                                else

                                  let (l2, c2) = compile_aexpr (Avar (Id x)) (new_reg + 1) c1 in
                                    if not_op then
                                      l1 @ l2 @ [Mbeq (T new_reg, T (new_reg + 1), label)], c2
                                    else
                                      l1 @ l2 @ [Mbne (T new_reg, T (new_reg + 1), label)], c2

    | Beq (a, Avar (Id x)) -> let new_reg = get_reg () in
                              let (l1, c1) = compile_aexpr a new_reg cache_list in
                              let reg_x = get_cached (to_int x) c1 in

                                if is_cached (to_int x) c1 then
                                  if not_op then
                                    l1 @ [Mbeq (T reg_x, T new_reg, label)], c1
                                  else
                                    l1 @ [Mbne (T reg_x, T new_reg, label)], c1

                                else
                                  let (l2, c2) = compile_aexpr (Avar (Id x)) (new_reg + 1) c1 in
                                    if not_op then
                                      l1 @ l2 @ [Mbeq (T new_reg, T (new_reg + 1), label)], c2
                                    else
                                      l1 @ l2 @ [Mbne (T new_reg, T (new_reg + 1), label)], c2

    | Beq (a, b) ->
        let new_reg = get_reg () in
        let (l1, c1) = compile_aexpr a new_reg cache_list in
        let (l2, c2) = compile_aexpr b (new_reg + 1) cache_list in

          if not_op then
            l1 @ l2 @ [Mbeq (T new_reg, T (new_reg + 1), label)], c1
          else
            l1 @ l2 @ [Mbne (T new_reg, T (new_reg + 1), label)], c1

    | Ble (Avar (Id x), Avar (Id y)) -> let reg_x = get_cached (to_int x) cache_list in
                                        let reg_y = get_cached (to_int y) cache_list in

                                        if is_cached (to_int x) cache_list && is_cached (to_int y) cache_list then
                                          if not_op then
                                            [Msub (AT, T reg_x, T reg_y);
                                             Mblez (AT, label)], cache_list
                                          else
                                            [Msub (AT, T reg_y, T reg_x);
                                             Mbltz (AT, label)], cache_list

                                        else if is_cached (to_int x) cache_list then
                                          let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                          if not_op then
                                            l @
                                            [Msub (AT, T reg_x, T reg);
                                             Mblez (AT, label)], c
                                          else
                                            l @
                                            [Msub (AT, T reg, T reg_x);
                                             Mbltz (AT, label)], c

                                        else if is_cached (to_int y) cache_list then
                                          let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                          if not_op then
                                            l @
                                            [Msub (AT, T reg, T reg_y);
                                             Mblez (AT, label)], c
                                          else
                                            l @
                                            [Msub (AT, T reg_y, T reg);
                                             Mbltz (AT, label)], c

                                        else
                                          let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                          let (l2, c2) = compile_aexpr (Avar (Id y)) (reg + 1) c1 in
                                          if not_op then
                                            l1 @ l2 @
                                            [Msub (AT, T reg, T (reg + 1));
                                             Mblez (AT, label)], c2
                                          else
																						l1 @ l2 @ 
                                            [Msub (AT, T (reg + 1), T reg);
                                             Mbltz (AT, label)], c2


    | Ble (Avar (Id x), a) -> let new_reg = get_reg () in
                              let (l1, c1) = compile_aexpr a new_reg cache_list in
                              let reg_x = get_cached (to_int x) c1 in

                                if is_cached (to_int x) c1 then
                                  if not_op then
                                    l1 @ [Msub (AT, T reg_x, T new_reg); Mblez (AT, label)], c1
                                  else
                                    l1 @ [Msub (AT, T new_reg, T reg_x); Mbltz (AT, label)], c1

                                else
                                  let (l2, c2) = compile_aexpr (Avar (Id x)) (new_reg + 1) c1 in
                                    if not_op then
                                      l1 @ l2 @ [Msub (AT, T (new_reg + 1), T new_reg); Mblez (AT, label)], c2
                                    else
                                      l1 @ l2 @ [Msub (AT, T new_reg, T (new_reg + 1)); Mbltz (AT, label)], c2

    | Ble (a, (Avar (Id x))) -> let new_reg = get_reg () in
                                let (l1, c1) = compile_aexpr a new_reg cache_list in
                                let reg_x = get_cached (to_int x) c1 in

                                if is_cached (to_int x) c1 then
                                  if not_op then
                                    l1 @ [Msub (AT, T new_reg, T reg_x); Mblez (AT, label)], c1
                                  else
                                    l1 @ [Msub (AT, T reg_x, T new_reg); Mbltz (AT, label)], c1

                                else
                                  let (l2, c2) = compile_aexpr (Avar (Id x)) (new_reg + 1) c1 in
                                    if not_op then
                                      l1 @ l2 @ [Msub (AT, T new_reg, T (new_reg + 1)); Mblez (AT, label)], c2
                                    else
                                      l1 @ l2 @ [Msub (AT, T (new_reg + 1), T new_reg); Mbltz (AT, label)], c2

    | Ble (a, b) -> let new_reg = get_reg () in
                    let (l1, c1) = compile_aexpr a new_reg cache_list in
                    let (l2, c2) = compile_aexpr b (new_reg + 1) c1 in

                      if not_op then
                        l1 @ l2 @ [Msub (AT, T new_reg, T (new_reg + 1)); Mblez (AT, label)], c2
                      else
                        l1 @ l2 @ [Msub (AT, T (new_reg + 1), T new_reg); Mbltz (AT, label)], c2

let pre_str = "\t.data" ^ nl ^
  "\t.space 1024\t\t# just a placholder" ^ nl ^
  "\t.set noreorder\t\t# necessary to avoid code optimization" ^ nl ^
  "\t.set noat\t\t# necessary to avoid warning accessing $at" ^ nl ^
  "\t.text\t\t\t# .text segment (code)" ^ nl ^
  "\tla $gp, .data\t\t# set gp to accesible data area" ^ nl ^
    "\t\t\t\t# sp initialized to 0 x80000000 on reset" ^ nl ^ nl

let string_of_m_prog p l=
  let rec prog_print = function
  | Mlab label :: ilist -> instr_to_str (Mlab label) ^ nl ^ prog_print ilist
  | instr :: ilist -> "\t" ^ instr_to_str instr ^ nl ^ prog_print ilist
  | [] -> ""
  in pre_str ^ prog_print(p @ write_back_cache l @ Mlab "halt" :: [Mbeq (AT, AT, "halt")])

let rec m_compile_com (cmd : com) (cache_list : c_list) : tuple =
  match cmd with
  | Cskip -> [], cache_list

  | Cassign (Id id, a) -> let cached = is_cached (to_int id) cache_list in
                          let cached_reg = get_cached (to_int id) cache_list in
                            if cached = false then
                              let reg = get_reg () in
                              compile_aexpr a reg (cache_var (to_int id) reg cache_list)
                            else
                              compile_aexpr a cached_reg cache_list

  | Cif (cond, tcmd, fcmd) -> let labelnr = newlabel () in
                              let _else = "else" ^ string_of_int labelnr in
                              let _endif = "endif" ^ string_of_int labelnr in
                              let (true_code, c1) = (m_compile_com tcmd cache_list) in
                              let (false_code, c2) = m_compile_com fcmd c1 in
                              let reg = get_reg () in
                              let (l, c3) = compile_bexpr cond reg _else false c2 in
                                l @ true_code @ branch _endif @ [Mlab _else] @ false_code @ [Mlab _endif], c3

  | Cseq (cmd1, cmd2) -> let (l1, c1) = (m_compile_com cmd1 cache_list) in
                         let (l2, c2) = (m_compile_com cmd2 c1) in
                           l1 @ l2, c2

  | Cwhile (cond, cmd) -> let labelnr = newlabel () in
                          let _while = "while" ^ string_of_int labelnr in
                          let _endwhile = "endwhile" ^ string_of_int labelnr in
                          let reg = get_reg () in
                          let (code, c1) = m_compile_com cmd cache_list in
                          let (w_cond, c2) = compile_bexpr cond reg _endwhile false c1 in
                            [Mlab _while] @ w_cond @ code @ [Mbeq(AT, AT, _while)] @ [Mlab _endwhile], c2





(*
let rec compile_aexpr (exp : aexpr) (reg : int) (cache_list : c_list) : tuple =
  match exp with
    | Anum n       -> [Mli (T reg, to_int n)], cache_list

    | Avar (Id id) -> (*[], cache_list*)if is_cached (to_int id) cache_list then
                        [Mmove (T reg, T (get_cached (to_int id) cache_list))], cache_var (to_int id) reg cache_list
                      else
                        [Mlw (T reg, (to_int id)*4, GP)], cache_list(*cache_var (to_int id) reg cache_list*)

    | Aadd (Avar (Id x), Avar (Id y)) -> let reg_x = get_cached (to_int x) cache_list in
                                         let reg_y = get_cached (to_int y) cache_list in
                                           if (is_cached (to_int x) cache_list && is_cached (to_int y) cache_list) then
                                             [Madd (T reg, T reg_x, T reg_y)], cache_list
                                           else if is_cached (to_int x) cache_list then
                                             let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                               l @ [Madd (T reg, T reg_x, T reg)], c
                                           else if is_cached (to_int y) cache_list then
                                             let (l, c) = compile_aexpr (Avar (Id x)) reg cache_list in
                                               l @ [Madd (T reg, T reg_y, T reg)], c
                                           else
                                            let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                            let (l2, c2) = compile_aexpr (Avar (Id y)) (reg + 1) c1 in
                                            l1 @ l2 @ [Madd (T reg, T reg, T (reg + 1))], c2

    | Aadd (Avar (Id x), Anum n) -> let reg_x = get_cached (to_int x) cache_list in
                                      if is_cached (to_int x) cache_list then
                                        [Maddi (T reg, T reg_x, to_int n)], cache_list
                                      else
                                        let (l, c) = (compile_aexpr (Avar (Id x)) reg cache_list) in
                                          l @ [Maddi (T reg, T reg, to_int n)], c


    | Aadd ((Anum n), Avar (Id x)) -> let reg_x = get_cached (to_int x) cache_list in
                                        if is_cached (to_int x) cache_list then
                                          [Maddi (T reg, T reg_x, to_int n)], cache_list
                                        else
                                         let (l, c) = compile_aexpr (Avar (Id x)) reg cache_list in
                                           l @ [Maddi (T reg, T reg, to_int n)], c

    | Aadd ((Anum n1), (Anum n2)) -> [Mli (T reg, to_int n1)] @ [Mli (T (reg + 1), to_int n2)] @ [Madd (T reg, T reg, T (reg + 1))], cache_list

    | Aadd (a, b)  -> let (l1, c1) = compile_aexpr a reg cache_list in
                      let (l2, c2) = compile_aexpr b (reg + 1) c1 in
                        l1 @ l2 @ [Madd (T reg, T reg, T (reg + 1))], c2

    | Asub (Avar (Id x), Avar (Id y)) -> let reg_x = get_cached (to_int x) cache_list in
                                         let reg_y = get_cached (to_int y) cache_list in
                                           if (is_cached (to_int x) cache_list && is_cached (to_int y) cache_list) then
                                             [Msub (T reg, T reg_x, T reg_y)], cache_list
                                           else if is_cached (to_int x) cache_list then
                                             let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                               l @ [Msub (T reg, T reg_x, T reg)], c
                                           else if is_cached (to_int y) cache_list then
                                             let (l, c) = compile_aexpr (Avar (Id x)) reg cache_list in
                                               l @ [Msub (T reg, T reg, T reg_y)], c
                                           else
                                            let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                            let (l2, c2) = compile_aexpr (Avar (Id y)) (reg + 1) c1 in
                                              l1 @ l2 @ [Msub (T reg, T reg, T (reg + 1))], c2

    | Asub (Avar (Id x), Anum n) -> let reg_x = get_cached (to_int x) cache_list in
                                      if is_cached (to_int x) cache_list then
                                        [Maddi (T reg, T reg_x, -(to_int n))], cache_list
                                      else
                                        let (l, c) = (compile_aexpr (Avar (Id x)) reg cache_list) in
                                          l @ [Maddi (T reg, T reg, -(to_int n))], c


    | Asub ((Anum n), Avar (Id x)) -> let reg_x = get_cached (to_int x) cache_list in
                                        if is_cached (to_int x) cache_list then
                                          [Mli (T reg, to_int n);
                                           Msub (T reg, T reg, T reg_x)], cache_list
                                        else
                                         let (l, c) = compile_aexpr (Avar (Id x)) reg cache_list in
                                           l @ [Mli (T (reg + 1), to_int n);
                                                Msub (T reg, T (reg + 1), T reg)], c

    | Asub ((Anum n1), (Anum n2)) -> [Mli (T reg, to_int n1);
                                      Mli (T (reg + 1), to_int n2);
                                      Msub (T reg, T reg, T (reg + 1))], cache_list

    | Asub (a, b)  -> let (l1, c1) = compile_aexpr a reg cache_list in
                      let (l2, c2) = compile_aexpr b (reg + 1) c1 in
                        l1 @ l2 @ [Msub (T reg, T reg, T (reg + 1))], c2

    | Amul (a, b)  -> raise (CompilerError "multiplication currently not supported") (* TODO *)


let rec compile_bexpr (exp : bexpr) (reg : int) (label : string) (not_op : bool) (cache_list : c_list) : tuple =
  match exp with
    | Btrue -> if not_op then branch label, cache_list else [], cache_list
    | Bfalse -> if not_op then [], cache_list else branch label, cache_list
    | Bnot b -> compile_bexpr b reg label (not not_op) cache_list

    | Band (b1, b2) ->
        let labelnr = newlabel () in
        let _and = "and" ^ string_of_int labelnr in
        let _dest = if not_op then _and else label in
        let (cond1, c1) = compile_bexpr b1 reg _dest false cache_list in
        let (cond2, c2) = compile_bexpr b2 reg label not_op c1 in
          cond1 @ [Mlab _and] @ cond2, c2

    | Beq (a, b) ->
        let (l1, c1) = compile_aexpr a reg cache_list in
        let (l2, c2) = compile_aexpr b reg c1 in
          if not_op then
            l1 @ l2 @ [Mbeq (T reg, T (reg + 1), label)], c2
          else
            l1 @ l2 @ [Mbne (T reg, T (reg + 1), label)], c2

    | Ble (Avar (Id x), Avar (Id y)) -> let reg_x = get_cached (to_int x) cache_list in
                                   let reg_y = get_cached (to_int y) cache_list in
                                     if is_cached (to_int x) cache_list && is_cached (to_int y) cache_list then
                                      if not_op then
                                        [Msub (AT, T reg_x, T reg_y);
                                         Mblez (AT, label)], cache_list
                                      else
                                        [Msub (AT, T reg_y, T reg_x);
                                         Mblez (AT, label)], cache_list
                                     else if is_cached (to_int x) cache_list then
                                       let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                         if not_op then
                                           l @
                                           [Msub (AT, T reg_x, T reg);
                                            Mblez (AT, label)], c
                                          else
                                            l @
                                            [Msub (AT, T reg, T reg_x);
                                             Mblez (AT, label)], c
                                     else if is_cached (to_int y) cache_list then
                                       let (l, c) = compile_aexpr (Avar (Id y)) reg cache_list in
                                         if not_op then
                                           l @
                                           [Msub (AT, T reg, T reg_y);
                                            Mblez (AT, label)], c
                                         else
                                           l @
                                           [Msub (AT, T reg_y, T reg);
                                            Mblez (AT, label)], c
                                     else
                                       let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                       let (l2, c2) = compile_aexpr (Avar (Id y)) (reg + 1) c1 in
                                         if not_op then
                                           l1 @ l2 @
                                           [Msub (AT, T reg, T (reg + 1));
                                            Mblez (AT, label)], c2
                                         else
                                           [Msub (AT, T (reg + 1), T reg);
                                            Mblez (AT, label)], c2

    | Ble (Avar (Id x), Anum n) -> let reg_x = get_cached (to_int x) cache_list in
                                     if is_cached (to_int x) cache_list then
                                       if not_op then
                                         [Mli (T reg, (to_int n));
                                          Msub (AT, T reg_x, T reg);
                                          Mblez (AT, label)], cache_list
                                       else
                                         [Mli (T reg, (to_int n));
                                          Msub (AT, T reg, T reg_x);
                                          Mbltz (AT, label)], cache_list
                                     else
                                       if not_op then
                                         let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                         let (l2, c2) = compile_aexpr (Anum n) (reg + 1) c1 in
                                           l1 @ l2 @
                                           [Msub (AT, T reg, T (reg + 1));
                                            Mblez (AT, label)], c2
                                       else
                                         let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                         let (l2, c2) = compile_aexpr (Anum n) (reg + 1) c1 in
                                           l1 @ l2 @
                                             [Msub (AT, T (reg + 1), T reg);
                                              Mblez (AT, label)], c2

    | Ble (Anum n, Avar (Id x)) -> let reg_x = get_cached (to_int x) cache_list in
                                     if is_cached (to_int x) cache_list then
                                       if not_op then
                                         [Mli (T reg, (to_int n));
                                          Msub (AT, T reg, T reg_x);
                                          Mblez (AT, label)], cache_list
                                       else
                                         [Mli (T reg, (to_int n));
                                          Msub (AT, T reg_x, T reg);
                                          Mbltz (AT, label)], cache_list
                                     else
                                       if not_op then
                                         let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                         let (l2, c2) = compile_aexpr (Anum n) (reg + 1) c1 in
                                           l1 @ l2 @
                                             [Msub (AT, T (reg + 1), T reg);
                                              Mblez (AT, label)], c2
                                       else
                                         let (l1, c1) = compile_aexpr (Avar (Id x)) reg cache_list in
                                         let (l2, c2) = compile_aexpr (Anum n) (reg + 1) c1 in
                                           l1 @ l2 @
                                           [Msub (AT, T reg, T (reg + 1));
                                            Mblez (AT, label)], c2

    | Ble (Anum n1, Anum n2) -> if not_op then
                                  [Mli (T reg, to_int n1);
                                   Mli (T (reg + 1), to_int n2);
                                   Msub (AT, T reg, T (reg + 1));
                                   Mblez (AT, label)], cache_list
                                else
                                  [Mli (T reg, to_int n1);
                                   Mli (T (reg + 1), to_int n2);
                                   Msub (AT, T (reg + 1), T reg);
                                   Mblez (AT, label)], cache_list

    (*| Ble (Avar (Id x), b) -> let (l1, c1) = compile_aexpr b reg cache_list in
                              let reg_x = get_cached (to_int x) c1 in
                                if is_cached (to_int x) c1 then
                                  if not_op then
                                    l1 @ [Msub (AT, T reg_x, T reg); Mblez (AT, label)], c1
                                  else
                                    l1 @ [Msub (AT, T reg, T reg_x); Mblez (AT, label)], c1
                                else
                                  let (l2, c2) = compile_aexpr (Avar (Id x)) (reg + 1) c1 in
                                    if not_op then
                                      l1 @ l2 @ [Msub (AT, T (reg + 1), T reg); Mblez (AT, label)], c2
                                    else
                                      l1 @ l2 @ [Msub (AT, T reg, T (reg + 1)); Mblez (AT, label)], c2*)

    | Ble (a, b) -> (*let new_reg = get_reg () in*)
                    let (l1, c1) = compile_aexpr a reg cache_list in
                    let (l2, c2) = compile_aexpr b (reg + 1) c1 in
                      if not_op then
                        l1 @ l2 @ [Msub (AT, T reg, T (reg + 1)); Mblez (AT, label)], c2
                      else
                        l1 @ l2 @ [Msub (AT, T (reg + 1), T reg); Mbltz (AT, label)], c2
*)
