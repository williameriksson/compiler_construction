(* Copyright Per Lindgren 2016, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* cimp/Env *)

(* Associative list for identifiers *)
open Why3extract.Why3__BigInt
open Listmap__ListMap
open State__StateEx
open State__StateGen
open Common

type id_list  = (string * t) list 
type id_list' = (t * string) list 

let id_nr = ref 0
let id_map  = ref ([]:id_list)
let id_map' = ref ([]:id_list')

let add_id (id:string) : t =
  try
    List.assoc id !id_map 
  with 
  | _ -> 
    id_nr := !id_nr + 1; 
    id_map := (id, of_int !id_nr) :: !id_map;
    id_map' := (of_int !id_nr, id) :: !id_map';
    of_int !id_nr

let get_id (id_nr : t) : string =
  List.assoc id_nr !id_map'  


    
let string_of_map (m : state_ex) = 
  let rec str = function
  | [] -> ""
  | (s, n) :: l' -> str l' ^ 
    "Id [" ^ s ^ ", #" ^ to_string n ^ "] = " ^ to_string (get m (Id n)) ^ nl     
  in
  str !id_map
  

