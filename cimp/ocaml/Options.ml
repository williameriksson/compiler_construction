(* Copyright Per Lindgren 2016, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* cimp/Options *)

open Common

type informat = IMP | MLW
let format_to_string = function
  | IMP -> "imp"
  | MLW -> "mlw"

type options = 
  {
   mutable infile:      string;
   mutable infmat:      informat;
   mutable outfile:     string;
   mutable verbose:     bool;
   mutable debug:       bool;
   mutable d_ast:       bool;
   mutable d_past:      bool;
   mutable d_code:      bool;
   mutable d_pcode:     bool;
   mutable imp_ex:      int;
   mutable vm_ex:       int;
	 mutable optim:       bool;
  }
  
let opt = 
  {
   infile    = "";
   infmat    = IMP;
   outfile   = "";
   verbose   = false;
   debug     = false;
   d_ast     = false;
   d_past    = false;
   d_code    = false;
   d_pcode   = false;
   imp_ex    = 0;
   vm_ex     = 0;
	 optim     = false;
  } 
        
let string_of_opt opt = 
  "cimp options:" ^ nl ^  
  "infile       : " ^ opt.infile ^ nl ^
  "informat     : " ^ format_to_string opt.infmat ^ nl ^ 
  "outfile      : " ^ opt.outfile ^ nl ^
  "verbose      : " ^ string_of_bool opt.verbose ^ nl ^
  "debug        : " ^ string_of_bool opt.debug ^ nl ^
	"optimization : " ^ string_of_bool opt.optim ^ nl ^
  "d_ast        : " ^ string_of_bool opt.d_ast ^ nl ^
  "d_past       : " ^ string_of_bool opt.d_past ^ nl ^
  "d_code       : " ^ string_of_bool opt.d_code ^ nl ^
  "d_pcode      : " ^ string_of_bool opt.d_pcode ^ nl ^
  "imp_ex       : " ^ string_of_int  opt.imp_ex ^ " steps" ^ nl ^
  "vm_ex        : " ^ string_of_int  opt.vm_ex ^ " steps" ^ nl
  