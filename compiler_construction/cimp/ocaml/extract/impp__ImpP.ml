(* This file has been generated from Why3 module ImpP *)

open Why3extract

type fmla =
  | Fexpr of Imp__Imp.bexpr
  | Fand of fmla * fmla
  | Fnot of fmla
  | Fimplies of fmla * fmla
  | Fforall of State__StateGen.id * fmla
  | Fexists of State__StateGen.id * fmla

type body =
  | Bseq of body * body
  | Com of Imp__Imp.com
  | Assert of fmla

type comp = fmla * body * fmla


