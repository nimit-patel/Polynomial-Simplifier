open Lexing
open Parser
open Lexer
open Poly
open Expr

exception Dummy
exception NotMatchingDegree
exception NotMatchingPolynomials

let rec _checkPoly (exp: expr) (pexp: pExp) (v: int) : pExp =
  if v = 0 
    then pexp 
  else 
    try if (eval_expr exp v) = (eval_pExp pexp v)
          then _checkPoly exp pexp (v-1)
        else
          raise NotMatchingPolynomials
    with Division_by_zero -> _checkRand exp pexp v
    
and _checkRand (exp: expr) (pexp: pExp) (resume: int) : pExp =
  let d = degree pexp in 
  let r = (Random.int d) + (d) + 1 in
  try if (eval_expr exp r) = (eval_pExp pexp r)
          then _checkPoly exp pexp (resume - 1)
      else
        raise NotMatchingPolynomials
  with Division_by_zero -> _checkRand exp pexp resume

let checkPoly (exp: expr) (pexp: pExp) : pExp =
  match degree_expr exp, degree pexp with
  | d1, d2 when d1 = d2 -> _checkPoly exp pexp (d1+1)
  | _ -> raise NotMatchingDegree

let filename = Sys.argv.(1)

let () =
  let file = open_in filename in
  let t = Lexing.from_channel file in
  let exp = Parser.main Lexer.token t in
  let exp = print_expr exp in
  let pexp = from_expr exp in 
  let pexp = simplify pexp in
  checkPoly exp pexp |>
  print_pExp; 