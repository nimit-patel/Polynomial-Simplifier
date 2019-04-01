open Lexing
open Parser
open Lexer
open Poly
open Expr

exception NotMatchingDegree
exception NotMatchingPolynomials

let filename = Sys.argv.(1)

let rec _checkPoly (exp: expr) (pexp: pExp) (v: int) : pExp =
  if v = 0 
    then pexp 
  else if (eval_expr exp v) = (eval_pExp pexp v)
    then _checkPoly exp pexp (v-1)
  else
    raise NotMatchingPolynomials

let checkPoly (exp: expr) (pexp: pExp) : pExp =
  match degree_expr exp, degree pexp with
  | d1, d2 -> _checkPoly exp pexp (max d1 d2)
  | _ -> raise NotMatchingDegree

let () =
    let file = open_in filename in
    let t = Lexing.from_channel file in
    let exp = Parser.main Lexer.token t in
    let pexp = from_expr exp in 
    let pexp = simplify pexp in
    checkPoly exp pexp |>
    print_pExp;