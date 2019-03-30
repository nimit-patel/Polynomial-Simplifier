open Core

exception Unrecognized_pExpr

type pExp =
  | Term of int*int
  | Plus of pExp list
  | Times of pExp list

let rec nlist (_l: pExp list) (_n: int) (_pe: pExp) : pExp list =
  match _n with
  | 0 -> _l
  | _ -> nlist (_l@[_pe]) (_n - 1) _pe

let rec from_expr (_e: Expr.expr) : pExp =
    match _e with
    | Num(c) -> Term(c, 0)
    | Var(x) -> Term(1, 1)
    | Add(e1, e2) -> Plus( [ from_expr e1; from_expr e2 ] )
    | Sub(e1, e2) -> Plus( [ from_expr e1; from_expr (Neg(e2)) ] )
    | Mul(e1, e2) -> Times( [ from_expr e1; from_expr e2 ] )
    | Pow(base, exp) -> Times((nlist [] exp (from_expr(base))))
    | Pos(e1) -> from_expr e1
    | Neg(e1) -> Times([Term(-1,0); from_expr e1])

let rec degree (_e: pExp): int =
  match _e with
  | Term(n,m) -> m
  | Plus(l) -> List.fold ~init:0 ~f:(fun acc e -> max acc (degree e)) l
  | Times(l) -> List.fold ~init:0 ~f:(fun acc e -> acc + degree e) l

let compare (e1: pExp) (e2: pExp) : int =
  degree e1 - degree e2

let rec raw_str_pExpr (_e: pExp): string = 
  match _e with
  | Term(a, b) -> "Term(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^ ")"
  | Plus(_h::_l) -> "Plus(" ^ List.fold ~init:(raw_str_pExpr _h) ~f:(fun acc term -> acc ^ "," ^ raw_str_pExpr(term)) _l ^ ")"
  | Times(_h::_l) -> "Times(" ^ List.fold ~init:(raw_str_pExpr _h) ~f:(fun acc term -> acc ^ "," ^ raw_str_pExpr(term)) _l ^ ")"
  | _ -> raise Unrecognized_pExpr

let str_pExpr_Term (a: int) (b: int) : string =
  match a, b with
  | 0, _ -> "0"
  | _, 0 -> string_of_int a
  | 1, 1 -> "x"
  | -1, 1 -> "-x"
  | 1, _ -> "x^" ^ string_of_int b
  | -1, _ -> "-x^" ^ string_of_int b
  | _, _ -> string_of_int a ^ "x^" ^ string_of_int b
  
let rec str_pExpr (_e: pExp): string = 
  match _e with
  | Term(a, b) -> str_pExpr_Term a b
  | Plus(_h::_l) -> "(" ^ List.fold ~init:(str_pExpr _h) ~f:(fun acc term -> acc ^ " + " ^ str_pExpr(term)) _l ^ ")"
  | Times(_h::_l) -> "(" ^ List.fold ~init:(str_pExpr _h) ~f:(fun acc term -> acc ^ " * " ^ str_pExpr(term)) _l ^ ")"
  | _ -> raise Unrecognized_pExpr

let rec print_pExp (_e: pExp): unit =
  print_string (str_pExpr _e)


(* 
  Function to simplify (one pass) pExpr

  n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
  Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

  Hint 1: Keep terms in Plus[...] sorted
  Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
  Hint 3: flatten times, i.e. times of times is times
  Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
          Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
  Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
    i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
      => Plus[Times[Term(11); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
      => Plus[Term(2,3); Term(6,5)]
  Hint 6: Find other situations that can arise
*)

let rec _accumulateTerms (ct: pExp) (l: pExp list) : pExp list =
  match l with
  | hd::tl -> (
    match hd with
    | Term(m,n) -> 
  )
  | _ -> []

let rec accumulateTerms (l: pExp list) : pExp list =
  match l with
  | hd::tl -> (
    match hd with
    | Term(c, e) -> _accumulateTerms Term(c, e) tl
    | _ -> [hd]@(accumulateTerms tl)
  )
  | _ -> []


let rec flatPlus (acc: pExp list) (e: pExp) : pExp list =
  acc @ (
    match e with
    | Plus(l) -> List.map ~f:simplify1 l
    | _       -> [simplify1 e]
  )

and flatTimes (acc: pExp list) (e: pExp) : pExp list =
acc @ (
  match e with
  | Times(l) -> List.map ~f:simplify1 l
  | _       -> [simplify1 e]
)

and simplify1 (e:pExp): pExp =
  match e with
  | Plus(l)  -> (
    let l = List.fold ~init:[] ~f:flatPlus (List.sort l compare) in
    Plus( accumulateTerms l )
  )
  | Times(l) -> Times(List.fold ~init:[] ~f:flatTimes l)
  | _ -> e

(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (_e1: pExp) (_e2: pExp) :bool =
  true

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
    let rE = simplify1(e) in
      print_pExp rE;
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE)
