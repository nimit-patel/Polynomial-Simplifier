open Core
open Expr

exception Unrecognized_pExpr
exception InvalidArgumentsForDistributive of string

type pExp =
  | Term of int*int
  | Plus of pExp list
  | Fraction of pExp * pExp
  | Times of pExp list

let rec nlist (_l: pExp list) (_n: int) (_pe: pExp) : pExp list =
  match _n with
  | 0 -> _l
  | _ -> nlist (_l@[_pe]) (_n - 1) _pe

let rec from_expr (_e: Expr.expr) : pExp =
    match _e with
    | Num(c)          -> Term (c, 0)
    | Var(x)          -> Term (1, 1)
    | Add(e1, e2)     -> Plus ([ from_expr e1; from_expr e2 ])
    | Sub(e1, e2)     -> Plus ([ from_expr e1; from_expr (Neg(e2))])
    | Mul(e1, e2)     -> Times([ from_expr e1; from_expr e2 ])
    | Div(num, denom) -> Fraction (from_expr num, from_expr denom)
    | Pow(base, exp)  -> Times((nlist [] exp (from_expr(base))))
    | Pos(e1)         -> from_expr e1
    | Neg(e1)         -> Times([Term(-1,0); from_expr e1])

let rec degree (_e: pExp): int =
  match _e with
  | Term  (n,m)    -> m
  | Plus  (l)      -> List.fold ~init:0 ~f:(fun acc e -> max acc (degree e)) l
  | Times (l)      -> List.fold ~init:0 ~f:(fun acc e -> acc + degree e    ) l
  | Fraction (n,d) -> (degree n) - (degree d)

let compareDeg (e1: pExp) (e2: pExp) : int =
  degree e1 - degree e2

let rec raw_str_pExpr (_e: pExp): string = 
  match _e with
  | Term    (a, b  )  -> "Term("     ^ (string_of_int a) ^ ", " ^ (string_of_int b)                                                ^ ")"
  | Plus    (_h::_l)  -> "Plus("     ^ List.fold ~init:(raw_str_pExpr _h) ~f:(fun acc term -> acc ^ ", " ^ raw_str_pExpr(term)) _l ^ ")"
  | Times   (_h::_l)  -> "Times("    ^ List.fold ~init:(raw_str_pExpr _h) ~f:(fun acc term -> acc ^ ", " ^ raw_str_pExpr(term)) _l ^ ")"
  | Fraction(n, d  )  -> "Fraction(" ^ raw_str_pExpr n ^ ", "  ^ raw_str_pExpr d                                                   ^ ")"
  | _ -> raise Unrecognized_pExpr

let str_pExpr_Term (a: int) (b: int) : string =
  match a, b with
  | 0 , 0 -> "0"
  | 0 , _ -> ""
  | _ , 0 -> string_of_int a
  | 1 , 1 -> "x"
  | -1, 1 -> "-x"
  | _ , 1 -> string_of_int a ^ "x"
  | 1 , _ -> "x^" ^ string_of_int b
  | -1, _ -> "-x^" ^ string_of_int b
  | _ , _ -> string_of_int a ^ "x^" ^ string_of_int b
  
let strip_first_char str =
  if str = "" then "" else String.sub str 1 ((String.length str) - 1)

let strip_root_parenthesis str =
  match String.length str with
  | 0 | 1 | 2 -> str
  | len -> (
    match str.[0] with
    | '(' -> String.sub str 1 (len - 2)
    | _ -> str
  )

let rec str_pExpr (_e: pExp): string = 
  match _e with
  | Term    (a, b  ) -> str_pExpr_Term a b
  | Plus    (_h::_l) -> "(" ^ List.fold ~init:(str_pExpr _h) ~f:str_pExpr_plus  _l ^ ")"
  | Times   (_h::_l) -> "(" ^ List.fold ~init:(str_pExpr _h) ~f:str_pExpr_times _l ^ ")"
  | Fraction(n, d  ) -> "(" ^ str_pExpr n ^ "/" ^ str_pExpr d ^ ")"
  | _ -> raise Unrecognized_pExpr

and str_pExpr_plus (acc: string) (e: pExp) : string =
  acc ^ match e with
  | Term(m,n) when compare m 0 = 0 -> ""
  | Term(m,n) when m < 0 -> " - " ^ strip_first_char (str_pExpr e)
  | _ -> " + " ^ str_pExpr e

and str_pExpr_times (acc: string) (e: pExp) : string =
  acc ^ match e with
  | Term(m,n) when compare m 0 = 0 -> ""
  | Term(m,n) when m < 0 -> "(" ^ str_pExpr e ^ ")"
  | _ -> " * " ^ str_pExpr e

let rec print_pExp (_e: pExp): unit =
  print_string (strip_root_parenthesis (str_pExpr _e))

let accumulatePlus (acc: pExp list) (e: pExp) : pExp list =
  match acc with
  | hd::tl -> (
    match hd, e with
    | Term(m1,n1)   , Term(m2,n2) when compare n1 n2 = 0 -> [Term(m1+m2,n1)                                          ]@tl
    | Fraction(a,b) , Fraction(c,d)                      -> [Fraction(Plus([Times([a;d]);Times([c;b])]),Times([b;d]))]@tl
    | a , Fraction(c,d)             | Fraction(c,d) , a  -> [Fraction(Plus([Times([a;d]);c           ]),           d)]@tl
    | _ -> [e]@acc
  )
  | [] -> [e]

let accumulateTimes (acc: pExp list) (e: pExp) : pExp list =
  match acc with
  | hd::tl -> (
    match hd, e with
    | Term(m1,n1), Term(m2,n2)                           -> [Term(m1*m2,n1+n2)                  ]@tl
    | Fraction(a,b) , Fraction(c,d)                      -> [Fraction(Times([a;c]),Times([b;d]))]@tl
    | a , Fraction(c,d)             | Fraction(c,d) , a  -> [Fraction(Times([a;c]),d)           ]@tl
    | _                                                  -> [e]@acc
  )
  | [] -> [e]

let rec _distribute (_e1: pExp) (_e2: pExp) : pExp list =
  match _e1, _e2 with
  | Term(_,_), Plus(l) -> [Plus((List.fold ~init:[] ~f:(fun a e -> [Times([_e1; e])         ]@a) l ))]
  | Plus(l), Term(_,_) -> [Plus((List.fold ~init:[] ~f:(fun a e -> [Times([_e2; e])         ]@a) l ))]
  | Plus(l1), Plus(l2) -> [Plus((List.fold ~init:[] ~f:(fun a e -> [Times(_distribute _e1 e)]@a) l2))]
  | _,_ -> [_e2; _e1]

let distribute (acc: pExp list) (e: pExp) : pExp list =
  match acc with
  | hd::tl -> (_distribute hd e)@tl
  | [] -> [e]

let rec simplify1 (e:pExp): pExp =
  match e with
  | Plus(l) -> (
    match l with 
    | l::[] -> l
    | _ -> (
      List.stable_sort compareDeg l         |>
      List.fold ~init:[] ~f:flatPlus        |>
      List.fold ~init:[] ~f:accumulatePlus  |>
      Plus
    )
  )
  | Times(l) -> ( 
    match l with 
    | l::[] -> l
    | _ -> (
      List.stable_sort compareDeg l         |>
      List.fold ~init:[] ~f:flatTimes       |>
      List.fold ~init:[] ~f:accumulateTimes |>
      List.fold ~init:[] ~f:distribute      |>
      Times
    )
  )
  | Fraction(n, d) -> handleFractions n d
  | _ -> e

and flatPlus (acc: pExp list) (e: pExp) : pExp list =
  acc @ (
    match e with
    | Plus(l) -> List.map ~f:simplify1 l
    | _       -> [simplify1 e]
  )

and flatTimes (acc: pExp list) (e: pExp) : pExp list =
  acc @ (
    match e with
    | Times(l) -> List.map ~f:simplify1 l
    | _        -> [simplify1 e]
  )

and handleFractions (n: pExp) (d: pExp) : pExp =
  match simplify1(n), simplify1(d) with
  | n           , Term(dc, dd) when dc = 1 && dd = 0    -> n                                                                 (* ax^n / 1                => ax^n           *)
  | Term(nc, nd), Term(dc, dd) when nc mod dc = 0       -> Fraction(Term(nc/dc, nd), Term(1,dd))                             (* ax^n / bx^m where a | b => (a/b)x^n / x^m *)
  | Term(nc, nd), Term(dc, dd) when nc = dc && nd = dd  -> Term(1,0)                                                         (* ax^n / ax^n = 1 *)
  | Term(nc, nd), Term(dc, dd) when nd = dd             -> Fraction(Term(nc,0), Term(dc,0))                                  (* ax^n / bx^n = a/b *)
  | Term(nc, nd), Term(dc, dd) when nd >= dd            -> Times([handleFractions (Term(nc,0)) (Term(dc,0)); Term(1,nd-dd)]) (* ax^n / bx^m = (a/b)x^(n-m) *)
  | n, d -> Fraction(n, d)

let rec equal_pExp (_e1: pExp) (_e2: pExp) : bool =
  match _e1, _e2 with
  | Times(l1), Times(l2) | Plus(l1), Plus(l2) -> equal_pExp_l l1 l2
  | Fraction(d1, n1), Fraction(d2, n2) -> equal_pExp d1 d2 && equal_pExp n1 n2
  | Term(m1,n1), Term(m2,n2) -> (compare m1 m2) = 0 && (compare n1 n2) = 0
  | _,_ -> false

and equal_pExp_l (_l1: pExp list) (_l2: pExp list) : bool =
  match _l1, _l2 with
  | [],[] -> true (* two empty lists are equal *)
  | hd1::tl1, hd2::tl2 -> (
    (equal_pExp hd1 hd2) && (equal_pExp_l tl1 tl2)
  )
  | _ -> false (* takes care of distinct lenghts *)

let rec eval_pExp (e: pExp) ~v:(v: int) : int =
  match e with
  | Plus(hd::tl)  -> List.fold ~init:(eval_pExp hd v) ~f:(fun t e -> t + eval_pExp e v) tl
  | Times(hd::tl) -> List.fold ~init:(eval_pExp hd v) ~f:(fun t e -> t * eval_pExp e v) tl
  | Fraction(n,d) -> eval_pExp n v / eval_pExp d v (* TODO flawed calc *)
  | Term(m, n)    -> m * Expr.pow v n
  | _ -> 0

let rec simplify (e:pExp): pExp =
  let rE = simplify1(e) in
    if (equal_pExp e rE) then
      e
    else
      simplify(rE)
