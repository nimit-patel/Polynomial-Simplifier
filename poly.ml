open Core
open Expr

exception Unrecognized_pExpr
exception InvalidArgumentsForDistributive of string

type pExp =
  | Term of int*int*char
  | Plus of pExp list
  | Times of pExp list

let rec nlist (_l: pExp list) (_n: int) (_pe: pExp) : pExp list =
  match _n with
  | 0 -> _l
  | _ -> nlist (_l@[_pe]) (_n - 1) _pe

let rec from_expr (_e: Expr.expr) : pExp =
    match _e with
    | Num(c)          -> Term (c, 0, '~')
    | Var(x)          -> Term (1, 1, x)
    | Add(e1, e2)     -> Plus ([ from_expr e1; from_expr e2 ])
    | Sub(e1, e2)     -> Plus ([ from_expr e1; from_expr (Neg(e2))])
    | Mul(e1, e2)     -> Times([ from_expr e1; from_expr e2 ])
    | Pow(base, exp)  -> Times((nlist [] exp (from_expr(base))))
    | Pos(e1)         -> from_expr e1
    | Neg(e1)         -> Times([Term(-1, 0, '~'); from_expr e1])

let rec degree (_e: pExp): int =
  match _e with
  | Term  (n,m,_) -> if n = 0 then 0 else m
  | Plus  (l)     -> List.fold ~init:0 ~f:(fun acc e -> max acc (degree e)) l
  | Times (l)     -> List.fold ~init:0 ~f:(fun acc e -> acc + degree e    ) l

let compareDeg (e1: pExp) (e2: pExp) : int =
  compare (degree e1) (degree e2)

let compareVarNamePlus (e1: pExp) (e2: pExp) : int =
  match e1, e2 with
  | Term(_,_,c1), Term(_,_,c2) -> -Char.compare c1 c2
  | _ -> 1

let compareVarNameTimes (e1: pExp) (e2: pExp) : int =
  match e1, e2 with
  | Term(_,_,c1), Term(_,_,c2) -> Char.compare c1 c2
  | _ -> 1

let rec raw_str_pExpr (_e: pExp): string = 
  match _e with
  | Term (a, b, c) -> "Term("  ^ (string_of_int a) ^ "," ^ (string_of_int b) ^ "," ^ String.make 1 c ^ ")"
  | Plus (_h::_l)  -> "Plus("  ^ List.fold ~init:(raw_str_pExpr _h) ~f:(fun acc term -> acc ^ "," ^ raw_str_pExpr(term)) _l ^ ")"
  | Times(_h::_l)  -> "Times(" ^ List.fold ~init:(raw_str_pExpr _h) ~f:(fun acc term -> acc ^ "," ^ raw_str_pExpr(term)) _l ^ ")"
  | _ -> raise Unrecognized_pExpr

let str_pExpr_Term (a: int) (b: int) (c: char) : string =
  match a, b, c with
  | 0 , 0, _ -> "0"
  | 0 , _, _ -> ""
  | _ , 0, _ -> string_of_int a
  | 1 , 1, c -> String.make 1 c
  | -1, 1, c -> "-" ^ String.make 1 c
  | _ , 1, c -> string_of_int a ^ String.make 1 c
  | 1 , _, c -> String.make 1 c ^ "^" ^ string_of_int b
  | -1, _, c -> "-" ^ String.make 1 c ^ "^" ^ string_of_int b
  | _ , _, c -> string_of_int a ^ String.make 1 c ^ "^" ^ string_of_int b

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
  | Term(a,b,c)   -> str_pExpr_Term a b c
  | Plus (_h::_l) -> "(" ^ List.fold ~init:(str_pExpr _h) ~f:str_pExpr_plus  _l ^ ")"
  | Times(_h::_l) -> "(" ^ List.fold ~init:(str_pExpr _h) ~f:str_pExpr_times _l ^ ")"
  | _ -> raise Unrecognized_pExpr

and str_pExpr_plus (acc: string) (e: pExp) : string =
  acc ^ match e with
  | Term(0,n,_) -> ""
  | Term(m,n,_) when m < 0 -> " - " ^ strip_first_char (str_pExpr e)
  | _ -> " + " ^ str_pExpr e

and str_pExpr_times (acc: string) (e: pExp) : string =
  acc ^ match e with
  | Term(0,n,_) -> ""
  | Term(m,n,_) when m < 0 -> "(" ^ str_pExpr e ^ ")"
  | _ -> str_pExpr e

let rec print_pExp (_e: pExp): unit =
  match _e with
  | Term(0,_,_)  -> print_string "0";
  | _ -> print_string (strip_root_parenthesis (str_pExpr _e))

let accumulatePlus (acc: pExp list) (e: pExp) : pExp list =
  match acc with
  | hd::tl -> (
    match hd, e with
    | Term(m1,n1,c1), Term(m2,n2,c2) when n1 = n2           && c1 = c2 -> [Term(m1+m2, n1, c1)]@tl
    | Term(m1,n1,c1), Term(m2,n2,c2) when m1 = 0  && m2 = 0 && c1 = c2 -> [Term(0, 0, '~')]@tl
    | Term(0,_,_), e | e, Term(0,_,_)                                  -> [e]@tl
    | _ -> [e]@acc 
  )
  | [] -> [e]

let accumulateTimes (acc: pExp list) (e: pExp) : pExp list =
  match acc with
  | hd::tl -> (
    match hd, e with
    | Term(m1,0,'~'), Term(m2,n,c) | Term(m1,n,c), Term(m2,0,'~') -> [Term(m1*m2,n,c)]@tl
    | Term(m1,n1,c1), Term(m2,n2,c2) when c1 = c2                 -> [Term(m1*m2,n1+n2,c1)]@tl
    | _ -> [e]@acc
  )
  | [] -> [e]

let rec _distribute (_e1: pExp) (_e2: pExp) : pExp list =
  match _e1, _e2 with
  | Term(_,_,_), Plus(l) -> [Plus((List.fold ~init:[] ~f:(fun a e -> [Times([_e1; e])         ]@a) l ))]
  | Plus(l), Term(_,_,_) -> [Plus((List.fold ~init:[] ~f:(fun a e -> [Times([_e2; e])         ]@a) l ))]
  | Plus(l1), Plus(l2)   -> [Plus((List.fold ~init:[] ~f:(fun a e -> [Times(_distribute _e1 e)]@a) l2))]
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
      let l = List.stable_sort compareDeg l in
      List.stable_sort compareVarNamePlus l |>
      List.fold ~init:[] ~f:flatPlus        |>
      List.fold ~init:[] ~f:accumulatePlus  |>
      Plus
    )
  )
  | Times(l) -> ( 
    match l with 
    | l::[] -> l
    | _ -> (
      let l = List.stable_sort compareDeg l in
      List.stable_sort compareVarNameTimes l |>
      List.fold ~init:[] ~f:flatTimes        |>
      List.fold ~init:[] ~f:accumulateTimes  |>
      List.fold ~init:[] ~f:distribute       |>
      Times
    )
  )
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

let rec equal_pExp (_e1: pExp) (_e2: pExp) : bool =
  match _e1, _e2 with
  | Times(l1), Times(l2) | Plus(l1), Plus(l2) -> equal_pExp_l l1 l2
  | Term(m1,n1,c1), Term(m2,n2,c2) -> m1 = m2 && n1 = n2 && c1 = c2
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
  | Term(m, n, c) -> m * Expr.pow v n
  | _ -> 0

let rec simplify (e:pExp): pExp =
  (*print_string ((raw_str_pExpr e) ^ "\n");
  print_pExp e; print_string "\n";*)
  let rE = simplify1(e) in
    if (equal_pExp e rE) then
      e
    else begin
      simplify(rE)
    end