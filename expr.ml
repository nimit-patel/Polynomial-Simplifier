type expr = 
  | Num of int
  | Var of char
  | Add of expr*expr
  | Sub of expr*expr
  | Mul of expr*expr
  | Pow of expr*int
  | Pos of expr
  | Neg of expr

let rec 
  print_op1 c e = 
    Printf.printf "%s(" c;
    print_expr_r e;
    Printf.printf ")"
and
  print_op2 c e1 e2 = 
    Printf.printf "(";
    print_expr_r e1;
    Printf.printf "%s" c;
    print_expr_r e2;
    Printf.printf ")"
and 
  print_expr_r (e:expr): unit = 
    match e with
      | Num(i) -> Printf.printf "%d" i
      | Var(c) -> Printf.printf "%c" c
      | Add(e1,e2) -> print_op2 "+" e1 e2
      | Sub(e1,e2) -> print_op2 "-" e1 e2
      | Mul(e1,e2) -> print_op2 "*" e1 e2
      | Pow(e,i) -> 
          Printf.printf "(";
          print_expr_r e;
          Printf.printf ")^%d" i;
      | Pos(e) -> print_op1 "+" e
      | Neg(e) -> print_op1 "-" e

let rec pow (b: int) (e: int) : int =
  match e with
  | 0 -> 1
  | _ -> (pow b (e - 1)) * b

let rec eval_expr (e: expr) ~v:(v: int) : int =
  match e with
  | Add(e1, e2) ->  eval_expr e1 v + eval_expr e2 v
  | Sub(e1, e2) ->  eval_expr e1 v - eval_expr e2 v
  | Mul(e1, e2) ->  eval_expr e1 v * eval_expr e2 v
  | Pow(e , i ) ->  pow (eval_expr e v) i
  | Pos(e)      ->  eval_expr e v
  | Neg(e)      -> -eval_expr e v
  | Num(i)      ->  i
  | Var(c)      ->  v

let rec degree_expr (e: expr) : int =
  match e with
  | Add(e1, e2)     ->  max (degree_expr e1) (degree_expr e2)
  | Mul(e1, e2)     ->  degree_expr e1 + degree_expr e2
  | Pow(e , i )     ->  degree_expr e * i
  | Pos(e) | Neg(e) ->  degree_expr e
  | Num(i)          ->  0
  | Var(c)          ->  1
  | Sub(e1, e2)     -> (
    match degree_expr e1, degree_expr e2 with
    | d1, d2 when d1 = d2 -> 0 (* if two poly of equal degree are substracted, we have 0 degree *)
    | d1, d2              -> max d1 d2
  ) 
  
let print_expr (e:expr) :expr = 
  print_expr_r e;
  print_newline ();
  e