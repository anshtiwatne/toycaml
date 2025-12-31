open Ast
open Token
open Error
open Pretty

let expect_msg expected found =
  match found with
  | t :: _ -> "Expected " ^ expected ^ " got '" ^ string_of_token t ^ "'"
  | [] -> "Expected " ^ expected ^ " but reached end of input"

let expect_var = function
  | ID x :: ts -> (x, ts)
  | ts -> raise (SyntaxError (expect_msg "variable" ts))

let expect t = function
  | t' :: tr when t' = t -> tr
  | ts -> raise (SyntaxError (expect_msg (string_of_token t) ts))
(* Type Grammar:
   ty    ::= pty [ -> ty ]          (Right associative arrow)
   pty   ::= aty [ * pty ]          (Right associative pair)
   aty   ::= int | bool | ( ty )
*)

let rec parse_ty ts =
  let t1, ts = parse_pty ts in
  match ts with
  | ARROW :: ts' ->
      let t2, ts'' = parse_ty ts' in
      (TArrow (t1, t2), ts'')
  | _ -> (t1, ts)

and parse_pty ts =
  let t1, ts = parse_aty ts in
  match ts with
  | TIMES :: ts' ->
      let t2, ts'' = parse_pty ts' in
      (TPair (t1, t2), ts'')
  | _ -> (t1, ts)

and parse_aty = function
  | TINT :: ts -> (TInt, ts)
  | TBOOL :: ts -> (TBool, ts)
  | LP :: ts ->
      let t, ts = parse_ty ts in
      let ts = expect RP ts in
      (t, ts)
  | ts -> raise (SyntaxError (expect_msg "type" ts))

(* Operator Precedence Table *)
let opcode_info = function
  | OR -> Some (Or, 1)
  | AND -> Some (And, 2)
  | EQ -> Some (Eq, 3)
  | NEQ -> Some (Neq, 3)
  | LT -> Some (Lt, 3)
  | GT -> Some (Gt, 3)
  | PLUS -> Some (Add, 4)
  | MINUS -> Some (Sub, 4)
  | TIMES -> Some (Mul, 5)
  | _ -> None

let rec parse_exp ts =
  match ts with
  | IF :: ts ->
      let e1, ts = parse_exp ts in
      let ts = expect THEN ts in
      let e2, ts = parse_exp ts in
      let ts = expect ELSE ts in
      let e3, ts = parse_exp ts in
      (If (e1, e2, e3), ts)
  | FUN :: ts ->
      let ts = expect LP ts in
      let x, ts = expect_var ts in
      let ts = expect COLON ts in
      let t, ts = parse_ty ts in
      let ts = expect RP ts in
      let ts = expect ARROW ts in
      let e, ts = parse_exp ts in
      (Fun (x, t, e), ts)
  | RFUN :: ts ->
      let f, ts = expect_var ts in
      let ts = expect LP ts in
      let x, ts = expect_var ts in
      let ts = expect COLON ts in
      let t1, ts = parse_pty ts in
      let ts = expect RP ts in
      let ts = expect COLON ts in
      let t2, ts = parse_pty ts in
      let ts = expect ARROW ts in
      let e, ts = parse_exp ts in
      (RFun (f, x, t1, t2, e), ts)
  | LET :: REC :: ts ->
      let f, ts = expect_var ts in
      let ts = expect LP ts in
      let x, ts = expect_var ts in
      let ts = expect COLON ts in
      let t_arg, ts = parse_ty ts in
      let ts = expect RP ts in
      let ts = expect COLON ts in
      let t_res, ts = parse_ty ts in
      let ts = expect EQ ts in
      let e1, ts = parse_exp ts in
      let ts = expect IN ts in
      let e2, ts = parse_exp ts in
      (LetRec (f, x, t_arg, t_res, e1, e2), ts)
  | LET :: ts ->
      let x, ts = expect_var ts in
      let ts = expect EQ ts in
      let e1, ts = parse_exp ts in
      let ts = expect IN ts in
      let e2, ts = parse_exp ts in
      (Let (x, e1, e2), ts)
  | _ -> parse_binary 0 ts

(* Operator Precedence Parsing *)
and parse_binary min_prec ts =
  let lhs, ts = parse_app ts in
  parse_binary_loop min_prec lhs ts

and parse_binary_loop min_prec lhs ts =
  match ts with
  | op_token :: ts' -> (
      match opcode_info op_token with
      | Some (op, prec) when prec >= min_prec ->
          (* Left associative *)
          let rhs, ts'' = parse_binary (prec + 1) ts' in
          parse_binary_loop min_prec (BinOp (op, lhs, rhs)) ts''
      | _ -> (lhs, ts))
  | [] -> (lhs, ts)

(* Function Application *)
and parse_app ts =
  let rec loop f ts =
    try
      let arg, ts' = parse_atom ts in
      loop (App (f, arg)) ts'
    with SyntaxError _ -> (f, ts)
    (* Catch par_err here to stop application chain *)
  in
  let e, ts = parse_atom ts in
  loop e ts

(* Atomic Expressions (and Unary Ops) *)
and parse_atom ts =
  match ts with
  | CONST c :: ts -> (Const c, ts)
  | ID x :: ts -> (Var x, ts)
  (* Unary Operators *)
  | NOT :: ts ->
      let e, ts = parse_atom ts in
      (UnOp (Not, e), ts)
  | MINUS :: ts ->
      (* Minus handled as negation if atomic *)
      let e, ts = parse_atom ts in
      (UnOp (Neg, e), ts)
  | FST :: ts ->
      let e, ts = parse_atom ts in
      (Fst e, ts)
  | SND :: ts ->
      let e, ts = parse_atom ts in
      (Snd e, ts)
  (* Parentheses and Pairs *)
  | LP :: ts -> (
      let e1, ts = parse_exp ts in
      match ts with
      | RP :: ts -> (e1, ts)
      | COMMA :: ts ->
          let e2, ts = parse_exp ts in
          let ts = expect RP ts in
          (Pair (e1, e2), ts)
      | _ -> raise (SyntaxError (expect_msg ") or ," ts)))
  | ts -> raise (SyntaxError (expect_msg "expression" ts))

let parse ts =
  let e, ts = parse_exp ts in
  match ts with
  | [] -> e
  | t :: _ ->
      raise
        (SyntaxError
           ("Unexpected token at end of parsing '" ^ string_of_token t ^ "')"))
