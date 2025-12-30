open Ast
open Env

let syn_err msg = failwith ("Syntax Error: " ^ msg)

let rec elab env = function
  (* Atoms *)
  | Var x -> env x
  | Const (ICon _) -> TInt
  | Const (BCon _) -> TBool
  (* Operations *)
  | UnOp (op, e) -> (
      let t = elab env e in
      match (op, t) with
      | Neg, TInt -> TInt
      | Not, TBool -> TBool
      | _ -> syn_err "Unary operator type mismatch")
  | BinOp (op, e1, e2) -> (
      let t1 = elab env e1 in
      let t2 = elab env e2 in
      match (op, t1, t2) with
      | (Add | Sub | Mul), TInt, TInt -> TInt
      | (Lt | Gt), TInt, TInt -> TBool
      | (Eq | Neq), _, _ when t1 = t2 -> TBool
      | (And | Or), TBool, TBool -> TBool
      | _ -> syn_err "Binary operator type mismatch")
  (* Control Flow *)
  | If (e1, e2, e3) ->
      let t1 = elab env e1 in
      if t1 <> TBool then syn_err "If condition must be Bool"
      else
        let t2 = elab env e2 in
        let t3 = elab env e3 in
        if t2 = t3 then t2 else syn_err "If branches have different types"
  (* Pairs *)
  | Pair (e1, e2) -> TPair (elab env e1, elab env e2)
  | Fst e -> (
      match elab env e with
      | TPair (t1, _) -> t1
      | _ -> syn_err "Fst applied to non-pair")
  | Snd e -> (
      match elab env e with
      | TPair (_, t2) -> t2
      | _ -> syn_err "Snd applied to non-pair")
  (* Functions *)
  | Fun (x, t, e) ->
      let t_body = elab (update env x t) e in
      TArrow (t, t_body)
  | RFun (f, x, t_arg, t_res, e) ->
      let env' = update env f (TArrow (t_arg, t_res)) in
      let t_body = elab (update env' x t_arg) e in
      if t_body = t_res then TArrow (t_arg, t_res)
      else syn_err "Recursive function body type mismatch"
  | App (e1, e2) -> (
      let t1 = elab env e1 in
      let t2 = elab env e2 in
      match t1 with
      | TArrow (t_arg, t_res) ->
          if t_arg = t2 then t_res
          else syn_err "Function argument type mismatch"
      | _ -> syn_err "Cannot apply non-function")
  (* Bindings *)
  | Let (x, e1, e2) ->
      let t1 = elab env e1 in
      elab (update env x t1) e2
  | LetRec (f, x, t_arg, t_res, e1, e2) ->
      let t_func = TArrow (t_arg, t_res) in
      let env_inner = update (update env f t_func) x t_arg in
      if elab env_inner e1 <> t_res then
        syn_err "LetRec definition type mismatch"
      else elab (update env f t_func) e2
