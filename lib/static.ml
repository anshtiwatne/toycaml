open Ast
open Env
open Error

let rec elab env = function
  (* Atoms *)
  | Var x -> env x
  | Const (ICon _) -> TInt
  | Const (BCon _) -> TBool
  | Nil -> TList (TInt) (* Polymorphic nil can be typed as any list *)
  (* Operations *)
  | UnOp (op, e) -> (
      let t = elab env e in
      match (op, t) with
      | Neg, TInt -> TInt
      | Not, TBool -> TBool
      | _ -> raise (TypeError "Unary operator type mismatch"))
  | BinOp (op, e1, e2) -> (
      let t1 = elab env e1 in
      let t2 = elab env e2 in
      match (op, t1, t2) with
      | (Add | Sub | Mul), TInt, TInt -> TInt
      | (Lt | Gt), TInt, TInt -> TBool
      | (Eq | Neq), _, _ when t1 = t2 -> TBool
      | (And | Or), TBool, TBool -> TBool
      | Cons, _, TList t when t1 = t -> TList t1
      | Append, TList t1, TList t2 when t1 = t2 -> TList t1
      | _ -> raise (TypeError "Binary operator type mismatch"))
  (* Control Flow *)
  | If (e1, e2, e3) ->
      let t1 = elab env e1 in
      if t1 <> TBool then raise (TypeError "If condition must be Bool")
      else
        let t2 = elab env e2 in
        let t3 = elab env e3 in
        if t2 = t3 then t2
        else raise (TypeError "If branches have different types")
  (* Pairs *)
  | Pair (e1, e2) -> TPair (elab env e1, elab env e2)
  | Fst e -> (
      match elab env e with
      | TPair (t1, _) -> t1
      | _ -> raise (TypeError "Fst applied to non-pair"))
  | Snd e -> (
      match elab env e with
      | TPair (_, t2) -> t2
      | _ -> raise (TypeError "Snd applied to non-pair"))
  (* Lists *)
  | List es -> (
      match es with
      | [] -> TList (TInt)
      | e :: rest ->
          let t = elab env e in
          List.iter (fun e' ->
            if elab env e' <> t then raise (TypeError "List elements have different types"))
            rest;
          TList t)
  (* Functions *)
  | Fun (x, t, e) ->
      let t_body = elab (update env x t) e in
      TArrow (t, t_body)
  | RFun (f, x, t_arg, t_res, e) ->
      let env' = update env f (TArrow (t_arg, t_res)) in
      let t_body = elab (update env' x t_arg) e in
      if t_body = t_res then TArrow (t_arg, t_res)
      else raise (TypeError "Recursive function body type mismatch")
  | App (e1, e2) -> (
      let t1 = elab env e1 in
      let t2 = elab env e2 in
      match t1 with
      | TArrow (t_arg, t_res) ->
          if t_arg = t2 then t_res
          else raise (TypeError "Function argument type mismatch")
      | _ -> raise (TypeError "Cannot apply non-function"))
  (* Bindings *)
  | Let (x, e1, e2) ->
      let t1 = elab env e1 in
      elab (update env x t1) e2
  | LetRec (f, x, t_arg, t_res, e1, e2) ->
      let t_func = TArrow (t_arg, t_res) in
      let env_inner = update (update env f t_func) x t_arg in
      if elab env_inner e1 <> t_res then
        raise (TypeError "LetRec definition type mismatch")
      else elab (update env f t_func) e2
