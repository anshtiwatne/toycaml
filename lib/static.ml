open Ast
open Env

let rec elab env = function
  | Con (BCon _) -> Bool
  | Con (ICon _) -> Int
  | Var x -> env x
  | If (e1, e2, e3) ->
      let t1 = elab env e1 in
      if t1 <> Bool then failwith "If condition must be Bool"
      else
        let t2 = elab env e2 in
        let t3 = elab env e3 in
        if t2 = t3 then t2 else failwith "If branches have different types"
  | OApp (op, e1, e2) -> (
      let t1 = elab env e1 in
      let t2 = elab env e2 in
      match (op, t1, t2) with
      | (Add | Sub | Mul), Int, Int -> Int
      | Leq, Int, Int -> Bool
      | _ -> failwith "Operator type mismatch")
  | Fun (x, t, e) ->
      let t_body = elab (update env x t) e in
      Arrow (t, t_body)
  | RFun (f, x, t_arg, t_res, e) ->
      let env' = update env f (Arrow (t_arg, t_res)) in
      let env'' = update env' x t_arg in
      let t_body = elab env'' e in
      if t_body = t_res then Arrow (t_arg, t_res)
      else failwith "Recursive function body type mismatch"
  | FApp (e1, e2) -> (
      let t1 = elab env e1 in
      let t2 = elab env e2 in
      match t1 with
      | Arrow (t_arg, t_res) ->
          if t_arg = t2 then t_res
          else failwith "Function argument type mismatch"
      | _ -> failwith "Cannot apply non-function")
  | Pair (e1, e2) ->
      let t1 = elab env e1 in
      let t2 = elab env e2 in
      Product (t1, t2)
  | Fst e -> (
      match elab env e with
      | Product (t1, _) -> t1
      | _ -> failwith "Fst applied to non-pair")
  | Snd e -> (
      match elab env e with
      | Product (_, t2) -> t2
      | _ -> failwith "Snd applied to non-pair")
