open Token

let lex_err msg = failwith ("Lexical Error: " ^ msg)

(* Character predicates *)
let is_digit = function '0' .. '9' -> true | _ -> false

let is_id_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false

(* Utility functions *)
let int_of_digit c = Char.code c - Char.code '0'
let explode s = List.init (String.length s) (String.get s)
let implode s = String.of_seq (List.to_seq s)

(* Keyword lookup *)
let keyword_or_var = function
  (* Type constructors *)
  | "int" -> TINT
  | "bool" -> TBOOL
  (* Control flow *)
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  (* Pairs *)
  | "fst" -> FST
  | "snd" -> SND
  (* Functions *)
  | "fun" -> FUN
  | "rfun" -> RFUN
  (* Bindings *)
  | "let" -> LET
  | "rec" -> REC
  | "in" -> IN
  (* Constants *)
  | "true" -> CONST (BCon true)
  | "false" -> CONST (BCon false)
  (* Variables *)
  | s -> ID s

(* Main lexer *)
let rec lex cs =
  match cs with
  | [] -> []
  (* Whitespace *)
  | (' ' | '\t' | '\n') :: cr -> lex cr
  (* Delimiters *)
  | '(' :: cr -> LP :: lex cr
  | ')' :: cr -> RP :: lex cr
  | ',' :: cr -> COMMA :: lex cr
  | ':' :: cr -> COLON :: lex cr
  (* Type constructors *)
  | '-' :: '>' :: cr -> ARROW :: lex cr
  (* Unary operators *)
  | '-' :: cr -> MINUS :: lex cr (* negation and subtraction *)
  | '!' :: cr -> NOT :: lex cr
  (* Binary operators: arithmetic *)
  | '+' :: cr -> PLUS :: lex cr
  | '*' :: cr -> TIMES :: lex cr
  (* Binary operators: comparison *)
  | '=' :: cr -> EQ :: lex cr
  | '<' :: '>' :: cr -> NEQ :: lex cr
  | '<' :: cr -> LT :: lex cr
  | '>' :: cr -> GT :: lex cr
  (* Binary operators: logical *)
  | '&' :: '&' :: cr -> AND :: lex cr
  | '|' :: '|' :: cr -> OR :: lex cr
  (* Numbers *)
  | '0' .. '9' :: _ -> lex_num 0 cs
  (* Identifiers and keywords *)
  | ('a' .. 'z' | 'A' .. 'Z' | '_') :: _ -> lex_var [] cs
  | c :: _ -> lex_err ("Unexpected character '" ^ String.make 1 c ^ "'")

(* Lex integer literals *)
and lex_num acc = function
  | c :: cr when is_digit c -> lex_num ((acc * 10) + int_of_digit c) cr
  | cs -> CONST (ICon acc) :: lex cs

(* Lex identifiers and keywords *)
and lex_var acc = function
  | c :: cr when is_id_char c -> lex_var (c :: acc) cr
  | cs ->
      let s = implode (List.rev acc) in
      keyword_or_var s :: lex cs
