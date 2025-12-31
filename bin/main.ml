open Toycaml_lib
open Static
open Dynamic
open Lexer
open Parser
open Env
open Pretty

let toycaml input =
  try
    let tokens = lex (explode input) in
    let ast = parse tokens in
    let ty = elab empty ast in
    let value = eval empty ast in
    Printf.printf "%s : %s\n" (string_of_val value) (string_of_ty ty)
  with exn ->
    Printf.eprintf "%s\n" (Error.string_of_exception exn);
    flush stderr

let read_file path =
  let ic = open_in path in
  let buf = Bytes.create (in_channel_length ic) in
  really_input ic buf 0 (Bytes.length buf);
  close_in ic;
  Bytes.to_string buf

let () =
  match Sys.argv with
  | [| _ |] ->
      print_endline
        "ToyCaml Interpreter\nEnter expressions (Press Ctrl+D to exit):";
      while true do
        print_string "> ";
        flush stdout;
        toycaml (read_line ())
      done
  | [| _; fname |] -> toycaml (read_file fname)
  | _ -> ()
