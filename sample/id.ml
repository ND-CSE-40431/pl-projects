(* Example of a REPL that does nothing *)

open Syntax

let () = read_lines "id> " (fun line -> 
  try
    print_string (format_term (parse_term line) ^ "\n")
  with Parse_error s -> print_string ("error: " ^ s ^ "\n"))
