open Base
open Lua_lib.Parser
open Lua_lib.Ast
include Parser

let print_ast = false

let print_ast code =
  match parse (Parser.string_to_input code) with
  | Failed m | HardFailed m ->
    print_endline (Printf.sprintf "Parser failed with message: %s" m)
  | Parsed (h, t) ->
    (match Parser.input_to_string t with
     | "" -> print_endline (Printf.sprintf "%s" (Ast.show_ast h))
     | t -> print_endline (Printf.sprintf "Parser failed and it is unparsed: %s" t))
;;

module _ = struct
  let sit_t = "34 'string =)' {1, 2}"
  let assign_t = "a[b], c = 3, 4, 5"
  let tables_c_t = "{} {b=3} {1} {1, 2} {1, b=2} {{}} {{}, 123} {[x] = x}"
  let tables_a_t = "a.b = 3 a.b.b.b = 3 a[b].c[d] = 3 a[4 + 1] = 3 + 2"

  let standart_constructions_t =
    {|
  if a then b elseif c then d else e end
  if a then b end
  if a then b else c end
  for i=1,2,3 do end
  for i=1,2 do end
  while a do b end
  repeat a until b
  return
  return 4
  break
  |}
  ;;

  let exprs_t =
    {|
  1 "3" true b {}
  a(1, {}, b)
  a.b a[b] 
  1 + 2
  1 + 2 * 3
  1 * 2 + 3
  1 + 2 * 3 + 4 * 5 ^ 6
  1 + 2 * 3 - 4 - 5 - 6
  1 ^ 2 - 3 * 5 ^ 5 - 7
  -5
  -5 + 1
  not false
  not true or 4
  not true and 4 or false
  not true and not false or true
  true and not false
  |}
  ;;

  let () = print_ast sit_t
  let () = print_ast assign_t
  let () = print_ast tables_c_t
  let () = print_ast tables_a_t
  let () = print_ast standart_constructions_t
  let () = print_ast exprs_t
end
