open Base
open Lua_lib.Parser
open Lua_lib.Interpreter
open Lua_lib.Ast
include Parser

let print_ast = false

let run_from_string code ctx =
  match parse (Parser.string_to_input code) with
  | Failed m | HardFailed m ->
    print_endline (Printf.sprintf "Parser failed with message: %s" m)
  | Parsed (h, t) ->
    (match Parser.input_to_string t with
     | "" ->
       if print_ast
       then print_endline (Printf.sprintf "Parsed ast for input: %s" (Ast.show_ast h))
       else ();
       (match Interpreter.interpret h ctx with
        | Interpreter.Interpreted _ -> ()
        | Error m ->
          print_endline (Printf.sprintf "Interpreter failed with message: %s" m)
        | _ -> print_endline "Something strange, returning or breaking still..")
     | t -> print_endline (Printf.sprintf "Parser failed and it is unparsed: %s" t))
;;

module _ = struct
  let fact =
    {|
function f(x)
  if x >= 1 then return f(x - 1) * x end
  return 1
end

print("Factorial")
for i = 1, 10 do
  print(f(i))
end
  |}
  ;;

  let fib =
    {|
f = function(x)
  if x < 2 then
    return 1
  else
    return f(x - 2) + f(x - 1)
  end
end

print()
print("Fibonacci")
for i = 1, 10 do
  print(f(i))
end
  |}
  ;;

  let global_local =
    {|
a = 4

function b()
  a = 3
  local a = 5
  print("local:")
  print(a)
end

print()
print("Global/local")
print("global:")
print(a)
b()
print("global after change:")
print(a)
|}
  ;;

  let tables =
    {|
-- Bool to string
function b2s(b) if b then return "true" else return "false" end end
print("Table tests")
a = {}
print("Empty table create: " .. b2s(a == {}))
a = {1, 2, 3}
print("Filled table create: " .. b2s(a[1] == 1 and a[2] == 2 and a[3] == 3))
a = {[6] = 5, 2, 3, [0] = "s"}
print("Filled indexed table create: " .. b2s(a[1] == 2 and a[2] == 3 and a[0] == "s" and a[6] == 5))
a = {["a"] = 4, ["b"] = 9, ["huge_giant_name"] = 42}
print("String indexing like fields: " .. b2s(a["a"] == 4 and a.b == 9 and a.huge_giant_name == 42))
a = {a = 4, b = 9, huge_giant_name = 42}
print("Creating fields in table: " .. b2s(a["a"] == 4 and a.b == 9 and a.huge_giant_name == 42))
a = {{{{}, {2}}, {}}, {b = {3}}}
print("Inner tables: " .. b2s(a[1][1][2][1] == 2 and a[2].b == {3} and a[2].b[1] == 3))
|}
  ;;

  let precedence_t =
    {|
print("Arithmetics and precedence")
print(1 + 2 * 3)
print(1 * 2 + 3)
print(3 + 4 * 9 - 2 ^ 4 * 3)
print(3 - 1 - 1)
print(true and not false)
|}
  ;;

  let map_t = 
    {|
-- I don't have forin loop but it will be here soon
function map(lst, f)
  local i = 1
  while lst[i] do
    lst[i] = f(lst[i])
    i = i + 1
  end
  return lst
end

l = {2, 4, 6, 8, 10, 12, 14, 17}
f_ = function(x) return x ^ 2 end
l1 = map(l, f_)

print("Map function")
for i = 1, 8 do
  print(l1[i])
end

    |}

  let ectx = Interpreter.emptyctx
  let () = run_from_string fact ectx
  let () = run_from_string fib ectx
  let () = run_from_string global_local ectx
  let () = run_from_string tables ectx
  let () = run_from_string precedence_t ectx
  let () = run_from_string map_t ectx
end
