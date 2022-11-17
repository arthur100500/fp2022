module Interpreter : sig
  type context

  type interpreter_result =
    | Interpreted of context
    | Error of string
    | Returning of context
    | Breaking of context

  val interpret : Ast.Ast.ast -> context -> interpreter_result
  val emptyctx : context
end
