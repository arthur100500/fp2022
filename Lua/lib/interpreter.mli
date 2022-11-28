(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Interpreter : sig
  type context [@@deriving show { with_path = false }]

  type interpreter_result =
    | Interpreted of context
    | Error of string

  val interpret : Ast.Ast.t -> context -> interpreter_result
  val emptyctx : context
end
