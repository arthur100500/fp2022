(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Ast : sig
  type ident = string [@@deriving show { with_path = false }]

  type block = statement list [@@deriving show { with_path = false }]

  and expr_table_entry =
    | JustExpr of expr
    | PairExpr of expr * expr

  and lua_function = ident list * block

  and const =
    (* true | false *)
    | LuaBool of bool
    (* 1, 2.3, 3, ...*)
    | LuaNumber of float
    (* "abc" *)
    | LuaString of string
    (* function(args) block end *)
    | LuaFunction of lua_function
    (* nil *)
    | LuaNil
  [@@deriving show { with_path = false }, ord]

  and expr =
    (* number or string or ..., written in const *)
    | LuaConst of const
    (* obv. variable *)
    | LuaVariable of ident
    (* expr[expr] *)
    | LuaTableGet of expr * expr
    (* {expr, expr, expr, ...} *)
    | LuaTableInit of expr_table_entry list
    (* expr binop expr *)
    | LuaBinOp of string * expr * expr
    (* unop expr*)
    | LuaUnOp of string * expr
    (* expr(expr, ..., expr) *)
    | LuaExprApply of apply
  [@@deriving show { with_path = false }]

  and apply = (* expr(expr, ..., expr) *)
    | LuaCall of expr * expr list
  [@@deriving show { with_path = false }]

  and statement =
    (* do sttmn sttmnt end*)
    | LuaDo of block
    (* a, b, c = d, e, f*)
    | LuaSet of lvalue list * expr list
    (* while expr do block*)
    | LuaWhile of expr * block
    (* repeat block until expr *)
    | LuaRepeat of block * expr
    (* if expr then block [elseif block] else block end*)
    | LuaIf of expr * block * elseif_block list * block option
    (* for var = expr, expr, ?expr, block *)
    | LuaFornum of ident * expr * expr * expr option * block
    (* for expr, expr, ... in expr, expr, ... do block *)
    | LuaForin of ident list * expr list * block
    (* local a = ?expr *)
    | LuaLocal of ident list * expr list
    (* return expr *)
    | LuaReturn of expr option
    (* break *)
    | LuaBreak
    (* just a fun call, needed because of side effects  *)
    | LuaStatementApply of apply
    (* function name(args) body end*)
    | LuaFunctionDeclare of lvalue * ident list * block
    (* just an expression, will be printed, for REPL *)
    | LuaExpr of expr
  [@@deriving show { with_path = false }]

  and lvalue =
    (* a[1][2][3]..[n][n][n] *)
    | Index of lvalue * expr
    (* var *)
    | Ident of ident
  [@@deriving show { with_path = false }]

  (* elseif expr then block *)
  and elseif_block = expr * block

  type ast = block [@@deriving show { with_path = false }]
  type t = ast
end = struct
  type ident = string [@@deriving show { with_path = false }]

  type block = statement list [@@deriving show { with_path = false }]

  and expr_table_entry =
    | JustExpr of expr
    | PairExpr of expr * expr

  and lua_function = ident list * block

  and const =
    | LuaBool of bool
    | LuaNumber of float
    | LuaString of string
    | LuaFunction of lua_function
    | LuaNil
  [@@deriving show { with_path = false }]

  and expr =
    | LuaConst of const
    | LuaVariable of ident
    | LuaTableGet of expr * expr
    | LuaTableInit of expr_table_entry list
    | LuaBinOp of string * expr * expr
    | LuaUnOp of string * expr
    | LuaExprApply of apply
  [@@deriving show { with_path = false }]

  and apply = LuaCall of expr * expr list [@@deriving show { with_path = false }]

  and statement =
    | LuaDo of block
    | LuaSet of lvalue list * expr list
    | LuaWhile of expr * block
    | LuaRepeat of block * expr
    | LuaIf of expr * block * elseif_block list * block option
    | LuaFornum of ident * expr * expr * expr option * block
    | LuaForin of ident list * expr list * block
    | LuaLocal of ident list * expr list
    | LuaReturn of expr option
    | LuaBreak
    | LuaStatementApply of apply
    | LuaFunctionDeclare of lvalue * ident list * block
    | LuaExpr of expr
  [@@deriving show { with_path = false }]

  and lvalue =
    | Index of lvalue * expr
    | Ident of ident
  [@@deriving show { with_path = false }]

  and elseif_block = expr * block

  type ast = statement list [@@deriving show { with_path = false }]
  type t = ast

  let show_const x = Format.asprintf "%a" pp_const x
end
