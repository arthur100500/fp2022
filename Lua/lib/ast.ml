(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Ast = struct
  type ident = string [@@deriving show { with_path = false }]

  type block = statement list [@@deriving show { with_path = false }]

  and unop =
    | Not
    | USub
  [@@deriving show { with_path = false }]

  and logic_binop =
    | And
    | Or
  [@@deriving show { with_path = false }]

  and arithm_binop =
    | Add
    | Mul
    | Sub
    | Div
    | Pow
  [@@deriving show { with_path = false }]

  and compare_binop =
    | Le
    | Ge
    | Lt
    | Gt
    | Eq
    | Ne
  [@@deriving show { with_path = false }]

  and string_binop = Concat [@@deriving show { with_path = false }]

  and binop =
    | LOp of logic_binop
    | AOp of arithm_binop
    | COp of compare_binop
    | SOp of string_binop
  [@@deriving show { with_path = false }]

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
    | LuaBinOp of binop * expr * expr
    | LuaUnOp of unop * expr
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
