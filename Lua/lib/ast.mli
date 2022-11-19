(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Ast : sig
  type ident = string [@@deriving show { with_path = false }]

  type block = statement list [@@deriving show { with_path = false }]

  and unop =
    | Not (** not *)
    | USub (** - *)
  [@@deriving show { with_path = false }]

  and logic_binop =
    | And (** and *)
    | Or (** or *)
  [@@deriving show { with_path = false }]

  and arithm_binop =
    | Add (** + *)
    | Mul (** * *)
    | Sub (** - *)
    | Div (** / *)
    | Pow (** ^ *)
  [@@deriving show { with_path = false }]

  and compare_binop =
    | Le (** <= *)
    | Ge (** >= *)
    | Lt (** > *)
    | Gt (** < *)
    | Eq (** == *)
    | Ne (** ~= *)
  [@@deriving show { with_path = false }]

  and string_binop = Concat (** .. *)
   [@@deriving show { with_path = false }]

  and binop =
    | LOp of logic_binop (** Logic operators: and, or*)
    | AOp of arithm_binop (** Arithmetic operators: +. -. /, *, ^ *)
    | COp of compare_binop (** Comparative operators: <=, >=, <, >, ==, ~= *)
    | SOp of string_binop (** String operators: .. *)
  [@@deriving show { with_path = false }]

  and expr_table_entry =
    | JustExpr of expr
        (** Just an expressinon, its index will be determined by position *)
    | PairExpr of expr * expr (** Key/Value Pair ([key]=value)*)

  and lua_function = ident list * block

  and const =
    | LuaBool of bool (** true | false *)
    | LuaNumber of float (** 1, 2.3, 3, ...*)
    | LuaString of string (** "abc" *)
    | LuaFunction of lua_function (** function(args) block end *)
    | LuaNil (** nil *)
  [@@deriving show { with_path = false }, ord]

  and expr =
    | LuaConst of const (** number or string or ..., written in const *)
    | LuaVariable of ident (** obv. variable *)
    | LuaTableGet of expr * expr (** expr[expr] *)
    | LuaTableInit of expr_table_entry list (** {expr, expr, expr, ...} *)
    | LuaBinOp of binop * expr * expr (** expr binop expr *)
    | LuaUnOp of unop * expr (** unop expr*)
    | LuaExprApply of apply (** expr(expr, ..., expr) *)
  [@@deriving show { with_path = false }]

  and apply = LuaCall of expr * expr list (** expr(expr, ..., expr) *)
  [@@deriving show { with_path = false }]

  and statement =
    | LuaDo of block (** do sttmn sttmnt end*)
    | LuaSet of lvalue list * expr list (** a, b, c = d, e, f*)
    | LuaWhile of expr * block (** while expr do block*)
    | LuaRepeat of block * expr (** repeat block until expr *)
    | LuaIf of expr * block * elseif_block list * block option
        (** if expr then block [elseif block] else block end*)
    | LuaFornum of ident * expr * expr * expr option * block
        (** for var = expr, expr, ?expr, block *)
    | LuaForin of ident list * expr list * block
        (** for expr, expr, ... in expr, expr, ... do block *)
    | LuaLocal of ident list * expr list (** local a = ?expr *)
    | LuaReturn of expr option (** return expr *)
    | LuaBreak (** break *)
    | LuaStatementApply of apply (** just a fun call, needed because of side effects  *)
    | LuaFunctionDeclare of lvalue * ident list * block (** function name(args) body end*)
    | LuaExpr of expr (** just an expression, will be printed, for REPL *)
  [@@deriving show { with_path = false }]

  and lvalue =
    | Index of lvalue * expr (** a[1][2][3]..[n][n][n] *)
    | Ident of ident (** var *)
  [@@deriving show { with_path = false }]

  (** elseif expr then block *)
  and elseif_block = expr * block

  type ast = block [@@deriving show { with_path = false }]
  type t = ast
end
