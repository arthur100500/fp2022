open Utils

module rec Ast : sig
  type ident = string [@@deriving show { with_path = false }]

  type block = statement list [@@deriving show { with_path = false }]

  and expr_table_entry =
    | JustExpr of expr
    | PairExpr of expr * expr

  and const =
    (* true | false *)
    | LuaBool of bool
    (* 1, 2.3, 3, ...*)
    | LuaNumber of float
    (* "abc" *)
    | LuaString of string
    (* table of strings ints tables with other thigs... *)
    | LuaTable of LuaTableMap.t
    (* function(args) block end *)
    | LuaFunction of ident list * block
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
    | LuaIf of expr * block * elseif_block list * block maybe
    (* for var = expr, expr, ?expr, block *)
    | LuaFornum of ident * expr * expr * expr maybe * block
    (* for expr, expr, ... in expr, expr, ... do block *)
    | LuaForin of ident list * expr list * block
    (* local a = ?expr *)
    | LuaLocal of ident list * expr list
    (* return expr *)
    | LuaReturn of expr maybe
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

  and const =
    | LuaBool of bool
    | LuaNumber of float
    | LuaString of string
    | LuaTable of LuaTableMap.t
    | LuaFunction of ident list * block
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
    | LuaIf of expr * block * elseif_block list * block maybe
    | LuaFornum of ident * expr * expr * expr maybe * block
    | LuaForin of ident list * expr list * block
    | LuaLocal of ident list * expr list
    | LuaReturn of expr maybe
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

and LuaTableMap : sig
  type t [@@deriving show]

  val empty : t
  val add : Ast.const -> Ast.const -> t -> t
  val remove : Ast.const -> t -> t
  val replace : Ast.const -> Ast.const -> t -> t
  val find_opt : Ast.const -> t -> Ast.const option
  val compare : t -> t -> int
end = struct
  include Ast

  module M = Map.Make (struct
    type t = Ast.const

    let compare = compare
  end)

  type t = Ast.const M.t

  let compare a b = M.compare compare a b

  let show m =
    M.fold (fun k v a -> a ^ Format.asprintf "[%a] = %a" pp_const k pp_const v) m ""
  ;;

  let pp ppf t =
    Format.fprintf ppf "{"
    |> fun () ->
    M.iter (fun k v -> Format.fprintf ppf "[%a] = %a" pp_const k pp_const v) t
    |> fun () -> Format.fprintf ppf "}"
  ;;

  let empty = M.empty
  let add = M.add
  let remove = M.remove
  let replace k v tbl = M.update k (fun _ -> Some v) tbl
  let find_opt = M.find_opt
end
