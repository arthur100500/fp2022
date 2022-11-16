open Utils
open Ast

module Interpreter : sig
  type context

  type interpreter_result =
    | Interpreted of context
    | Error of string
    | Returning of context
    | Breaking of context

  val interpret : Ast.ast -> context -> interpreter_result
  val emptyctx : context
end = struct
  include Ast
  include Parser

  module VarsMap : sig
    type t [@@deriving show]

    val find_opt : string -> t -> const option
    val replace : string -> const -> t -> t
    val empty : t
  end = struct
    module M = Map.Make (String)

    type t = const M.t

    let show m = M.fold (fun k v a -> a ^ Format.asprintf "[%s] = %a" k pp_const v) m ""
    let pp ppf t = M.iter (fun k v -> Format.fprintf ppf "[%s] = %a" k pp_const v) t
    let find_opt = M.find_opt
    let replace k v t = M.update k (fun _ -> Some v) t
    let empty = M.empty
  end

  type variables = VarsMap.t [@@deriving show { with_path = false }]

  type context =
    { vars : variables
    ; previous : context maybe
    ; last_exec : const
    ; brk : int maybe
    ; ret : int maybe
    ; level : int
    }
  [@@deriving show { with_path = false }]

  let show_context x = Format.asprintf "%a" pp_context x

  type interpreter_result =
    | Interpreted of context
    | Error of string
    | Returning of context
    | Breaking of context

  type interpreter = context -> interpreter_result

  let return c _ = Interpreted c
  let error m _ = Error m

  let ( >>= ) : interpreter -> (context -> interpreter) -> interpreter =
   fun i f c ->
    match i c with
    | Interpreted c2 -> f c2 c2
    | Error s -> Error s
    | Returning s -> Returning s
    | Breaking s -> Breaking s
 ;;

  let return_le res ctx = return { ctx with last_exec = res }

  let rec get_var varname = function
    | None -> return_le LuaNil
    | Some ctx ->
      (match VarsMap.find_opt varname ctx.vars with
       | Some v -> return_le v
       | None -> get_var varname ctx.previous)
  ;;

  let rec exec_expr ex ctx =
    match ex with
    | LuaConst c -> return_le c ctx ctx
    | LuaVariable v -> (get_var v (Some ctx)) ctx ctx
    | LuaTableGet (t, i) -> get_from_table ctx t i
    | LuaTableInit el -> create_table ctx el ctx
    | LuaUnOp (op, le) -> exec_un_op op le ctx
    | LuaBinOp (op, le, re) -> exec_bin_op op le re ctx
    | LuaExprApply apply -> exec_fun apply ctx ctx

  and exec_fun apply ctx =
    (* Hardcoded functions *)
    let rec print_vars exprs ctx =
      match exprs with
      | [] -> return ctx
      | h :: t ->
        exec_expr h
        >>= fun h_res ->
        (match h_res.last_exec with
         | LuaString s -> print_string s
         | LuaNumber n -> print_float n
         | LuaFunction _ -> print_string "<function>"
         | LuaNil -> print_string "nil"
         | LuaBool v -> print_string (if v then "true" else "false")
         | tbl -> print_string (show_const tbl));
        print_endline "";
        print_vars t h_res
    in
    match apply with
    | LuaCall (fn, args) ->
      (* Hardcoded functions *)
      (match fn with
       | LuaVariable "print" -> print_vars args ctx
       | LuaVariable "pctx" ->
         print_endline (show_context ctx);
         return ctx
       | _ ->
         exec_expr fn
         >>= fun expr_e ->
         return
           { ctx with
             ret = Some ctx.level
           ; vars = VarsMap.empty
           ; previous = Some ctx
           ; last_exec = LuaNil
           ; level = ctx.level + 1
           }
         >>= fun _ ->
         (match expr_e.last_exec with
          | LuaFunction (idents, body) ->
            exec_local_set idents args
            >>= fun c ->
            (match exec_many body c with
             | Returning ctx -> return ctx
             | Interpreted ctx ->
               return ctx
               >>= fun ctx ->
               (match ctx.previous with
                | Some p -> return { p with last_exec = LuaNil }
                | None -> error "Exiting from global context")
             | Error m -> error m
             | Breaking _ -> error "breaking from non loop")
          | _ -> error "You can only call functions"))

  and exec_un_op op le =
    let exec_bool_uop op last_ctx = function
      | LuaBool x -> return { last_ctx with last_exec = LuaBool (op x) }
      | _ -> error "Attempt to do logic with non boolens"
    in
    let exec_num_uop op last_ctx = function
      | LuaNumber x -> return { last_ctx with last_exec = LuaNumber (op x) }
      | _ -> error "Attempt to do math with non numbers"
    in
    exec_expr le
    >>= fun ler ->
    match op with
    | "not" -> exec_bool_uop not ler ler.last_exec
    | "-" -> exec_num_uop (fun x -> 0. -. x) ler ler.last_exec
    | _ -> error "operator is not implemented"

  and exec_bin_op op le re ctx =
    (let exec_num_op ler rer op last_ctx =
       match ler, rer with
       | LuaNumber x, LuaNumber y ->
         return { last_ctx with last_exec = LuaNumber (op x y) }
       | _ -> error "Attempt to do math with non numbers"
     in
     let exec_str_op ler rer op last_ctx =
       match ler, rer with
       | LuaString x, LuaString y ->
         return { last_ctx with last_exec = LuaString (op x y) }
       | _ -> error "Attempt to do concatenation with non strings"
     in
     let exec_bool_op ler rer op last_ctx =
       match ler, rer with
       | x, y -> return { last_ctx with last_exec = LuaBool (op (get_bool x) (get_bool y)) }
     in
     let exec_eq_op ler rer op last_ctx = 
      match ler, rer with
        x, y  -> return { last_ctx with last_exec = LuaBool (op x y) }
     in
     let exec_comp_op ler rer op last_ctx =
       match ler, rer with
       | x,  y -> return { last_ctx with last_exec = LuaBool (op x y) }
     in
     exec_expr le
     >>= fun le_e ->
     exec_expr re
     >>= fun re_e ->
     let ler = le_e.last_exec in
     let rer = re_e.last_exec in
     match op with
     | "*" -> exec_num_op ler rer ( *. ) re_e
     | "+" -> exec_num_op ler rer ( +. ) re_e
     | "-" -> exec_num_op ler rer ( -. ) re_e
     | "/" -> exec_num_op ler rer ( /. ) re_e
     | "^" -> exec_num_op ler rer ( ** ) re_e
     | "and" -> exec_bool_op ler rer ( && ) re_e
     | "or" -> exec_bool_op ler rer ( || ) re_e
     | "<=" -> exec_comp_op ler rer ( <= ) re_e
     | ">=" -> exec_comp_op ler rer ( >= ) re_e
     | ">" -> exec_comp_op ler rer ( > ) re_e
     | "<" -> exec_comp_op ler rer ( < ) re_e
     | "==" -> exec_eq_op ler rer ( = ) re_e
     | "~=" -> exec_eq_op ler rer ( <> ) re_e
     | ".." -> exec_str_op ler rer ( ^ ) re_e
     | _ -> error "operator is not implemented")
      ctx

  and get_from_table ctx t i =
    (exec_expr t
    >>= fun t_e ->
    exec_expr i
    >>= fun i_e ->
    match t_e.last_exec with
    | LuaTable ht ->
      (match LuaTableMap.find_opt i_e.last_exec ht with
       | Some v -> return { ctx with last_exec = v }
       | None -> return { ctx with last_exec = LuaNil })
    | _ -> error "Taking index from non table")
      ctx

  and create_table ctx kvp_e_list =
    let ht = LuaTableMap.empty in
    let rec add_kvp tbl ictx li = function
      | [] -> return { ictx with last_exec = LuaTable tbl }
      | h :: t ->
        (match h with
         | PairExpr (k, v) ->
           exec_expr k
           >>= fun k_e ->
           exec_expr v
           >>= fun v_e ->
           add_kvp (LuaTableMap.replace k_e.last_exec v_e.last_exec tbl) v_e li t
         | JustExpr v ->
           exec_expr v
           >>= fun v_e ->
           add_kvp (LuaTableMap.replace (LuaNumber li) v_e.last_exec tbl) v_e (li +. 1.) t)
    in
    add_kvp ht ctx 1. kvp_e_list

  and exec_up_ctx interpreter ctx =
    (return
       { ctx with
         vars = VarsMap.empty
       ; previous = Some ctx
       ; last_exec = LuaNil
       ; level = ctx.level + 1
       }
    >>= fun _ ->
    interpreter
    >>= fun chctx ->
    match chctx.previous with
    | Some c -> return c
    | None -> error "exit from global context")
      ctx

  and exec_loop_ctx interpreter ctx =
    (return
       { ctx with
         vars = VarsMap.empty
       ; previous = Some ctx
       ; brk = Some ctx.level
       ; last_exec = LuaNil
       ; level = ctx.level + 1
       }
    >>= fun nctx ->
    match interpreter nctx with
    | Breaking ct -> return ct
    | Returning e -> fun _ -> Returning e
    | Error m -> error m
    | Interpreted ct ->
      (match ct.previous with
       | Some c -> return c
       | None -> error "exit from global context"))
      ctx

  and exec_local_set idents exprs ctx =
    let set_ident id ex ctx =
      (exec_expr ex
      >>= fun ex_ctx ->
      return { ex_ctx with vars = VarsMap.replace id ex_ctx.last_exec ex_ctx.vars })
        ctx
    in
    let rec helper ctx = function
      | ih :: it, ah :: at -> set_ident ih ah >>= fun nctx -> helper nctx (it, at)
      | ih :: it, _ ->
        return ctx
        >>= fun _ -> set_ident ih (LuaConst LuaNil) >>= fun nctx -> helper nctx (it, [])
      | _, _ -> return ctx
    in
    helper ctx (idents, exprs) ctx

  and exec_stat stat ctx =
    match stat with
    | LuaExpr expr ->
      exec_stat (LuaStatementApply (LuaCall (LuaVariable "print", [ expr ]))) ctx
    | LuaSet (le, re) -> exec_set le re ctx
    | LuaLocal (ids, exps) -> exec_local_set ids exps ctx
    | LuaDo block -> exec_up_ctx (exec_many block) ctx
    | LuaIf (ifexpr, block, elseif_blocks, else_block) ->
      exec_up_ctx (exec_if ifexpr block elseif_blocks else_block) ctx
    | LuaStatementApply apply -> exec_expr (LuaExprApply apply) ctx
    | LuaReturn ex -> exec_return ex ctx
    | LuaFunctionDeclare (id, ids, blk) -> exec_set_one id (LuaFunction (ids, blk)) ctx
    | LuaBreak -> exec_break ctx
    | LuaWhile (ex, blck) -> exec_loop_ctx (exec_while ex blck) ctx
    | LuaRepeat (blck, ex) -> exec_loop_ctx (exec_until blck ex) ctx
    | LuaFornum (ident, st, en, step, blck) ->
      exec_loop_ctx (exec_for_num ident st en step blck) ctx
    | LuaForin (_, _, _) -> error "For in loop is not implemented yet =( " ctx

  and exec_for_num id bg ed st blck =
    exec_expr bg
    >>= fun loop_begin ->
    exec_expr ed
    >>= fun loop_end ->
    exec_expr
      (match st with
       | Some s -> s
       | None -> LuaConst (LuaNumber 1.))
    >>= fun loop_step ->
    match loop_begin.last_exec, loop_end.last_exec, loop_step.last_exec with
    | LuaNumber lbegin, LuaNumber lend, LuaNumber lstep ->
      let rec loop_iter iter ctx =
        match
          (if lstep > 0. then ( <= ) else ( >= )) (lbegin +. (iter *. lstep)) lend
        with
        | false -> return ctx
        | true ->
          exec_local_set [ id ] [ LuaConst (LuaNumber (lbegin +. (iter *. lstep))) ]
          >>= fun _ -> exec_many blck >>= fun nctx -> loop_iter (iter +. 1.) nctx
      in
      loop_iter 0. loop_step
    | _, _, _ -> error "End, begin and step of the loop must be numbers"

  and exec_while ex blck =
    exec_expr ex
    >>= fun ctx_e ->
    match get_bool ctx_e.last_exec with
    | true -> exec_many blck >>= fun _ -> exec_while ex blck
    | false -> return ctx_e

  and exec_until blck ex =
    exec_many blck
    >>= fun _ ->
    exec_expr ex
    >>= fun ctx_e ->
    match get_bool ctx_e.last_exec with
    | true -> exec_until blck ex
    | false -> return ctx_e

  and exec_break ctx =
    let rec find_ctx lvl c =
      if c.level = lvl
      then Breaking c
      else if c.level < lvl
      then Error "breaking outside a loop"
      else (
        match c.previous with
        | Some p -> find_ctx lvl p
        | None -> Error "trying to exit global context")
    in
    match ctx.brk with
    | Some lvl -> find_ctx lvl ctx
    | None -> Error "breaking outside a loop"

  and exec_return ex =
    exec_expr
      (match ex with
       | Some e -> e
       | None -> LuaConst LuaNil)
    >>= fun ctx ->
    let rec find_ctx lvl c =
      if c.level = lvl
      then Returning { c with last_exec = ctx.last_exec }
      else if c.level < lvl
      then Error "returning outside a function"
      else (
        match c.previous with
        | Some p -> find_ctx lvl p
        | None -> Error "trying to exit global context")
    in
    match ctx.ret with
    | Some lvl -> fun _ -> find_ctx lvl ctx
    | None -> error "returning outside a function"

  and get_bool aexpr =
    match aexpr with
    | LuaBool false | LuaNil -> false
    | _ -> true

  and exec_if iex blck elseifs elseblck =
    exec_expr iex
    >>= fun if_ex_res ->
    match get_bool if_ex_res.last_exec with
    | true -> exec_many blck
    | _ ->
      (match elseifs with
       | [] ->
         (match elseblck with
          | Some sttmnts -> exec_many sttmnts
          | None -> exec_many [])
       | h :: t ->
         (match h with
          | iex, blck -> exec_if iex blck t elseblck))

  and exec_set ce re ctx =
    let rec helper cl rl ctx =
      match cl, rl with
      | ch :: ct, rh :: rt ->
        exec_expr rh
        >>= fun rhr -> exec_set_one ch rhr.last_exec >>= fun nctx -> helper ct rt nctx
      | ch :: ct, [] -> exec_set_one ch LuaNil >>= fun nctx -> helper ct rl nctx
      | [], _ -> return ctx
    in
    helper ce re ctx ctx

  and exec_set_one ce re ctx =
    let rec replace_ident i ctx =
      match ctx.previous with
      | None -> { ctx with vars = VarsMap.replace i re ctx.vars }
      | Some prev ->
        (match VarsMap.find_opt i ctx.vars with
         | None -> { ctx with previous = Some (replace_ident i prev) }
         | Some _ -> { ctx with vars = VarsMap.replace i re ctx.vars })
    in
    let rec apply_to_ctx f lval ctx =
      match lval with
      | Ident i ->
        (match VarsMap.find_opt i ctx.vars with
         | Some _ -> f ctx
         | None ->
           (match ctx.previous with
            | Some prev ->
              (match apply_to_ctx f (Ident i) prev with
               | Interpreted nctx -> Interpreted { ctx with previous = Some nctx }
               | x -> x)
            | None -> Error "Attempting to index nil"))
      | Index (p, _) -> apply_to_ctx f p ctx
    in
    let rec indexes_to_list index ctx =
      match index with
      | Index (p, i) ->
        (match exec_expr i ctx with
         | Error m -> "", [], Error m
         | Interpreted nctx ->
           let next_res = indexes_to_list p nctx in
           let e1, e2, e3 = next_res in
           e1, nctx.last_exec :: e2, e3
         | _ -> "", [], Error "Breaking or returning still..")
      | Ident i -> i, [], Interpreted ctx
    in
    let replace_index index ctx =
      let primal_id, const_lst, ctx = indexes_to_list index ctx in
      let rec replace_in_table klst tbl =
        match klst with
        | h :: [] -> Some (LuaTableMap.replace h re tbl)
        | h :: t ->
          (match LuaTableMap.find_opt h tbl with
           | Some (LuaTable ntbl) ->
             (match replace_in_table t ntbl with
              | Some nntbl -> Some (LuaTableMap.replace h (LuaTable nntbl) tbl)
              | None -> None)
           | _ -> None)
        | [] -> Some tbl
      in
      let f ctx =
        match VarsMap.find_opt primal_id ctx.vars with
        | Some tbl ->
          (match tbl with
           | LuaTable tbl ->
             (match replace_in_table (List.rev const_lst) tbl with
              | Some tbl ->
                Interpreted
                  { ctx with vars = VarsMap.replace primal_id (LuaTable tbl) ctx.vars }
              | None -> Error "Attempt to index non table")
           | _ -> Error "Attempt to index non table")
        | None -> Error "Attempt to index nil"
      in
      match ctx with
      | Interpreted ctx -> apply_to_ctx f (Ident primal_id) ctx
      | e -> e
    in
    match ce with
    | Ident ident -> Interpreted (replace_ident ident ctx)
    | index -> replace_index index ctx

  and exec_many stmnts ctx =
    match stmnts with
    | [] -> Interpreted ctx
    | h :: t -> (exec_stat h >>= fun _ -> exec_many t) ctx
  ;;

  let emptyctx =
    { vars = VarsMap.empty
    ; previous = None
    ; last_exec = LuaNil
    ; ret = None
    ; brk = None
    ; level = 0
    }
  ;;

  let interpret ast = exec_many ast
end
