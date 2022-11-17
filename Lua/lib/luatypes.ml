(** Copyright 2021-2022, Arthur Alekseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type luatype =
  | Variable of string
  | Number of string
  | String of string
  | Keyword of string
  | Comment of string
  | Operator of string
  | Spaces of string
  | LParentheses
  | RParentheses
  | LBrackets
  | RBrackets
  | LCurlBrackets
  | RCurlBrackets
  | Comma
  | Dot
  | Semicolon
  | EOF

let luakeywords =
  [ "and"
  ; "break"
  ; "do"
  ; "else"
  ; "elseif"
  ; "end"
  ; "false"
  ; "for"
  ; "function"
  ; "if"
  ; "in"
  ; "local"
  ; "nil"
  ; "not"
  ; "or"
  ; "repeat"
  ; "return"
  ; "then"
  ; "true"
  ; "until"
  ; "while"
  ]
;;

let operators =
  [ "and"; "or"; "+"; "-"; "/"; "*"; ">="; "<="; ">"; "<"; "=="; "^"; "not"; ".."; "=" ]
;;
