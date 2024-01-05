open! Core
(* type _ expr =
   | Value : 'a -> 'a expr
   | If : bool expr * 'a expr * 'a expr -> 'a expr
   | Plus : int expr * int expr -> int expr
   | Eq : 'a expr * 'a expr -> bool expr

   type astExpr =
   | T : _ expr -> astExpr *)

type item =
  | MyInt of int
  | MyBool of bool
[@@deriving sexp]

type expr =
  | Value of item
  | If of expr * expr * expr
  | Plus of expr * expr
  | Eq of expr * expr
[@@deriving sexp]
