open! Base
(*
   type _ value =
   | MyInt : int -> int value
   | MyBool : bool -> bool value *)

(* type item = MyInt of int | MyBool of bool *)

(* type expr =
   Value of item
   | If of item * item * item
   | Plus of item * item
   | Eq of item * item *)

type _ expr =
  | Value : 'a -> 'a expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Plus : int expr * int expr -> int expr
  | Eq : int expr * int expr -> bool expr

let rec eval : type a. a expr -> a = function
  | Value v -> v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Plus (x, y) -> eval x + eval y
  | Eq (x, y) -> eval x = eval y
;;
