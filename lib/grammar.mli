(* type _ value =
  | MyInt : int -> int value
  | MyBool : bool -> bool value *)

(* type item = MyInt of int | MyBool of bool *)

type _ expr =
  | Value : 'a -> 'a expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Plus : int expr * int expr -> int expr
  | Eq : int expr * int expr -> bool expr

(* type expr = 
    Value of item
  | If of item * item * item
  | Plus of item * item
  | Eq of item * item *)

val eval : 'a expr -> 'a
