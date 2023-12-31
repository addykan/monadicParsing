(* open Base

   type value =
   Int of int
   | Bool of bool

   type expr =
   | Value of value
   | If of expr * expr * expr
   | Plus of expr * expr
   | Eq of expr * expr

   exception Ill_typed

   let rec eval expr =
   match expr with
   Value v -> v
   | If (c, t, e) -> (match eval c with Bool b -> if b then eval t else eval e | Int _ -> raise Ill_typed)
   | Plus (v1, v2) -> (
   match eval v1, eval v2 with
   Int x, Int y -> Int (x + y)
   | _ -> raise Ill_typed
   )
   | Eq (v1, v2) ->
   match eval v1, eval v2 with
   Int x, Int y -> Bool (x = y)
   | Bool x, Bool y -> Bool ((x && y ) || (not x && not y))
   | _ -> raise Ill_typed

   let value_to_string (v: value) =
   match v with
   Int x -> Int.to_string x
   | Bool b -> if b then "true" else "false"
*)

(* open! Base

   type _ value =
   MyInt : int -> int value
   | MyBool : bool -> bool value

   type _ expr =
   Value : 'a -> 'a expr
   | If : bool expr * 'a expr * 'a expr -> 'a expr
   | Plus : int expr * int expr -> int expr
   | Eq : int expr * int expr -> bool expr

   let rec eval : type a. a expr -> a = function
   Value v -> v
   | If (c, t, e) -> if eval c then eval t else eval e
   | Plus (x, y) -> eval x + eval y
   | Eq (x, y) -> eval x = eval y *)
