open! Base
open Core
open! Scrapwork.Grammar
open Scrapwork.SimpleParsers

(* let inputStr = "if 5 + 7 == 12 then 6 else 7" *)

let intString = "-12345"

let parsedInt, _ =
  match intParser %% intString with
  | Some (i, l) -> i, l
  | None -> -1, []
;;

let () = Stdio.print_endline (Int.to_string parsedInt)
let my_expr = Plus (Value 5, Value 32)
let res = eval my_expr
let () = Stdio.print_endline (Int.to_string res)



