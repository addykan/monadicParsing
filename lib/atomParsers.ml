open! SimpleParsers
open! Grammar
open! Core

let trueVal = word (String.to_list "true")
let falseVal = word (String.to_list "false")

let makeBoolValue =
  (trueVal >>= fun _ r -> Some (Value (MyBool true), r))
  ++ (falseVal >>= fun _ r -> Some (Value (MyBool false), r))
;;

let makeNatValue = nat >>= fun x r -> Some (Value (MyInt x), r)
let valueExpr = makeBoolValue ++ makeNatValue

let%expect_test "trueTest" =
  let res = makeBoolValue (String.to_list "true") in
  (match res with
   | Some (parsedTrue, _) -> print_endline (Sexp.to_string (sexp_of_expr parsedTrue))
   | None -> print_endline "failed to parse");
  [%expect {| (Value(MyBool true)) |}]
;;

let%expect_test "falseTest" =
  let res = makeBoolValue (String.to_list "false") in
  (match res with
   | Some (parsedFalse, _) -> print_endline (Sexp.to_string (sexp_of_expr parsedFalse))
   | None -> print_endline "failed to parse");
  [%expect {| (Value(MyBool false)) |}]
;;

let%expect_test "intTest" =
  let res = makeNatValue (String.to_list "369") in
  (match res with
   | Some (parsedInt, _) -> print_endline (Sexp.to_string (sexp_of_expr parsedInt))
   | None -> print_endline "failed to parse");
  [%expect {| (Value(MyInt 369)) |}]
;;

let%expect_test "valueTest" =
  let res = valueExpr (String.to_list "369") in
  (match res with
   | Some (parsedInt, _) -> print_endline (Sexp.to_string (sexp_of_expr parsedInt))
   | None -> print_endline "failed to parse");
  [%expect {| (Value(MyInt 369)) |}]
;;

let%expect_test "valueTest" =
  let res = valueExpr (String.to_list "true") in
  (match res with
   | Some (parsedInt, _) -> print_endline (Sexp.to_string (sexp_of_expr parsedInt))
   | None -> print_endline "failed to parse");
  [%expect {| (Value(MyBool true)) |}]
;;

let plusExpr =
  makeNatValue
  >>= fun x r ->
  (exactChar '+' >> makeNatValue >>= fun y rest -> Some (Plus (x, y), rest)) r
;;

let%expect_test "plusTest" =
  let res = plusExpr (String.to_list "369+248") in
  (match res with
   | Some (parsedRes, _) -> print_endline (Sexp.to_string (sexp_of_expr parsedRes))
   | None -> print_endline "failed to parse");
  [%expect {| (Plus(Value(MyInt 369))(Value(MyInt 248))) |}]
;;

let eqExpr =
  makeNatValue
  >>= fun x r ->
  (exactChar '=' >> makeNatValue >>= fun y rest -> Some (Eq (x, y), rest)) r
;;

let%expect_test "eqTest" =
  let res = eqExpr (String.to_list "369=248") in
  (match res with
   | Some (parsedRes, _) -> print_endline (Sexp.to_string (sexp_of_expr parsedRes))
   | None -> print_endline "failed to parse");
  [%expect {| (Eq(Value(MyInt 369))(Value(MyInt 248))) |}]
;;

let%expect_test "bigTest" =
  let res = eqExpr (String.to_list "369=248") in
  (match res with
   | Some (parsedRes, _) -> print_endline (Sexp.to_string (sexp_of_expr parsedRes))
   | None -> print_endline "failed to parse");
  [%expect {| (Eq(Value(MyInt 369))(Value(MyInt 248))) |}]
;;

type inputData = char list [@@deriving sexp]
type parseOutput = (char list * char list) option [@@deriving sexp]

let ifExact x =
  let res = word (String.to_list "if ") x in
  res
;;

(* let thenExact = word (String.to_list " then ") *)
let thenExact x =
  let res = word (String.to_list " then ") x in
  res
;;

let elseExact = word (String.to_list " else ")

let ifExpr =
  ifExact
  >> valueExpr
  >>= fun cond r ->
  (thenExact
   >> valueExpr
   >>= fun thenVal rest ->
   (elseExact
    >> valueExpr
    >>= fun elseVal final -> Some (If (cond, thenVal, elseVal), final))
     rest)
    r
;;

let%expect_test "ifTest" =
  let res = ifExpr (String.to_list "if true then 5 else 6") in
  (match res with
   | Some (parsedRes, _) -> print_endline (Sexp.to_string (sexp_of_expr parsedRes))
   | None -> print_endline "failed to parse");
  [%expect {| (If(Value(MyBool true))(Value(MyInt 5))(Value(MyInt 6))) |}]
;;
