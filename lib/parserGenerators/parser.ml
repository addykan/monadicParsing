open! Core
open GrammarLexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf =
  try GrammarParser.prog GrammarLexer.read lexbuf with
  | SyntaxError msg ->
    print_endline "syntax error";
    printf "%a: %s\n" print_position lexbuf msg;
    None
  | GrammarParser.Error ->
    print_endline "failing parse_with_error None";
    printf "%a: syntax error\n" print_position lexbuf;
    None
;;

(* exit (-1) *)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    print_endline "fail in parse_and_print";
    print_endline (Sexp.to_string (Grammar.sexp_of_expr value));
    parse_and_print lexbuf
  | None -> ()
;;

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx
;;

(* let _ = loop "/Users/addykan/Github/monadicParsing/lib/parserGenerators/test.txt" () *)

let%expect_test "basicParserTest" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "5") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Value(MyInt 5)) |}]
;;

let%expect_test "testPlusNoWhitespace" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "5+5") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Plus(Value(MyInt 5))(Value(MyInt 5))) |}]
;;

let%expect_test "testEqNoWhitespace" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "4=3") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Eq(Value(MyInt 4))(Value(MyInt 3))) |}]
;;

let%expect_test "testPlusWithWhitespace" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "5 + 5") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Plus(Value(MyInt 5))(Value(MyInt 5))) |}]
;;

let%expect_test "testEqWithWhitespace" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "4 = 3") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Eq(Value(MyInt 4))(Value(MyInt 3))) |}]
;;

let%expect_test "testConditional" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "if 5 = 3 then 4 + 5 else 3 + 9") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Plus(If(Eq(Value(MyInt 5))(Value(MyInt 3)))(Plus(Value(MyInt 4))(Value(MyInt 5)))(Value(MyInt 3)))(Value(MyInt 9))) |}]
;;

let%expect_test "testSimpleParen" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "(5)") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Value(MyInt 5)) |}]
;;

let%expect_test "testSimpleMathExpr" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "4 + (5 + 3)") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Plus(Value(MyInt 4))(Plus(Value(MyInt 5))(Value(MyInt 3)))) |}]
;;

let%expect_test "testConditionalNoParens" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "if 5 = 3 then 9 else 3 + 5") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Plus(If(Eq(Value(MyInt 5))(Value(MyInt 3)))(Value(MyInt 9))(Value(MyInt 3)))(Value(MyInt 5))) |}]
;;

let%expect_test "testConditional" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "if (5 = 3) then 9 else (3 + 5)") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (If(Eq(Value(MyInt 5))(Value(MyInt 3)))(Value(MyInt 9))(Plus(Value(MyInt 3))(Value(MyInt 5)))) |}]
;;

let%expect_test "testNestedParens" =
  (* let inx = In_channel.create "./test.txt" in
  let lexbuf = Lexing.from_channel inx in *)
  let parsedInt = parse_with_error (Lexing.from_string "(5 + (if 5=4 then 9 else 12))") in
  (match parsedInt with
   | Some res -> print_endline (Sexp.to_string (Grammar.sexp_of_expr res))
   | None -> print_endline "failed to parse");
  [%expect {| (Plus(Value(MyInt 5))(If(Eq(Value(MyInt 5))(Value(MyInt 4)))(Value(MyInt 9))(Value(MyInt 12)))) |}]
;;


