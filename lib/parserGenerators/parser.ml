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
    print_endline "fail";
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
  [%expect {| (((t e s t)(b))) |}]
;;
