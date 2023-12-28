(* open! Base *)

module type Monad = sig
  type 'a parser = char list -> ('a * char list) option
  type 'a m = 'a parser

  val return : 'a -> 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
end

module ParserM : Monad = struct
  (* open Core *)
  type 'a parser = char list -> ('a * char list) option [@@deriving sexp]
  type 'a m = 'a parser

  let return x (i : char list) = Some (x, i)

  let ( >>= ) p f i =
    match p i with
    | Some (x, r) -> f x r
    | None -> None
  ;;
end
[@@deriving sexp]

let ( >>= ) p f i =
  match p i with
  | Some (x, r) -> f x r
  | None -> None
;;

let zero _ = None

let item cList =
  match cList with
  | [] -> None
  | c :: cs -> Some (c, cs)
;;

let sat (p : char -> bool) : char ParserM.m =
  ParserM.( >>= ) item (fun x -> if p x then ParserM.return x else zero)
;;

let ( %% ) p s =
  let open! Base in
  p (String.to_list s)
;;

let ( ++ ) p q i =
  match p i with
  | None -> q i
  | v -> v
;;

let in_range (x, y) c =
  let open! Char in
  x <= c && c <= y
;;

let digit = sat (in_range ('0', '9'))
let lower = sat (in_range ('a', 'z'))
let upper = sat (in_range ('A', 'Z'))
let alpha = lower ++ upper
let alphanum = alpha ++ digit

let exactChar c =
  sat
    (let open! Char in
     fun x -> x = c)
;;

let ( >> ) p q = ParserM.( >>= ) p (fun _ -> q)
let ( << ) p q = ParserM.( >>= ) p (fun x -> q >> fun r -> ParserM.return x r)

let rec word cs : char list ParserM.m =
  match cs with
  | [] -> ParserM.return []
  | c :: cs ->
    ParserM.( >>= ) (exactChar c) (fun x ->
      ParserM.( >>= ) (word cs) (fun xs -> ParserM.return (x :: xs)))
;;

let rec many1 p =
  ParserM.( >>= ) p (fun x ->
    ParserM.( >>= ) (many1 p ++ ParserM.return []) (fun xs -> ParserM.return (x :: xs)))
;;

let many p = many1 p ++ ParserM.return []

let nat =
  let toNum i =
    let () = Stdio.print_endline (Char.escaped i) in
    Char.code i - Char.code '0'
  in
  let eval = List.fold_left (fun acc i -> (10 * acc) + toNum i) 0 in
  ParserM.( >>= ) (many1 digit) (fun ds -> ParserM.return (eval ds))
;;

let intParser =
  let neg x = -x in
  (exactChar '-' >> ParserM.return neg) ++ ParserM.return (fun x -> x)
  >>= fun f -> nat >>= fun n -> ParserM.return (f n)
;;

open Core

type parseOutput = (char list * char list) option [@@deriving sexp]

let parsedTest = word [ 't'; 'e'; 's'; 't' ] %% "testb"

let%expect_test "parser" =
  print_endline (Sexp.to_string (sexp_of_parseOutput parsedTest));
  [%expect {| (((t e s t)())) |}]
;;
