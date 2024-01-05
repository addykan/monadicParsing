{ 
open! Core
open GrammarParser
open Lexing
exception SyntaxError of string
}

let int = '-'? ['0' - '9'] ['0' - '9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let true = "true"
let false = "false"
let plus = "+"
let eq = "="
let if = "if" 
let then = "then"
let else = "else"

rule read = 
  parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | eof { EOF }
  | _ {raise (SyntaxError "Unexpected character in buffer")}