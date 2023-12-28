module type Monad = sig
  type 'a parser = char list -> ('a * char list) option
  type 'a m = 'a parser

  val return : 'a -> 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
end

module ParserM : sig
  type 'a parser = char list -> ('a * char list) option
  type 'a m
end

val ( >>= ) : 'a ParserM.parser -> ('a -> 'b ParserM.parser) -> 'b ParserM.parser
val ( ++ ) : 'a ParserM.parser -> 'a ParserM.parser -> 'a ParserM.parser
val ( %% ) : 'a ParserM.parser -> string -> ('a * char list) option
val ( >> ) : 'a ParserM.parser -> 'b ParserM.parser -> 'b ParserM.parser
val ( << ) : 'a ParserM.parser -> 'b ParserM.parser -> 'a ParserM.parser
val word : char list -> char list ParserM.parser
val many1 : 'a ParserM.parser -> 'a list ParserM.parser
val many : 'a ParserM.parser -> 'a list ParserM.parser
val nat : int ParserM.parser
val alphanum : char ParserM.parser
val intParser : int ParserM.parser
