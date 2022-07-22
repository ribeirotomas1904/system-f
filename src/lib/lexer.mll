{
open Parser

exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let lower_letter = ['a'-'z']
let upper_letter = ['A'-'Z']
let letter = lower_letter | upper_letter

let ident = lower_letter (lower_letter | '_')*
let upper_ident = upper_letter (letter '_')*
let int = '-'? ['0'-'9'] ['0'-'9']*

rule read =
  parse
  | white { read lexbuf }
  | newline { read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '=' { EQUALS }
  | ':' { COLON }
  | '*' { ASTERISK }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '.' { DOT }
  | "->" { ARROW }
  | "let" { LET }
  | "in" { IN }
  | "type" { TYPE }
  | "fun" { FUN }
  | "forall" { FORALL }
  | "Int" { T_INT }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | upper_ident { UPPER_IDENT (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
