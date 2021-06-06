{
  open Parse
  open Util.Srcloc
  exception Error of string * src_loc
}

rule token = parse
| [' ' '\t']+
  { token lexbuf }
| "//"[^'\n']*?
  { token lexbuf }
| "/*"_*?"*/"
  { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| '-'?['0'-'9']+ as i
  { NUM (int_of_string i, loc_from_lexbuf lexbuf) }
| '-'?"0x"['0'-'9''a'-'f''A'-'F']+ as hex
  { NUM (int_of_string hex, loc_from_lexbuf lexbuf) }
| '-'?"0b"['0' '1']+ as binary
  { NUM (int_of_string binary, loc_from_lexbuf lexbuf) }
| '('
  { LPAREN (loc_from_lexbuf lexbuf) }
| ')'
  { RPAREN (loc_from_lexbuf lexbuf) }
| '{'
  { LBRACE (loc_from_lexbuf lexbuf) }
| '}'
  { RBRACE (loc_from_lexbuf lexbuf) }
| ';'
  { SEMICOLON (loc_from_lexbuf lexbuf) }
| ','
  { COMMA (loc_from_lexbuf lexbuf) }
| '+'
  { PLUS (loc_from_lexbuf lexbuf) }
| '-'
  { MINUS (loc_from_lexbuf lexbuf) }
| '*'
  { TIMES (loc_from_lexbuf lexbuf) }
| '/'
  { DIV (loc_from_lexbuf lexbuf) }
| '%'
  { MOD (loc_from_lexbuf lexbuf) }
| "&&"
  { LAND (loc_from_lexbuf lexbuf) }
| "||"
  { LOR (loc_from_lexbuf lexbuf) }
| '&'
  { BAND (loc_from_lexbuf lexbuf) }
| '|'
  { BOR (loc_from_lexbuf lexbuf) }
| '^'
  { BXOR (loc_from_lexbuf lexbuf) }
| '>'
  { GT (loc_from_lexbuf lexbuf) }
| '<'
  { LT (loc_from_lexbuf lexbuf) }
| ">="
  { GTE (loc_from_lexbuf lexbuf) }
| "<="
  { LTE (loc_from_lexbuf lexbuf) }
| "=="
  { EQ (loc_from_lexbuf lexbuf) }
| "!="
  { NEQ (loc_from_lexbuf lexbuf) }
| '~'
  { BNOT (loc_from_lexbuf lexbuf) }
| '!'
  { LNOT (loc_from_lexbuf lexbuf) }
| '='
  { ASSIGN (loc_from_lexbuf lexbuf) }
| "++"
  { INR (loc_from_lexbuf lexbuf) }
| "--"
  { DCR (loc_from_lexbuf lexbuf) }
| "if"
  { IF (loc_from_lexbuf lexbuf) }
| "else"
  { ELSE (loc_from_lexbuf lexbuf) }
| "while"
  { WHILE (loc_from_lexbuf lexbuf) }
| "return"
  { RETURN (loc_from_lexbuf lexbuf) }
| "print"
  { PRINT (loc_from_lexbuf lexbuf) }
| "exit"
  { EXIT (loc_from_lexbuf lexbuf) }
| "assert"
  { ASSERT (loc_from_lexbuf lexbuf) }
| "int"
  { INT (loc_from_lexbuf lexbuf) }
| "void"
  { VOID (loc_from_lexbuf lexbuf) }
| ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* as name
    { IDENT (name, (loc_from_lexbuf lexbuf)) }
| eof
    { EOF }
| _
    { raise (Error 
      ((Printf.sprintf 
        "unexpected character '%s' at position %d:%d" 
        (String.escaped (Lexing.lexeme lexbuf))
        (Lexing.lexeme_start_p lexbuf).pos_lnum
        (Lexing.lexeme_start lexbuf)), 
      (loc_from_lexbuf lexbuf))) }
