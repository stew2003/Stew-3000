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
| "/*"('*'[^'/']|'/'|[^'*''/'])*?"*/" as comment
  { 
    (* count the number of newlines in the multiline 
       comment and apply them to the lexbuf. *)
    let lines = String.split_on_char '\n' comment in 
    for _ = 1 to (List.length lines) - 1 do 
      Lexing.new_line lexbuf;
    done;
    token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| '-'?['0'-'9']+ as i
  { NUMLIT (int_of_string i, loc_from_lexbuf lexbuf) }
| '-'?"0x"['0'-'9''a'-'f''A'-'F']+ as hex
  { NUMLIT (int_of_string hex, loc_from_lexbuf lexbuf) }
| '-'?"0b"['0' '1']+ as binary
  { NUMLIT (int_of_string binary, loc_from_lexbuf lexbuf) }
| "'"_"'" as character
  { CHARLIT ((String.get character 1), loc_from_lexbuf lexbuf) }
| '"'[^'"''\n']*'"' as str
  { STRINGLIT (String.sub str 1 (String.length str - 2), loc_from_lexbuf lexbuf) }
| '('
  { LPAREN (loc_from_lexbuf lexbuf) }
| ')'
  { RPAREN (loc_from_lexbuf lexbuf) }
| '{'
  { LBRACE (loc_from_lexbuf lexbuf) }
| '}'
  { RBRACE (loc_from_lexbuf lexbuf) }
| '['
  { LBRACKET (loc_from_lexbuf lexbuf) }
| ']'
  { RBRACKET (loc_from_lexbuf lexbuf) }
| ';'
  { SEMICOLON (loc_from_lexbuf lexbuf) }
| ','
  { COMMA (loc_from_lexbuf lexbuf) }
| '+'
  { PLUS (loc_from_lexbuf lexbuf) }
| '-'
  { MINUS (loc_from_lexbuf lexbuf) }
| '*'
  { STAR (loc_from_lexbuf lexbuf) }
| '/'
  { DIV (loc_from_lexbuf lexbuf) }
| '%'
  { MOD (loc_from_lexbuf lexbuf) }
| "&&"
  { LAND (loc_from_lexbuf lexbuf) }
| "||"
  { LOR (loc_from_lexbuf lexbuf) }
| '&'
  { AMPERSAND (loc_from_lexbuf lexbuf) }
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
| "+="
  { PLUS_UPDATE (loc_from_lexbuf lexbuf) }
| "-="
  { MINUS_UPDATE (loc_from_lexbuf lexbuf) }
| "*="
  { TIMES_UPDATE (loc_from_lexbuf lexbuf) }
| "/="
  { DIV_UPDATE (loc_from_lexbuf lexbuf) }
| "%="
  { MOD_UPDATE (loc_from_lexbuf lexbuf) }
| "&="
  { BAND_UPDATE (loc_from_lexbuf lexbuf) }
| "|="
  { BOR_UPDATE (loc_from_lexbuf lexbuf) }
| "^="
  { BXOR_UPDATE (loc_from_lexbuf lexbuf) }
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
| "print_lcd"
  { PRINTLCD (loc_from_lexbuf lexbuf) }
| "exit"
  { EXIT (loc_from_lexbuf lexbuf) }
| "assert"
  { ASSERT (loc_from_lexbuf lexbuf) }
| "void"
  { VOID (loc_from_lexbuf lexbuf) }
| "int"
  { INT (loc_from_lexbuf lexbuf) }
| "unsigned"
  { UNSIGNED (loc_from_lexbuf lexbuf) }
| "char"
  { CHAR (loc_from_lexbuf lexbuf) }
| "#define"
  { DEFINE (loc_from_lexbuf lexbuf) }
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
