%{
  open Ast
  open Util.Srcloc
%}

%token <int * Util.Srcloc.src_loc> NUMLIT
%token <char * Util.Srcloc.src_loc> CHARLIT
%token <string * Util.Srcloc.src_loc> STRINGLIT
%token <string * Util.Srcloc.src_loc> IDENT

%token <Util.Srcloc.src_loc> LPAREN
%token <Util.Srcloc.src_loc> RPAREN
%token <Util.Srcloc.src_loc> LBRACE
%token <Util.Srcloc.src_loc> RBRACE
%token <Util.Srcloc.src_loc> LBRACKET
%token <Util.Srcloc.src_loc> RBRACKET
%token <Util.Srcloc.src_loc> SEMICOLON
%token <Util.Srcloc.src_loc> COMMA

%token <Util.Srcloc.src_loc> PLUS
%token <Util.Srcloc.src_loc> MINUS
%token <Util.Srcloc.src_loc> STAR
%token <Util.Srcloc.src_loc> DIV
%token <Util.Srcloc.src_loc> MOD
%token <Util.Srcloc.src_loc> AMPERSAND
%token <Util.Srcloc.src_loc> BOR
%token <Util.Srcloc.src_loc> BXOR
%token <Util.Srcloc.src_loc> LAND
%token <Util.Srcloc.src_loc> LOR
%token <Util.Srcloc.src_loc> GT
%token <Util.Srcloc.src_loc> LT
%token <Util.Srcloc.src_loc> GTE
%token <Util.Srcloc.src_loc> LTE
%token <Util.Srcloc.src_loc> EQ
%token <Util.Srcloc.src_loc> NEQ

%token <Util.Srcloc.src_loc> BNOT
%token <Util.Srcloc.src_loc> LNOT

%token <Util.Srcloc.src_loc> ASSIGN
%token <Util.Srcloc.src_loc> INR
%token <Util.Srcloc.src_loc> DCR
%token <Util.Srcloc.src_loc> PLUS_UPDATE
%token <Util.Srcloc.src_loc> MINUS_UPDATE
%token <Util.Srcloc.src_loc> TIMES_UPDATE
%token <Util.Srcloc.src_loc> DIV_UPDATE
%token <Util.Srcloc.src_loc> MOD_UPDATE
%token <Util.Srcloc.src_loc> BAND_UPDATE
%token <Util.Srcloc.src_loc> BOR_UPDATE
%token <Util.Srcloc.src_loc> BXOR_UPDATE

%token <Util.Srcloc.src_loc> IF
%token <Util.Srcloc.src_loc> ELSE
%token <Util.Srcloc.src_loc> WHILE
%token <Util.Srcloc.src_loc> RETURN

%token <Util.Srcloc.src_loc> PRINT
%token <Util.Srcloc.src_loc> EXIT
%token <Util.Srcloc.src_loc> ASSERT

%token <Util.Srcloc.src_loc> INT
%token <Util.Srcloc.src_loc> VOID
%token <Util.Srcloc.src_loc> UNSIGNED
%token <Util.Srcloc.src_loc> CHAR

%token <Util.Srcloc.src_loc> DEFINE

%token EOF

%start <pp_define list * func_defn list> program

%right ASSIGN PLUS_UPDATE MINUS_UPDATE TIMES_UPDATE DIV_UPDATE MOD_UPDATE BAND_UPDATE BOR_UPDATE BXOR_UPDATE
%left LOR
%left LAND
%left BOR
%left BXOR
%nonassoc AMPERSAND
%left EQ NEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left STAR DIV MOD
%right BNOT LNOT
%nonassoc INR DCR

%%

// a program is a tuple of preprocessor #defines and function definitions
program:
| defines = list(define) defns = list(definition) EOF
  { (defines, defns) }

define:
| start_loc = DEFINE var = IDENT expression = expr
  {
    let (var, _) = var in 
    let (expression, end_loc) = expression in 
    {
      var;
      expression;
      loc= Some (span start_loc end_loc);
    }
  }

definition:
| fn = decl LPAREN params = params RPAREN 
  LBRACE body = stmt_list end_loc = RBRACE
  {
    let (name, return_ty, start_loc) = fn in 
    let params = List.map (fun (name, typ, _) -> (name, typ)) params in
    {
      name;
      params;
      body;
      return_ty;
      ctrl_reaches_end= None;
      loc= Some (span start_loc end_loc);
    }
  }

params:
| p = separated_list(COMMA, decl)
  { p }

typ:
| loc = VOID
  { (Void, loc) }
| loc = INT
  { (Int, loc) }
| loc = UNSIGNED
  { (Unsigned, loc) }
| loc = CHAR 
  { (Char, loc) }
| t = typ STAR
  {
    let (t, loc) = t in 
    (Pointer t, loc)
  }

// variable declaration, type then name
decl:
| t = typ name = IDENT
  { 
    let (t, start_loc) = t in 
    let (name, end_loc) = name in
    (name, t, span start_loc end_loc)
  }

// array initializers
array_init:
| ASSIGN s = STRINGLIT
  {
    let (s, loc) = s in 
    (* Convert string literal to null-terminated list of chars *)
    let chars = s |> String.to_seq |> List.of_seq in 
    let chars = List.rev ('\x00' :: (List.rev chars)) in 
    List.map (fun c -> CharLiteral (c, Some loc)) chars
  }
| ASSIGN LBRACE es = separated_list(COMMA, expr) RBRACE
  {
    List.map (fun (e, _) -> e) es
  }

stmt_list:
| stmts = list(stmt_in_block)
  { stmts }

// statement inside a block can be either simple stmt with semicolon,
// or block statement (if, while, etc)
stmt_in_block:
| stmt = stmt SEMICOLON
  { stmt }
| stmt = block_stmt
  { stmt }

// statements that do not require semicolon termination
block_stmt:
| d = decl ASSIGN e = expr end_loc = SEMICOLON scope = stmt_list
  {
    let (name, typ, start_loc) = d in 
    let (e, _) = e in 
    Declare(name, typ, Some e, scope, Some (span start_loc end_loc))
  }
| d = decl end_loc = SEMICOLON scope = stmt_list
  {
    let (name, typ, start_loc) = d in 
    Declare (name, typ, None, scope, Some (span start_loc end_loc))
  }
| d = decl LBRACKET size = option(expr) RBRACKET init = option(array_init) 
  end_loc = SEMICOLON scope = stmt_list
  {
    let (name, typ, start_loc) = d in 
    let size = Option.map (fun (e, _) -> e) size in
    let loc = span start_loc end_loc in  
    ArrayDeclare (name, typ, size, init, scope, Some loc)
  }
| start_loc = IF LPAREN cond = expr RPAREN 
  LBRACE thn = stmt_list end_loc = RBRACE
  { let (cond, _) = cond in 
    If (cond, thn, Some (span start_loc end_loc)) }
| start_loc = IF LPAREN cond = expr RPAREN LBRACE thn = stmt_list RBRACE 
  ELSE LBRACE els = stmt_list end_loc = RBRACE
  { let (cond, _) = cond in 
    IfElse (cond, thn, els, Some (span start_loc end_loc)) }
| start_loc = LBRACE stmts = stmt_list end_loc = RBRACE
  { Block (stmts, Some (span start_loc end_loc)) }
| start_loc = WHILE LPAREN cond = expr RPAREN LBRACE body = stmt_list end_loc = RBRACE
  { let (cond, _) = cond in 
    While (cond, body, Some (span start_loc end_loc)) }

// statements that require semicolon termination
stmt:
| start_loc = RETURN e = option(expr) 
  {
    let e, end_loc = (match e with 
      | None -> (None, start_loc)
      | Some (e, end_loc) -> (Some e, end_loc)
    ) in 
    Return (e, Some (span start_loc end_loc))
  }
| e = expr 
  { let (e, loc) = e in 
    ExprStmt (e, Some loc) }
| start_loc = PRINT LPAREN arg = expr end_loc = RPAREN
  { let (arg, _) = arg in 
    PrintDec (arg, Some (span start_loc end_loc)) }
| start_loc = EXIT LPAREN arg = option(expr) end_loc = RPAREN
  { let arg = Option.map (fun (a, _) -> a) arg in 
    Exit (arg, Some (span start_loc end_loc)) }
| start_loc = ASSERT LPAREN e = expr end_loc = RPAREN 
  { 
    let (e, _) = e in 
    Assert (e, Some (span start_loc end_loc))
  }

// parses an update (*=, +=, ...) and returns corresponding bin_op
update:
| PLUS_UPDATE
  { Plus }
| MINUS_UPDATE
  { Minus }
| TIMES_UPDATE
  { Mult }
| DIV_UPDATE
  { Div }
| MOD_UPDATE
  { Mod }
| BAND_UPDATE
  { BAnd }
| BOR_UPDATE
  { BOr }
| BXOR_UPDATE
  { BXor }

// expressions
expr:
| start_loc = LPAREN e = expr end_loc = RPAREN
  { let (e, _) = e in
    (e, span start_loc end_loc) }
| n = NUMLIT
  { let (n, loc) = n in 
    (NumLiteral (n, Some loc), loc) }
| c = CHARLIT
  { 
    let (c, loc) = c in 
    (CharLiteral (c, Some loc), loc)
  }
| var = IDENT 
  { let (var, loc) = var in 
    (Var (var, Some loc), loc) }
| fn = IDENT LPAREN args = arg_list end_loc = RPAREN
  { let (fn, start_loc) = fn in 
    let loc = span start_loc end_loc in 
    (Call (fn, args, Some loc), loc) }
| start_loc = STAR e = expr
  {
    let (e, end_loc) = e in 
    let loc = span start_loc end_loc in 
    (Deref (e, Some loc), loc)
  }
| start_loc = AMPERSAND e = expr
  {
    let (e, end_loc) = e in 
    let loc = span start_loc end_loc in 
    (AddrOf (e, Some loc), loc)
  }
| start_loc = LPAREN t = typ RPAREN e = expr
  {
    let (t, _) = t in 
    let (e, end_loc) = e in 
    let loc = span start_loc end_loc in 
    (Cast (t, e, Some loc), loc)
  }
| dest = expr ASSIGN e = expr
  { 
    let (dest, start_loc) = dest in 
    let (e, end_loc) = e in 
    let loc = span start_loc end_loc in 
    (Assign (dest, e, Some loc), loc)
  }
| e = expr end_loc = INR
  { 
    let (e, start_loc) = e in 
    let loc = span start_loc end_loc in 
    (PostfixInr (e, Some loc), loc) 
  }
| e = expr end_loc = DCR
  { 
    let (e, start_loc) = e in 
    let loc = span start_loc end_loc in 
    (PostfixDcr (e, Some loc), loc) 
  }
| start_loc = BNOT e = expr 
  { let (e, end_loc) = e in 
    let loc = span start_loc end_loc in 
    (UnOp (BNot, e, Some loc), loc) }
| start_loc = LNOT e = expr 
  { let (e, end_loc) = e in 
    let loc = span start_loc end_loc in 
    (UnOp (LNot, e, Some loc), loc) }
| l = expr PLUS r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Plus, l, r, Some loc), loc) }
| l = expr MINUS r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Minus, l, r, Some loc), loc) }
| l = expr STAR r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Mult, l, r, Some loc), loc) }
| l = expr DIV r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Div, l, r, Some loc), loc) }
| l = expr MOD r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Mod, l, r, Some loc), loc) }
| l = expr AMPERSAND r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (BAnd, l, r, Some loc), loc) }
| l = expr BOR r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (BOr, l, r, Some loc), loc) }
| l = expr BXOR r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (BXor, l, r, Some loc), loc) }
| l = expr LAND r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (LAnd, l, r, Some loc), loc) }
| l = expr LOR r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (LOr, l, r, Some loc), loc) }
| l = expr GT r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Gt, l, r, Some loc), loc) }
| l = expr LT r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Lt, l, r, Some loc), loc) }
| l = expr GTE r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Gte, l, r, Some loc), loc) }
| l = expr LTE r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Lte, l, r, Some loc), loc) }
| l = expr EQ r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Eq, l, r, Some loc), loc) }
| l = expr NEQ r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Neq, l, r, Some loc), loc) }
| start_loc = INR e = expr 
  { 
    let (e, end_loc) = e in
    let loc = span start_loc end_loc in
    (SPrefixInr (e, Some loc), loc)
  }
| start_loc = DCR e = expr 
  { 
    let (e, end_loc) = e in
    let loc = span start_loc end_loc in
    (SPrefixDcr (e, Some loc), loc)
  }
| dest = expr op = update e = expr 
  {
    let (dest, start_loc) = dest in 
    let (e, end_loc) = e in 
    let loc = span start_loc end_loc in 
    (SUpdate (dest, e, op, Some loc), loc)
  }
| arr = expr LBRACKET sub = expr end_loc = RBRACKET
  {
    let (arr, start_loc) = arr in 
    let (sub, _) = sub in 
    let loc = span start_loc end_loc in 
    (SSubscript (arr, sub, Some loc), loc)
  }

// argument list of expressions for function calls
arg_list:
| args = separated_list(COMMA, expr)
{ 
  let args = List.map (fun (arg, _) -> arg) args in 
  args
}
