%{
  open Ast
  open Util.Srcloc
%}

%token <int * Util.Srcloc.src_loc> NUM
%token <string * Util.Srcloc.src_loc> IDENT

%token <Util.Srcloc.src_loc> LPAREN
%token <Util.Srcloc.src_loc> RPAREN
%token <Util.Srcloc.src_loc> LBRACE
%token <Util.Srcloc.src_loc> RBRACE
%token <Util.Srcloc.src_loc> SEMICOLON
%token <Util.Srcloc.src_loc> COMMA

%token <Util.Srcloc.src_loc> PLUS
%token <Util.Srcloc.src_loc> MINUS
%token <Util.Srcloc.src_loc> TIMES
%token <Util.Srcloc.src_loc> DIV
%token <Util.Srcloc.src_loc> MOD
%token <Util.Srcloc.src_loc> BAND
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

%token <Util.Srcloc.src_loc> IF
%token <Util.Srcloc.src_loc> ELSE
%token <Util.Srcloc.src_loc> WHILE
%token <Util.Srcloc.src_loc> RETURN

%token <Util.Srcloc.src_loc> PRINT
%token <Util.Srcloc.src_loc> EXIT

%token <Util.Srcloc.src_loc> INT
%token <Util.Srcloc.src_loc> VOID

%token EOF

%start <func_defn list> program

%left LOR
%left LAND
%left BOR
%left BXOR
%left BAND
%left EQ NEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIV MOD
%right BNOT LNOT

%%

// a program is a list of function definitions
program:
| defns = list(definition) EOF
  { defns }

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
      loc= span start_loc end_loc;
    }
  }

params:
| p = separated_list(COMMA, decl)
  { p }

typ:
| loc = INT
  { (Int, loc) }
| loc = VOID
  { (Void, loc) }

// variable declaration, type then name
decl:
| t = typ name = IDENT
  { 
    let (t, start_loc) = t in 
    let (name, end_loc) = name in
    (name, t, span start_loc end_loc)
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
    Let (name, typ, e, scope, span start_loc end_loc)
  }
| start_loc = IF LPAREN cond = expr RPAREN 
  LBRACE thn = stmt_list end_loc = RBRACE
  { let (cond, _) = cond in 
    If (cond, thn, span start_loc end_loc) }
| start_loc = IF LPAREN cond = expr RPAREN LBRACE thn = stmt_list RBRACE 
  ELSE LBRACE els = stmt_list end_loc = RBRACE
  { let (cond, _) = cond in 
    IfElse (cond, thn, els, span start_loc end_loc) }
| start_loc = LBRACE stmts = stmt_list end_loc = RBRACE
  { Block (stmts, span start_loc end_loc) }
| start_loc = WHILE LPAREN cond = expr RPAREN LBRACE body = stmt_list end_loc = RBRACE
  { let (cond, _) = cond in 
    While (cond, body, span start_loc end_loc) }

// statements that require semicolon termination
stmt:
| var = IDENT ASSIGN e = expr
  { 
    let (var, start_loc) = var in 
    let (e, end_loc) = e in 
    Assign (var, e, span start_loc end_loc)
  }
| start_loc = RETURN e = expr 
  {
    let (e, end_loc) = e in 
    Return (Some e, span start_loc end_loc)
  }
| loc = RETURN 
  { Return (None, loc) }
| e = expr 
  { let (e, loc) = e in 
    ExprStmt (e, loc) }
| start_loc = PRINT LPAREN arg = expr end_loc = RPAREN
  { let (arg, _) = arg in 
    PrintDec (arg, span start_loc end_loc) }
| var = IDENT end_loc = INR
  { let (var, start_loc) = var in 
    Inr (var, span start_loc end_loc) }
| var = IDENT end_loc = DCR
  { let (var, start_loc) = var in 
    Dcr (var, span start_loc end_loc) }
| start_loc = EXIT LPAREN arg = expr end_loc = RPAREN
  { let (arg, _) = arg in 
    Exit (Some arg, span start_loc end_loc) }
| start_loc = EXIT LPAREN end_loc = RPAREN
  { Exit (None, span start_loc end_loc) }

// expressions
expr:
| start_loc = LPAREN e = expr end_loc = RPAREN
  { let (e, _) = e in
    (e, span start_loc end_loc) }
| n = NUM
  { let (n, loc) = n in 
    (Num (n, loc), loc) }
| var = IDENT 
  { let (var, loc) = var in 
    (Var (var, loc), loc) }
| fn = IDENT LPAREN args = arg_list end_loc = RPAREN
  { let (fn, start_loc) = fn in 
    let loc = span start_loc end_loc in 
    (Call (fn, args, loc), loc) }
| start_loc = BNOT e = expr 
  { let (e, end_loc) = e in 
    let loc = span start_loc end_loc in 
    (UnOp (BNot, e, loc), loc) }
| l = expr PLUS r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Plus, l, r, loc), loc) }
| l = expr MINUS r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Minus, l, r, loc), loc) }
| l = expr TIMES r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Mult, l, r, loc), loc) }
| l = expr DIV r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Div, l, r, loc), loc) }
| l = expr MOD r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Mod, l, r, loc), loc) }
| l = expr BAND r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (BAnd, l, r, loc), loc) }
| l = expr BOR r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (BOr, l, r, loc), loc) }
| l = expr BXOR r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (BXor, l, r, loc), loc) }
| l = expr GT r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Gt, l, r, loc), loc) }
| l = expr LT r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Lt, l, r, loc), loc) }
| l = expr GTE r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Gte, l, r, loc), loc) }
| l = expr LTE r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Lte, l, r, loc), loc) }
| l = expr EQ r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Eq, l, r, loc), loc) }
| l = expr NEQ r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    (BinOp (Neq, l, r, loc), loc) }
| l = expr LAND r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    ((LogOp ((LAnd (l, r)), loc)), loc) }
| l = expr LOR r = expr 
  { let (l, start_loc) = l in 
    let (r, end_loc) = r in 
    let loc = span start_loc end_loc in 
    ((LogOp ((LOr (l, r)), loc)), loc) }
| start_loc = LNOT e = expr 
  { let (e, end_loc) = e in 
    let loc = span start_loc end_loc in 
    ((LogOp ((LNot e), loc)), loc) }

// argument list of expressions for function calls
arg_list:
| args = separated_list(COMMA, expr)
{ 
  let args = List.map (fun (arg, _) -> arg) args in 
  args
}
