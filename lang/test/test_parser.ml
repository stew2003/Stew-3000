open OUnit2
open Compiler.Parser
open Compiler.Ast
open Util.Srcloc

let sl = loc 0 0

let norm_src_locs (pgrm : prog) =
  let rec norm_expr (exp : expr) =
    match exp with
    | Num (n, _) -> Num (n, sl)
    | Var (id, _) -> Var (id, sl)
    | UnOp (op, e, _) -> UnOp (op, norm_expr e, sl)
    | BinOp (op, l, r, _) -> BinOp (op, norm_expr l, norm_expr r, sl)
    | LogOp (LNot e, _) -> LogOp (LNot (norm_expr e), sl)
    | LogOp (LAnd (l, r), _) -> LogOp (LAnd (norm_expr l, norm_expr r), sl)
    | LogOp (LOr (l, r), _) -> LogOp (LOr (norm_expr l, norm_expr r), sl)
    | Call (fn, args, _) -> Call (fn, List.map norm_expr args, sl)
  and norm_stmt (stmt : stmt) =
    match stmt with
    | Let (id, typ, value, body, _) ->
        Let (id, typ, norm_expr value, norm_stmt_list body, sl)
    | Assign (id, exp, _) -> Assign (id, norm_expr exp, sl)
    | If (cond, thn, _) -> If (norm_expr cond, norm_stmt_list thn, sl)
    | IfElse (cond, thn, els, _) ->
        IfElse (norm_expr cond, norm_stmt_list thn, norm_stmt_list els, sl)
    | Block (stmts, _) -> Block (norm_stmt_list stmts, sl)
    | Return (Some e, _) -> Return (Some (norm_expr e), sl)
    | Return (None, _) -> Return (None, sl)
    | ExprStmt (e, _) -> ExprStmt (norm_expr e, sl)
    | While (cond, body, _) -> While (norm_expr cond, norm_stmt_list body, sl)
    | PrintDec (e, _) -> PrintDec (norm_expr e, sl)
    | Inr (name, _) -> Inr (name, sl)
    | Dcr (name, _) -> Dcr (name, sl)
    | Exit (Some e, _) -> Exit (Some (norm_expr e), sl)
    | Exit (None, _) -> Exit (None, sl)
  and norm_stmt_list (stmts : stmt list) = List.map norm_stmt stmts
  and norm_func (func : func_defn) =
    { func with body = norm_stmt_list func.body; loc = sl }
  in
  let { funcs; main } = pgrm in
  { funcs = List.map norm_func funcs; main = norm_func main }

let main_from_body (body : stmt list) : func_defn =
  { name = "main"; params = []; body; return_ty = Void; loc = sl }

let empty_main = main_from_body []

let assert_parses_to (pgrm : string) (exp : prog) =
  let parsed = norm_src_locs (parse pgrm) in
  assert_equal parsed exp

let test_empty_main _ =
  assert_parses_to "void main() {}" { funcs = []; main = empty_main }

let test_fact _ =
  let fact =
    "\n\
    \    // fact(n) computes n!\n\
    \    int fact(int n) {\n\
    \      if (n == 0) {\n\
    \        return 1;\n\
    \      } else {\n\
    \        return n * fact(n - 1);\n\
    \      }\n\
    \    }\n\n\
    \    void main() {}\n\
    \  "
  in
  assert_parses_to fact
    {
      funcs =
        [
          {
            name = "fact";
            params = [ ("n", Int) ];
            body =
              [
                IfElse
                  ( BinOp (Eq, Var ("n", sl), Num (0, sl), sl),
                    [ Return (Some (Num (1, sl)), sl) ],
                    [
                      Return
                        ( Some
                            (BinOp
                               ( Mult,
                                 Var ("n", sl),
                                 Call
                                   ( "fact",
                                     [
                                       BinOp
                                         (Minus, Var ("n", sl), Num (1, sl), sl);
                                     ],
                                     sl ),
                                 sl )),
                          sl );
                    ],
                    sl );
              ];
            return_ty = Int;
            loc = sl;
          };
        ];
      main = empty_main;
    }

let suite =
  "Source Language Parser Tests"
  >::: [ "test_empty_main" >:: test_empty_main; "test_fact" >:: test_fact ]

let () = run_test_tt_main suite
