open OUnit2
open Compiler.Parser
open Compiler.Ast

(* [norm_src_locs] replaces all source locations in a program with None *)
let norm_src_locs (pgrm : prog) =
  let rec norm_expr (exp : expr) =
    match exp with
    | Num (n, _) -> Num (n, None)
    | Var (id, _) -> Var (id, None)
    | UnOp (op, e, _) -> UnOp (op, norm_expr e, None)
    | BinOp (op, l, r, _) -> BinOp (op, norm_expr l, norm_expr r, None)
    | LogOp (LNot e, _) -> LogOp (LNot (norm_expr e), None)
    | LogOp (LAnd (l, r), _) -> LogOp (LAnd (norm_expr l, norm_expr r), None)
    | LogOp (LOr (l, r), _) -> LogOp (LOr (norm_expr l, norm_expr r), None)
    | Call (fn, args, _) -> Call (fn, List.map norm_expr args, None)
  and norm_stmt (stmt : stmt) =
    match stmt with
    | Let (id, typ, value, body, _) ->
        Let (id, typ, norm_expr value, norm_stmt_list body, None)
    | Assign (id, exp, _) -> Assign (id, norm_expr exp, None)
    | If (cond, thn, _) -> If (norm_expr cond, norm_stmt_list thn, None)
    | IfElse (cond, thn, els, _) ->
        IfElse (norm_expr cond, norm_stmt_list thn, norm_stmt_list els, None)
    | Block (stmts, _) -> Block (norm_stmt_list stmts, None)
    | Return (Some e, _) -> Return (Some (norm_expr e), None)
    | Return (None, _) -> Return (None, None)
    | ExprStmt (e, _) -> ExprStmt (norm_expr e, None)
    | While (cond, body, _) -> While (norm_expr cond, norm_stmt_list body, None)
    | PrintDec (e, _) -> PrintDec (norm_expr e, None)
    | Inr (name, _) -> Inr (name, None)
    | Dcr (name, _) -> Dcr (name, None)
    | Exit (Some e, _) -> Exit (Some (norm_expr e), None)
    | Exit (None, _) -> Exit (None, None)
    | Assert (e, _) -> Assert (e, None)
  and norm_stmt_list (stmts : stmt list) = List.map norm_stmt stmts
  and norm_func (func : func_defn) =
    { func with body = norm_stmt_list func.body; loc = None }
  in
  let { funcs; main } = pgrm in
  { funcs = List.map norm_func funcs; main = norm_func main }

(* [main_from_body] constructs an ast function defn that conforms
  to what main functions must look like, with the given body filled in. *)
let main_from_body (body : stmt list) : func_defn =
  {
    name = "main";
    params = [];
    body;
    return_ty = Void;
    ctrl_reaches_end = false;
    loc = None;
  }

let empty_main = main_from_body []

(* [assert_parses_to] tests that a given full program parses to 
  the given program ast. *)
let assert_parses_to (pgrm : string) (exp : prog) =
  let parsed = norm_src_locs (parse pgrm) in
  assert_equal parsed exp

(* [assert_body_parses_to] asserts that the given text for the body 
  of a main function parses to a program with the given stmt list 
  as the body of its main function. This removes some of the overhead
  of constructing full programs when testing individual constructs. *)
let assert_body_parses_to (body : string) (exp_body : stmt list) =
  let pgrm = Printf.sprintf "void main() { %s }" body in
  let expected = { funcs = []; main = main_from_body exp_body } in
  assert_parses_to pgrm expected

let test_empty_main _ =
  assert_parses_to "void main() {}" { funcs = []; main = empty_main }

let test_num _ =
  assert_body_parses_to "1; -117; 0xc; -0b101;"
    [
      ExprStmt (Num (1, None), None);
      ExprStmt (Num (-117, None), None);
      ExprStmt (Num (0xc, None), None);
      ExprStmt (Num (-0b101, None), None);
    ]

let test_var _ =
  assert_body_parses_to "x; name_with_underscores; nameWithNum50;"
    [
      ExprStmt (Var ("x", None), None);
      ExprStmt (Var ("name_with_underscores", None), None);
      ExprStmt (Var ("nameWithNum50", None), None);
    ]

let test_un_op _ =
  assert_body_parses_to "~7;"
    [ ExprStmt (UnOp (BNot, Num (7, None), None), None) ]

let test_bin_op _ =
  assert_body_parses_to "4 + 5;"
    [ ExprStmt (BinOp (Plus, Num (4, None), Num (5, None), None), None) ];
  assert_body_parses_to "16 - 7;"
    [ ExprStmt (BinOp (Minus, Num (16, None), Num (7, None), None), None) ];
  assert_body_parses_to "100 * 2;"
    [ ExprStmt (BinOp (Mult, Num (100, None), Num (2, None), None), None) ];
  assert_body_parses_to "0xa / 5;"
    [ ExprStmt (BinOp (Div, Num (0xa, None), Num (5, None), None), None) ];
  assert_body_parses_to "120 % 10;"
    [ ExprStmt (BinOp (Mod, Num (120, None), Num (10, None), None), None) ];
  assert_body_parses_to "0b110 & 0b010;"
    [
      ExprStmt (BinOp (BAnd, Num (0b110, None), Num (0b010, None), None), None);
    ];
  assert_body_parses_to "0b1111 | 0b1010;"
    [
      ExprStmt (BinOp (BOr, Num (0b1111, None), Num (0b1010, None), None), None);
    ];
  assert_body_parses_to "0b01 ^ 0b01;"
    [ ExprStmt (BinOp (BXor, Num (0b01, None), Num (0b01, None), None), None) ];
  assert_body_parses_to "-6 > -10;"
    [ ExprStmt (BinOp (Gt, Num (-6, None), Num (-10, None), None), None) ];
  assert_body_parses_to "100 < 110;"
    [ ExprStmt (BinOp (Lt, Num (100, None), Num (110, None), None), None) ];
  assert_body_parses_to "81 >= 77;"
    [ ExprStmt (BinOp (Gte, Num (81, None), Num (77, None), None), None) ];
  assert_body_parses_to "-41 <= 4;"
    [ ExprStmt (BinOp (Lte, Num (-41, None), Num (4, None), None), None) ];
  assert_body_parses_to "16 == 48;"
    [ ExprStmt (BinOp (Eq, Num (16, None), Num (48, None), None), None) ];
  assert_body_parses_to "44 != -44;"
    [ ExprStmt (BinOp (Neq, Num (44, None), Num (-44, None), None), None) ]

let test_log_op _ =
  assert_body_parses_to "1 && 0;"
    [ ExprStmt (LogOp (LAnd (Num (1, None), Num (0, None)), None), None) ];
  assert_body_parses_to "5 || 7;"
    [ ExprStmt (LogOp (LOr (Num (5, None), Num (7, None)), None), None) ];
  assert_body_parses_to "!17;"
    [ ExprStmt (LogOp (LNot (Num (17, None)), None), None) ]

let test_precedence _ =
  assert_body_parses_to "1 + 2 * 8 < (100 ^ 3) && ~7 == (40 & 18);"
    [
      ExprStmt
        ( LogOp
            ( LAnd
                ( BinOp
                    ( Lt,
                      BinOp
                        ( Plus,
                          Num (1, None),
                          BinOp (Mult, Num (2, None), Num (8, None), None),
                          None ),
                      BinOp (BXor, Num (100, None), Num (3, None), None),
                      None ),
                  BinOp
                    ( Eq,
                      UnOp (BNot, Num (7, None), None),
                      BinOp (BAnd, Num (40, None), Num (18, None), None),
                      None ) ),
              None ),
          None );
    ];
  (* logical operators *)
  assert_body_parses_to "!4 && 7 || !(6 && 23);"
    [
      ExprStmt
        ( LogOp
            ( LOr
                ( LogOp
                    ( LAnd (LogOp (LNot (Num (4, None)), None), Num (7, None)),
                      None ),
                  LogOp
                    ( LNot (LogOp (LAnd (Num (6, None), Num (23, None)), None)),
                      None ) ),
              None ),
          None );
    ]

let test_assoc _ =
  assert_body_parses_to "1 + 2 + 3 + 4;"
    [
      ExprStmt
        ( BinOp
            ( Plus,
              BinOp
                ( Plus,
                  BinOp (Plus, Num (1, None), Num (2, None), None),
                  Num (3, None),
                  None ),
              Num (4, None),
              None ),
          None );
    ]

let test_arbitrary_parens _ =
  assert_body_parses_to "(((40)));" [ ExprStmt (Num (40, None), None) ]

let test_let _ =
  (* simple *)
  let body = "int x = 70;" in
  let body_stmts = [ Let ("x", Int, Num (70, None), [], None) ] in
  assert_body_parses_to body body_stmts;
  (* nested scope *)
  let body = "int z = 1; int y = 2; z;" in
  let body_stmts =
    [
      Let
        ( "z",
          Int,
          Num (1, None),
          [
            Let
              ( "y",
                Int,
                Num (2, None),
                [ ExprStmt (Var ("z", None), None) ],
                None );
          ],
          None );
    ]
  in
  assert_body_parses_to body body_stmts;
  let body = "int first = 12; 1; 2; int second = first; 3;" in
  let body_stmts =
    [
      Let
        ( "first",
          Int,
          Num (12, None),
          [
            ExprStmt (Num (1, None), None);
            ExprStmt (Num (2, None), None);
            Let
              ( "second",
                Int,
                Var ("first", None),
                [ ExprStmt (Num (3, None), None) ],
                None );
          ],
          None );
    ]
  in
  assert_body_parses_to body body_stmts;
  (* scope ends at end of block *)
  let body = "int x = 10; { int y = x; } 1;" in
  let body_stmts =
    [
      Let
        ( "x",
          Int,
          Num (10, None),
          [
            Block ([ Let ("y", Int, Var ("x", None), [], None) ], None);
            ExprStmt (Num (1, None), None);
          ],
          None );
    ]
  in
  assert_body_parses_to body body_stmts

let test_assign _ =
  assert_body_parses_to "int x = 0; x = 7;"
    [
      Let ("x", Int, Num (0, None), [ Assign ("x", Num (7, None), None) ], None);
    ]

let test_if _ =
  assert_body_parses_to "if (1) { 10; }"
    [ If (Num (1, None), [ ExprStmt (Num (10, None), None) ], None) ]

let test_if_else _ =
  assert_body_parses_to "if (-5) { 1; } else { 0; }"
    [
      IfElse
        ( Num (-5, None),
          [ ExprStmt (Num (1, None), None) ],
          [ ExprStmt (Num (0, None), None) ],
          None );
    ]

let test_block _ =
  assert_body_parses_to "{ 1; 2; { 3; } }"
    [
      Block
        ( [
            ExprStmt (Num (1, None), None);
            ExprStmt (Num (2, None), None);
            Block ([ ExprStmt (Num (3, None), None) ], None);
          ],
          None );
    ]

let test_return _ =
  assert_body_parses_to "return; return 0;"
    [ Return (None, None); Return (Some (Num (0, None)), None) ]

let test_exprstmt _ =
  assert_body_parses_to "1 + 2; x;"
    [
      ExprStmt (BinOp (Plus, Num (1, None), Num (2, None), None), None);
      ExprStmt (Var ("x", None), None);
    ]

let test_while _ =
  assert_body_parses_to "while (1) { 5; }"
    [ While (Num (1, None), [ ExprStmt (Num (5, None), None) ], None) ]

let test_print_dec _ =
  assert_body_parses_to "print(7);" [ PrintDec (Num (7, None), None) ]

let test_inr _ =
  assert_body_parses_to "x++; name++;" [ Inr ("x", None); Inr ("name", None) ]

let test_dcr _ =
  assert_body_parses_to "x--; name--;" [ Dcr ("x", None); Dcr ("name", None) ]

let test_exit _ =
  assert_body_parses_to "exit(); exit(-1);"
    [ Exit (None, None); Exit (Some (Num (-1, None)), None) ]

let test_fact _ =
  let fact =
    "\n\
    \        // fact(n) computes n!\n\
    \        int fact(int n) {\n\
    \          if (n == 0) {\n\
    \            return 1;\n\
    \          } else {\n\
    \            return n * fact(n - 1);\n\
    \          }\n\
    \        }\n\
    \        void main() {}"
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
                  ( BinOp (Eq, Var ("n", None), Num (0, None), None),
                    [ Return (Some (Num (1, None)), None) ],
                    [
                      Return
                        ( Some
                            (BinOp
                               ( Mult,
                                 Var ("n", None),
                                 Call
                                   ( "fact",
                                     [
                                       BinOp
                                         ( Minus,
                                           Var ("n", None),
                                           Num (1, None),
                                           None );
                                     ],
                                     None ),
                                 None )),
                          None );
                    ],
                    None );
              ];
            return_ty = Int;
            ctrl_reaches_end = false;
            loc = None;
          };
        ];
      main = empty_main;
    }

let suite =
  "Source Language Parser Tests"
  >::: [
         "test_empty_main" >:: test_empty_main;
         "test_num" >:: test_num;
         "test_var" >:: test_var;
         "test_un_op" >:: test_un_op;
         "test_bin_op" >:: test_bin_op;
         "test_log_op" >:: test_log_op;
         "test_precedence" >:: test_precedence;
         "test_assoc" >:: test_assoc;
         "test_arbitrary_parens" >:: test_arbitrary_parens;
         "test_let" >:: test_let;
         "test_assign" >:: test_assign;
         "test_if" >:: test_if;
         "test_if_else" >:: test_if_else;
         "test_block" >:: test_block;
         "test_return" >:: test_return;
         "test_exprstmt" >:: test_exprstmt;
         "test_while" >:: test_while;
         "test_print_dec" >:: test_print_dec;
         "test_inr" >:: test_inr;
         "test_dcr" >:: test_dcr;
         "test_exit" >:: test_exit;
         "test_fact" >:: test_fact;
       ]

let () = run_test_tt_main suite
