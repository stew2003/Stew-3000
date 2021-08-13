open OUnit2
open Compiler.Parser
open Compiler.Ast
open Testing_utils

(* [main_from_body] constructs an ast function defn that conforms
   to what main functions must look like, with the given body filled in. *)
let main_from_body (body : stmt list) : func_defn =
  {
    name = "main";
    params = [];
    body;
    return_ty = Void;
    ctrl_reaches_end = None;
    loc = None;
  }

let empty_main = main_from_body []

(* [assert_parses_to] tests that a given full program parses to
   the given program ast. *)
let assert_parses_to (pgrm : string) (exp : prog) =
  let parsed = norm_prog_locs (parse pgrm) in
  assert_equal parsed exp

(* [assert_body_parses_to] asserts that the given text for the body
   of a main function parses to a program with the given stmt list
   as the body of its main function. This removes some of the overhead
   of constructing full programs when testing individual constructs. *)
let assert_body_parses_to (body : string) (exp_body : stmt list) =
  let pgrm = Printf.sprintf "void main() { %s }" body in
  let expected = { defines = []; funcs = []; main = main_from_body exp_body } in
  assert_parses_to pgrm expected

let test_empty_main _ = assert_body_parses_to "" []

let test_num _ =
  assert_body_parses_to "1; -117; 0xc; -0b101;"
    [
      ExprStmt (NumLiteral (1, None), None);
      ExprStmt (NumLiteral (-117, None), None);
      ExprStmt (NumLiteral (0xc, None), None);
      ExprStmt (NumLiteral (-0b101, None), None);
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
    [ ExprStmt (UnOp (BNot, NumLiteral (7, None), None), None) ]

let test_bin_op _ =
  assert_body_parses_to "4 + 5;"
    [
      ExprStmt
        (BinOp (Plus, NumLiteral (4, None), NumLiteral (5, None), None), None);
    ];
  assert_body_parses_to "16 - 7;"
    [
      ExprStmt
        (BinOp (Minus, NumLiteral (16, None), NumLiteral (7, None), None), None);
    ];
  assert_body_parses_to "100 * 2;"
    [
      ExprStmt
        (BinOp (Mult, NumLiteral (100, None), NumLiteral (2, None), None), None);
    ];
  assert_body_parses_to "0xa / 5;"
    [
      ExprStmt
        (BinOp (Div, NumLiteral (0xa, None), NumLiteral (5, None), None), None);
    ];
  assert_body_parses_to "120 % 10;"
    [
      ExprStmt
        (BinOp (Mod, NumLiteral (120, None), NumLiteral (10, None), None), None);
    ];
  assert_body_parses_to "0b110 & 0b010;"
    [
      ExprStmt
        ( BinOp (BAnd, NumLiteral (0b110, None), NumLiteral (0b010, None), None),
          None );
    ];
  assert_body_parses_to "0b1111 | 0b1010;"
    [
      ExprStmt
        ( BinOp (BOr, NumLiteral (0b1111, None), NumLiteral (0b1010, None), None),
          None );
    ];
  assert_body_parses_to "0b01 ^ 0b01;"
    [
      ExprStmt
        ( BinOp (BXor, NumLiteral (0b01, None), NumLiteral (0b01, None), None),
          None );
    ];
  assert_body_parses_to "-6 > -10;"
    [
      ExprStmt
        (BinOp (Gt, NumLiteral (-6, None), NumLiteral (-10, None), None), None);
    ];
  assert_body_parses_to "100 < 110;"
    [
      ExprStmt
        (BinOp (Lt, NumLiteral (100, None), NumLiteral (110, None), None), None);
    ];
  assert_body_parses_to "81 >= 77;"
    [
      ExprStmt
        (BinOp (Gte, NumLiteral (81, None), NumLiteral (77, None), None), None);
    ];
  assert_body_parses_to "-41 <= 4;"
    [
      ExprStmt
        (BinOp (Lte, NumLiteral (-41, None), NumLiteral (4, None), None), None);
    ];
  assert_body_parses_to "16 == 48;"
    [
      ExprStmt
        (BinOp (Eq, NumLiteral (16, None), NumLiteral (48, None), None), None);
    ];
  assert_body_parses_to "44 != -44;"
    [
      ExprStmt
        (BinOp (Neq, NumLiteral (44, None), NumLiteral (-44, None), None), None);
    ]

let test_log_op _ =
  assert_body_parses_to "1 && 0;"
    [
      ExprStmt
        (BinOp (LAnd, NumLiteral (1, None), NumLiteral (0, None), None), None);
    ];
  assert_body_parses_to "5 || 7;"
    [
      ExprStmt
        (BinOp (LOr, NumLiteral (5, None), NumLiteral (7, None), None), None);
    ];
  assert_body_parses_to "!17;"
    [ ExprStmt (UnOp (LNot, NumLiteral (17, None), None), None) ]

let test_precedence _ =
  assert_body_parses_to "1 + 2 * 8 < (100 ^ 3) && ~7 == (40 & 18);"
    [
      ExprStmt
        ( BinOp
            ( LAnd,
              BinOp
                ( Lt,
                  BinOp
                    ( Plus,
                      NumLiteral (1, None),
                      BinOp
                        (Mult, NumLiteral (2, None), NumLiteral (8, None), None),
                      None ),
                  BinOp
                    (BXor, NumLiteral (100, None), NumLiteral (3, None), None),
                  None ),
              BinOp
                ( Eq,
                  UnOp (BNot, NumLiteral (7, None), None),
                  BinOp
                    (BAnd, NumLiteral (40, None), NumLiteral (18, None), None),
                  None ),
              None ),
          None );
    ];
  (* logical operators *)
  assert_body_parses_to "!4 && 7 || !(6 && 23);"
    [
      ExprStmt
        ( BinOp
            ( LOr,
              BinOp
                ( LAnd,
                  UnOp (LNot, NumLiteral (4, None), None),
                  NumLiteral (7, None),
                  None ),
              UnOp
                ( LNot,
                  BinOp (LAnd, NumLiteral (6, None), NumLiteral (23, None), None),
                  None ),
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
                  BinOp (Plus, NumLiteral (1, None), NumLiteral (2, None), None),
                  NumLiteral (3, None),
                  None ),
              NumLiteral (4, None),
              None ),
          None );
    ]

let test_arbitrary_parens _ =
  assert_body_parses_to "(((40)));" [ ExprStmt (NumLiteral (40, None), None) ]

let test_let _ =
  (* simple *)
  let body = "int x = 70;" in
  let body_stmts =
    [ Declare ("x", Int, Some (NumLiteral (70, None)), [], None) ]
  in
  assert_body_parses_to body body_stmts;
  (* nested scope *)
  let body = "int z = 1; int y = 2; z;" in
  let body_stmts =
    [
      Declare
        ( "z",
          Int,
          Some (NumLiteral (1, None)),
          [
            Declare
              ( "y",
                Int,
                Some (NumLiteral (2, None)),
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
      Declare
        ( "first",
          Int,
          Some (NumLiteral (12, None)),
          [
            ExprStmt (NumLiteral (1, None), None);
            ExprStmt (NumLiteral (2, None), None);
            Declare
              ( "second",
                Int,
                Some (Var ("first", None)),
                [ ExprStmt (NumLiteral (3, None), None) ],
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
      Declare
        ( "x",
          Int,
          Some (NumLiteral (10, None)),
          [
            Block
              ([ Declare ("y", Int, Some (Var ("x", None)), [], None) ], None);
            ExprStmt (NumLiteral (1, None), None);
          ],
          None );
    ]
  in
  assert_body_parses_to body body_stmts

let test_array_decl _ =
  let literals_from_chars (chars : char list) : expr list =
    chars |> List.map (fun c -> CharLiteral (c, None))
  in

  assert_body_parses_to "int array[10];"
    [
      ArrayDeclare ("array", Int, Some (NumLiteral (10, None)), None, [], None);
    ];
  assert_body_parses_to "char s[] = \"neat\";"
    [
      ArrayDeclare
        ( "s",
          Char,
          None,
          Some (literals_from_chars [ 'n'; 'e'; 'a'; 't'; '\x00' ]),
          [],
          None );
    ];
  assert_body_parses_to "unsigned *x[3] = {4, 6, 8};"
    [
      ArrayDeclare
        ( "x",
          Pointer Unsigned,
          Some (NumLiteral (3, None)),
          Some
            [ NumLiteral (4, None); NumLiteral (6, None); NumLiteral (8, None) ],
          [],
          None );
    ];
  assert_body_parses_to "char str[100] = \"string value\";"
    [
      ArrayDeclare
        ( "str",
          Char,
          Some (NumLiteral (100, None)),
          Some
            (literals_from_chars
               [
                 's';
                 't';
                 'r';
                 'i';
                 'n';
                 'g';
                 ' ';
                 'v';
                 'a';
                 'l';
                 'u';
                 'e';
                 '\x00';
               ]),
          [],
          None );
    ];
  assert_body_parses_to "char ***x[5]; 1; 2;"
    [
      ArrayDeclare
        ( "x",
          Pointer (Pointer (Pointer Char)),
          Some (NumLiteral (5, None)),
          None,
          [
            ExprStmt (NumLiteral (1, None), None);
            ExprStmt (NumLiteral (2, None), None);
          ],
          None );
    ]

let test_assign _ =
  assert_body_parses_to "int x = 0; x = 7;"
    [
      Declare
        ( "x",
          Int,
          Some (NumLiteral (0, None)),
          [
            ExprStmt (Assign (Var ("x", None), NumLiteral (7, None), None), None);
          ],
          None );
    ]

let test_if _ =
  assert_body_parses_to "if (1) { 10; }"
    [
      If (NumLiteral (1, None), [ ExprStmt (NumLiteral (10, None), None) ], None);
    ]

let test_if_else _ =
  assert_body_parses_to "if (-5) { 1; } else { 0; }"
    [
      IfElse
        ( NumLiteral (-5, None),
          [ ExprStmt (NumLiteral (1, None), None) ],
          [ ExprStmt (NumLiteral (0, None), None) ],
          None );
    ]

let test_block _ =
  assert_body_parses_to "{ 1; 2; { 3; } }"
    [
      Block
        ( [
            ExprStmt (NumLiteral (1, None), None);
            ExprStmt (NumLiteral (2, None), None);
            Block ([ ExprStmt (NumLiteral (3, None), None) ], None);
          ],
          None );
    ]

let test_return _ =
  assert_body_parses_to "return; return 0;"
    [ Return (None, None); Return (Some (NumLiteral (0, None)), None) ]

let test_exprstmt _ =
  assert_body_parses_to "1 + 2; x;"
    [
      ExprStmt
        (BinOp (Plus, NumLiteral (1, None), NumLiteral (2, None), None), None);
      ExprStmt (Var ("x", None), None);
    ]

let test_while _ =
  assert_body_parses_to "while (1) { 5; }"
    [
      While
        (NumLiteral (1, None), [ ExprStmt (NumLiteral (5, None), None) ], None);
    ]

let test_print_dec _ =
  assert_body_parses_to "print(7);" [ PrintDec (NumLiteral (7, None), None) ]

let test_inr _ =
  assert_body_parses_to "x++; name++;"
    [
      ExprStmt (SInr (Var ("x", None), None), None);
      ExprStmt (SInr (Var ("name", None), None), None);
    ];
  assert_body_parses_to "x = y++;"
    [
      ExprStmt
        (Assign (Var ("x", None), SInr (Var ("y", None), None), None), None);
    ]

let test_dcr _ =
  assert_body_parses_to "x--; name--;"
    [
      ExprStmt (SDcr (Var ("x", None), None), None);
      ExprStmt (SDcr (Var ("name", None), None), None);
    ];
  assert_body_parses_to "x = y--;"
    [
      ExprStmt
        (Assign (Var ("x", None), SDcr (Var ("y", None), None), None), None);
    ]

let test_exit _ =
  assert_body_parses_to "exit(); exit(-1);"
    [ Exit (None, None); Exit (Some (NumLiteral (-1, None)), None) ]

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
      defines = [];
      funcs =
        [
          {
            name = "fact";
            params = [ ("n", Int) ];
            body =
              [
                IfElse
                  ( BinOp (Eq, Var ("n", None), NumLiteral (0, None), None),
                    [ Return (Some (NumLiteral (1, None)), None) ],
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
                                           NumLiteral (1, None),
                                           None );
                                     ],
                                     None ),
                                 None )),
                          None );
                    ],
                    None );
              ];
            return_ty = Int;
            ctrl_reaches_end = None;
            loc = None;
          };
        ];
      main = empty_main;
    }

let test_defines _ =
  assert_parses_to "#define X 10 #define Y name void main() {}"
    {
      defines =
        [
          { var = "X"; expression = NumLiteral (10, None); loc = None };
          { var = "Y"; expression = Var ("name", None); loc = None };
        ];
      funcs = [];
      main = empty_main;
    }

let test_update _ =
  assert_body_parses_to "x *= 15;"
    [
      ExprStmt
        (SUpdate (Var ("x", None), NumLiteral (15, None), Mult, None), None);
    ];
  assert_body_parses_to "int y = z &= 3;"
    [
      Declare
        ( "y",
          Int,
          Some (SUpdate (Var ("z", None), NumLiteral (3, None), BAnd, None)),
          [],
          None );
    ]

let test_subscript _ =
  assert_body_parses_to "arr[15 + 2];"
    [
      ExprStmt
        ( SSubscript
            ( Var ("arr", None),
              BinOp (Plus, NumLiteral (15, None), NumLiteral (2, None), None),
              None ),
          None );
    ];
  (* subscript has higher precedence than deref *)
  assert_body_parses_to "*(p_array + 2)[x];"
    [
      ExprStmt
        ( Deref
            ( SSubscript
                ( BinOp (Plus, Var ("p_array", None), NumLiteral (2, None), None),
                  Var ("x", None),
                  None ),
              None ),
          None );
    ]

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
         "test_array_decl" >:: test_array_decl;
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
         "test_defines" >:: test_defines;
         "test_update" >:: test_update;
         "test_subscript" >:: test_subscript;
       ]

let () = run_test_tt_main suite
