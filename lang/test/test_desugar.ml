open OUnit2
open Testing_utils
open Compiler
open Ast

(* [from_source] parses, preprocesses, and desugars a source program *)
let from_source (source : string) : prog =
  let pgrm = Parser.parse source in
  let pgrm = Preprocess.preprocess pgrm in
  Desugar.desugar pgrm

(* [from_body] wraps from_source to eliminate the main() boilerplate *)
let from_body (body : string) : prog =
  from_source (Printf.sprintf "void main() { %s }" body)

let test_inr _ =
  assert_prog_eq (from_body "x++;") (from_body "x = x + 1;");
  assert_prog_eq (from_body "(*p)++;") (from_body "*p = *p + 1;")

let test_dcr _ =
  assert_prog_eq (from_body "x--;") (from_body "x = x - 1;");
  assert_prog_eq (from_body "(*p)--;") (from_body "*p = *p - 1;")

let test_update _ =
  assert_prog_eq
    (from_body "v += 10; y /= 2 + 3; *px |= 0b101;")
    (from_body "v = v + 10; y = y / (2 + 3); *px = *px | 0b101;")

(* This is a bit inconvenient since the `any` type isn't accessible
   to the outside world. *)
let test_subscript _ =
  assert_prog_eq (from_body "arr[40];")
    {
      defines = [];
      funcs = [];
      main =
        {
          name = "main";
          params = [];
          body =
            [
              ExprStmt
                ( Deref
                    ( BinOp
                        ( Plus,
                          Var ("arr", None),
                          Cast (Any, NumLiteral (40, None), None),
                          None ),
                      None ),
                  None );
            ];
          return_ty = Void;
          ctrl_reaches_end = None;
          loc = None;
        };
    };
  assert_prog_eq
    (from_body "(*(p + 10))[2 + 4];")
    {
      defines = [];
      funcs = [];
      main =
        {
          name = "main";
          params = [];
          body =
            [
              ExprStmt
                ( Deref
                    ( BinOp
                        ( Plus,
                          Deref
                            ( BinOp
                                ( Plus,
                                  Var ("p", None),
                                  NumLiteral (10, None),
                                  None ),
                              None ),
                          Cast
                            ( Any,
                              BinOp
                                ( Plus,
                                  NumLiteral (2, None),
                                  NumLiteral (4, None),
                                  None ),
                              None ),
                          None ),
                      None ),
                  None );
            ];
          return_ty = Void;
          ctrl_reaches_end = None;
          loc = None;
        };
    }

let suite =
  "Desugaring Tests"
  >::: [
         "test_inr" >:: test_inr;
         "test_dcr" >:: test_dcr;
         "test_update" >:: test_update;
         "test_subscript" >:: test_subscript;
       ]

let () = run_test_tt_main suite
