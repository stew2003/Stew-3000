open Ast
open Asm.Isa
open Util.Env
open Util.Srcloc

type compile_err = UnboundVariable of string * src_loc

exception CompileError of compile_err

(* [compile_expr] generates instructions for the given expression 
  in a given environment and stack index *)
let rec compile_expr (expression : expr) (bindings : int env) (si : int) :
    instr list =
  match expression with
  | Num (n, _) -> [ Mvi (n, A, None) ]
  | Var (x, loc) -> (
      match Env.find_opt x bindings with
      | Some x_si -> [ Lds (x_si, A, None) ]
      | None -> raise (CompileError (UnboundVariable (x, loc))))
  | _ -> failwith "not implemented"

(* [compile_stmt] generates instructions for a single statement,  
  in a given environment and stack index *)
and compile_stmt (statement : stmt) (bindings : int env) (si : int) : instr list
    =
  match statement with
  | Let (name, value, scope, _) ->
      let ext_env = Env.add name si bindings in
      compile_expr value bindings si
      @ [ Sts (A, si, None) ]
      @ compile_stmt_lst scope ext_env (si + 1)
  | _ -> failwith "not implemented"

(* [compile_stmt_lst] generates instructions for a list of statements,
  in an environment and stack index *)
and compile_stmt_lst (statements : stmt list) (bindings : int env) (si : int) :
    instr list =
  failwith "not implemented"

(* [compile_func_defn] generates instructions for a function definition *)
let compile_func_defn (function_definition : func_defn) : instr list =
  failwith "not implemented"

(* [compile] generates instructions for a complete program *)
let compile (program : prog) : instr list = failwith "not implemented"
