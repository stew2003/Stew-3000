open Ast
open Asm.Isa
open Util.Env

type compile_err = UnboundVariable of string

exception CompileError of compile_err

let compile_func_defn (function_definition : func_defn) : instr list = []

let compile_stmt_lst (statements : stmt list) (bindings : int env) (si : int) :
    instr list =
  []

let compile_stmt (statement : stmt) (bindings : int env) (si : int) : instr list
    =
  match statement with
  | Let (name, value, scope) ->
      let ext_env = Env.add name si bindings in
      compile_expr value bindings si
      @ [ Sts (A, si) ]
      @ compile_stmt_lst scope ext_env (si + 1)

let compile_expr (expression : expr) (bindings : int env) (si : int) :
    instr list =
  match expression with
  | Num n -> [ Mvi (n, A) ]
  | Var x -> (
      match Env.find_opt x bindings with
      | Some x_si -> [ Lds (x_si, A) ]
      | None -> raise (CompileError (UnboundVariable x)))
  | _ -> []

let compile (program : prog) : instr list = []
