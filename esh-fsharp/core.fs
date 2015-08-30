module Mal.Core

open Env
open Types

  
let repl_env = new Env()

let private set_numeric_funcs () : _ =
  let make_numeric_func (fn:int -> int -> MalType) : MalBuiltinFunc =
    new MalBuiltinFunc(
      fun args -> fn (args.[0] :?> MalNumber).Get
                     (args.[1] :?> MalNumber).Get)

  let make_op_func (fn:int -> int -> int) : MalBuiltinFunc =
    make_numeric_func (fun x y -> new MalNumber(fn x y) :> _)
    
  let make_compare_func (fn:int -> int -> bool) : MalBuiltinFunc =
    make_numeric_func (fun x y -> new MalBool(fn x y) :> _)
    
  repl_env.Set "+" (make_op_func (fun x y -> x + y))
  repl_env.Set "-" (make_op_func (fun x y -> x - y))
  repl_env.Set "/" (make_op_func (fun x y -> x / y))
  repl_env.Set "*" (make_op_func (fun x y -> x * y))
  
  repl_env.Set "<"  (make_compare_func (fun x y -> x <  y))
  repl_env.Set "<=" (make_compare_func (fun x y -> x <= y))
  repl_env.Set ">"  (make_compare_func (fun x y -> x >  y))
  repl_env.Set ">=" (make_compare_func (fun x y -> x >= y))

let private set_str_funcs () : _ =
  let make_str_func () : MalBuiltinFunc =
    new MalBuiltinFunc(
      fun args ->
        new MalString(
          args |> List.map (fun x -> x.ToStringReadably.Replace("\"", "\\\""))
               |> String.concat "") :> _)
    
  let make_pr_str_func () : MalBuiltinFunc =
    new MalBuiltinFunc(
      fun args ->
        new MalString(
          args |> List.map (fun x -> x.ToStringWithEscape)
               |> String.concat " ") :> _)

  let make_prn_func () : MalBuiltinFunc =
    new MalBuiltinFunc(
      fun args ->
        printfn "%s" (args |> List.map (fun x -> x.ToString)
                           |> String.concat " ")
        new MalNil() :> _)

  let make_println_func () : MalBuiltinFunc =
    new MalBuiltinFunc(
      fun args ->
        printfn "%s" (args |> List.map (fun x -> x.ToStringReadably)
                           |> String.concat " ")
        new MalNil() :> _)

  repl_env.Set "str" (make_str_func())
  repl_env.Set "pr-str" (make_pr_str_func())
  repl_env.Set "prn" (make_prn_func())
  repl_env.Set "println" (make_println_func())

let private set_list_funcs () : _ =
  let make_list_func () : MalBuiltinFunc =
    new MalBuiltinFunc(fun args -> new MalList(args) :> _)
    
  let make_listp_func () : MalBuiltinFunc =
    new MalBuiltinFunc(
      fun args -> new MalBool(args.[0] :? MalList) :> _)

  let make_a_list_func (fn:MalType list -> MalType) : MalBuiltinFunc =
    new MalBuiltinFunc(
      fun args ->
        if args.Length <> 1 then
          failwith "SyntaxError: This function requires one argument"
        match args.[0] with
          | :? MalList as s -> fn s.Get
          | :? MalNil  as n -> fn []
          | _ -> failwith "SyntaxError: This function requires a list as the parameter")
    
  let make_emptyp_func () : MalBuiltinFunc =
    make_a_list_func (fun list -> new MalBool(list.IsEmpty) :> _)
    
  let make_count_func () : MalBuiltinFunc =
    make_a_list_func (fun list -> new MalNumber(list.Length) :> _)
  
  repl_env.Set "list"   (make_list_func())
  repl_env.Set "list?"  (make_listp_func())
  repl_env.Set "empty?" (make_emptyp_func())
  repl_env.Set "count"  (make_count_func())

let private set_other_funcs () : _ =
  let make_eq_func () : MalBuiltinFunc =
    new MalBuiltinFunc(
      fun args ->
        new MalBool(
          if args.Length <> 2 then
            failwith (sprintf "SyntaxError: '=' requires 2 arguments but takes %d argument[s]" args.Length)
          args.[0].Equals(args.[1])) :> _)

  let make_not_func() : MalBuiltinFunc =
    new MalBuiltinFunc(
      fun args ->
        new MalBool(
          if args.Length <> 1 then
            failwith (sprintf "SyntaxError: 'not' requires 1 argument but takes %d argument[s]" args.Length)
          match args.[0] with
            | :? MalNil -> true
            | :? MalBool as b -> not (b.Get)
            | :? MalList as l -> (l.Get).Length = 0
            | _ -> false) :> _)
    
  repl_env.Set "=" (make_eq_func())
  repl_env.Set "not" (make_not_func())

let init () : _ =
  set_numeric_funcs()
  set_str_funcs()
  set_list_funcs()
  set_other_funcs()
