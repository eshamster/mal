open System
open System.Collections.Generic;
open Mal.Env
open Mal.Types
open Mal.Reader
open Mal.Printer

let MakeNumberMulFunc (fn:int -> int -> int) : MalFunc =
  new MalFunc(fun args ->
              new MalNumber (fn (args.[0] :?> MalNumber).Get
                                (args.[1] :?> MalNumber).Get)
              :> _)
  
let repl_env = new Env(Option<Env>.None)

let init () : _ =
  repl_env.Set "+" (MakeNumberMulFunc (fun x y -> x + y))
  repl_env.Set "-" (MakeNumberMulFunc (fun x y -> x - y))
  repl_env.Set "/" (MakeNumberMulFunc (fun x y -> x / y))
  repl_env.Set "*" (MakeNumberMulFunc (fun x y -> x * y))

let rec EVAL (data:MalType) (env:Env) : MalType =
  let eval_ast (ast:MalType) (env:Env) : MalType =
    match ast with
      | :? MalSymbol as s ->
          let name : string = s.Get
          env.Get name
      | :? MalList as s ->
          let rec rec_evaled_list result (rest:MalType list) : MalType list =
            if rest.IsEmpty then
              result |> List.rev
            else
              rec_evaled_list ((EVAL (List.head rest) env) :: result)
                              (List.tail rest)
          new MalList(rec_evaled_list [] s.Get) :> _
      | _ -> ast

  let set_symbol_to_env (list:MalType list) (env:Env) : MalType =
    if list.Length < 2 then
      failwith "SyntaxError: Symbol definition requires 2 parameters"
    let key_obj = list.[0] :?> MalSymbol
    let value = EVAL list.[1] env
    env.Set key_obj.Get value
    value

  let eval_def_ex (rest:MalType list) (env:Env) : MalType =
    if rest.Length <> 2 then
      failwith "SyntaxError: 'def!' requires 2 parameters"
    set_symbol_to_env rest env

  let eval_let_ast (rest:MalType list) (env:Env) : MalType =
    let rec rec_set_to_env (defines:MalType list) (env:Env) : _ =
      match defines.Length with
        | 0 -> ()
        | x when x % 2 = 0 ->
            set_symbol_to_env defines env |> ignore
            rec_set_to_env (defines |> List.tail |> List.tail) env
        | _ -> failwith "SyntaxError: 'let*' requires even number parameters"
    if rest.Length <> 2 then
      failwith "SyntaxError: 'let*' requires 2 parameters"
    if not (rest.[0] :? MalList) then
      failwith ("SyntaxError: 'let*' requires a list as the definition of symbols: " + rest.[0].ToString)
    let new_env = new Env(Some env)
    rec_set_to_env (rest.[0] :?> MalList).Get new_env
    EVAL rest.[1] new_env
      
  match data with
    | :? MalList as l ->
      match (List.head l.Get) with
        | :? MalSymbol as s ->
          match s.Get with
            | "def!" -> eval_def_ex (List.tail l.Get) env
            | "let*" -> eval_let_ast (List.tail l.Get) env
            | _ -> let list = (eval_ast l env :?> MalList).Get
                   (List.head list :?> MalFunc).Call (List.tail list)
        | _ -> let list = (eval_ast l env :?> MalList).Get
               (List.head list :?> MalFunc).Call (List.tail list)
    | _ -> eval_ast data env

let READ (str:string) : MalType =
  read_str str

let PRINT (data:MalType) : unit =
  printfn "%s" (pr_str data)

let rec rep() =
  printf "user> "
  let msg = Console.ReadLine()
  try
    PRINT (EVAL (READ msg) repl_env) |> ignore
  with
    | Failure msg -> printfn "%s" msg
  rep()

[<EntryPoint>]
let main args =
  init() |> ignore
  rep() |> ignore
  0
