open System
open System.Collections.Generic;
open Mal.Types
open Mal.Reader
open Mal.Printer

let READ (str:string) : MalType =
  read_str str

let EVAL (data:MalType) : MalType =
  data

let PRINT (data:MalType) : unit =
  printfn "%s" (pr_str data)

let rec rep() =
  printf "user> "
  let msg = Console.ReadLine()
  PRINT (EVAL (READ msg)) |> ignore
  rep()

[<EntryPoint>]
let main args =
  
  let d = new Dictionary<string, MalType>()
  
  rep() |> ignore
  0
