open System
open Mal.Reader

let READ str =
  str

let EVAL str =
  str

let PRINT str =
  printfn "%s" str
  str

let ignore _ = ()

let rec rep() =
  printf "user> "
  let msg = Console.ReadLine()
  PRINT (EVAL (READ msg)) |> ignore
  rep()

[<EntryPoint>]
let main args =
  rep() |> ignore
  0
