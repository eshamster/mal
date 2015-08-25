module Mal.Reader

open System
open System.Text.RegularExpressions
open Mal.Types

type Reader(tokens : string list) = class
  let mutable rest_tokens = tokens

  member this.next : string =
    match rest_tokens with
      | lst when Seq.isEmpty lst -> ""
      | _  -> let result = List.head rest_tokens
              rest_tokens <- List.tail rest_tokens
              result

  member this.peek : string =
    match rest_tokens with
      | lst when Seq.isEmpty lst -> ""
      | _  -> List.head rest_tokens
  end

type private TokenAndRest = { token: string ; rest: string }

let private tokenizer (str:string) =
  let token_reg = "[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\s\[\]{}('\"`,;)]*)"
  
  let cut_down_token_by_regex (str:string) (reg:string) =
    let m = Regex.Match(str, "^(" + reg + ")(.*$)")
    if m.Success then
      let values = [ for x in m.Groups -> x.Value ]
      { token = values.[1] ; rest = List.last values }
    else
      failwith ("ReaderError: this string has no valid token: " + str)
    
  let rec rec_tokenizer (result:string list) (rest:string) =
    if rest.Length = 0 then
      result
    else
      let tr = cut_down_token_by_regex rest token_reg
      rec_tokenizer (tr.token.Trim() :: result) tr.rest
  rec_tokenizer [] str |> List.rev

let private read_str (str:string) : Reader = 
  let tokens = tokenizer str
  new Reader(tokens)

let rec private read_form (reader:Reader) =
  let read_list (reader:Reader) : MalType =
    let mutable list = []
    while reader.peek <> ")" do
      if reader.peek = "" then
        failwith "ReaderError: Unmatched parenthesis"
      list <- read_form reader :: list
    reader.next |> ignore
    new MalList(List.rev list) :> _
  
  let read_atom (reader:Reader) : MalType =
    let str = reader.next
    match Int32.TryParse(str) with
      | (true, int) -> new MalNumber(int) :> _
      | _ -> new MalSymbol(str) :> _
  
  let first = reader.peek
  if first = "(" then
    reader.next |> ignore
    read_list reader
  else
    read_atom reader

let test() =
  // tokenizer ",, ,,(12 13 1)" |> List.iter (fun x -> printfn "%s" x)
  let reader = read_str "(12 (13 (3 2) 3) 1)"
  let tokens = read_form reader
  printfn "%s" tokens.toString
  0
