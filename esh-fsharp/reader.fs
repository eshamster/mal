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

let rec private read_form (reader:Reader) =
  let read_list (reader:Reader) : MalType =
    let rec rec_read_list (result:MalType list) : MalType list =
      match reader.peek with
        | ")" -> reader.next |> ignore
                 List.rev result
        | ""  -> failwith "ReaderError: Unmatched parenthesis"
        | _ -> rec_read_list (read_form reader :: result)
    new MalList(rec_read_list []) :> _
  
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

let read_str (str:string) : MalType = 
  read_form (new Reader(tokenizer str))
