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
  let token_reg = "(~@|[\[\]{}()'`~^@]|\"([^\\\\\"]|(\\\\\\\"))*\"|;.*|[^\s\[\]{}('\"`,;)]*)"
  
  let cut_down_token_by_regex (str:string) (reg:string) =
    let m = Regex.Match(str, "^[\s,]*(" + reg + ")(.*$)")
    if m.Success then
      let values = [ for x in m.Groups -> x.Value ]
      let token = values.[1]
      // printfn "token: '%s', rest: '%s'" token (List.last values)
      { token = token ; rest = List.last values }
    else
      failwith ("ReaderError: this string has no valid token: " + str)
    
  let rec rec_tokenizer (result:string list) (rest:string) =
    if rest.Length = 0 then
      result
    else
      let tr = cut_down_token_by_regex rest token_reg
      if tr.rest = rest then
        failwith ("ReaderError: this string has no valid token: " + rest)
      if tr.token <> "" then
        rec_tokenizer (tr.token :: result) tr.rest
      else
        rec_tokenizer result tr.rest
  rec_tokenizer [] str |> List.rev

let rec private read_form (reader:Reader) =
  let read_list (reader:Reader) (right_paren:string) : MalType =
    let rec rec_read_list (result:MalType list) : MalType list =
      match reader.peek with
        | x when x = right_paren ->
          reader.next |> ignore
          List.rev result
        | ""  -> failwith "ReaderError: Unmatched parenthesis"
        | _ -> rec_read_list (read_form reader :: result)
    new MalList(rec_read_list []) :> _

  let read_quote (reader:Reader) : MalType =
    let quote_symbol = reader.next
    if reader.peek = "" then
      failwith "ReaderError: Quote requires one argument"
    let quote_name =
      match quote_symbol with
        | "'" -> "quote"
        | "`" -> "quasiquote"
        | "~" -> "unquote"
        | "~@" -> "splice-unquote"
        | _ -> failwith "ReaderError: \"" + quote_symbol + "\" is not a quote symbol"
    new MalList([ new MalSymbol(quote_name) ; read_form reader]) :> _
  
  let read_atom (reader:Reader) : MalType =
    let str = reader.next
    match Int32.TryParse(str) with
      | (true, int) -> new MalNumber(int) :> _
      | _ -> match str with
               | "true"  -> new MalBool(true) :> _
               | "false" -> new MalBool(false) :> _
               | "nil"   -> new MalNil() :> _
               | x when Regex.Match(x, "^\".*\"$").Success ->
                 new MalString(x.[1..(x.Length - 2)]) :> _
               | _ -> new MalSymbol(str) :> _
  
  match reader.peek with
    | "(" -> reader.next |> ignore
             read_list reader ")"
    | "[" -> reader.next |> ignore
             read_list reader "]"
    | "'" | "`" | "~" | "~@"
          -> read_quote reader
    | _ -> read_atom reader

let read_str (str:string) : MalType =
  try
    read_form (new Reader(tokenizer str))
  with
    | Failure msg -> new MalError(msg) :> _
