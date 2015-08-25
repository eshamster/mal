module Mal.Reader

open System.Text.RegularExpressions

type Reader(tokens : string list) = class
  let mutable rest_tokens = tokens

  member this.next : string =
    let result = List.head rest_tokens
    rest_tokens <- List.tail rest_tokens
    result
    
  member this.peek : string =
    List.head rest_tokens
  end

type private TokenAndRest = { token: string ; rest: string }

let private tokenizer (str:string) =
  let token_reg = "[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\s\[\]{}('\"`,;)]*)"
  
  // TODO: Error Processing
  let cut_down_by_regex (str:string) (reg:string) =
    let m = Regex.Match(str, "^(" + reg + ")(.*$)")
    let values = [ for x in m.Groups -> x.Value ]
    { token = values.[1] ; rest = List.last values }
    
  let rec rec_tokenizer (result:string list) (rest:string) =
    if rest.Length = 0 then
      result
    else
      let tr = cut_down_by_regex rest token_reg
      rec_tokenizer (tr.token.Trim() :: result) tr.rest
  rec_tokenizer [] str |> List.rev

let private read_str (str:string) : Reader = 
  let tokens = tokenizer str
  new Reader(tokens)

let private read_from = ()
let private read_list = ()
let private read_atom = ()

let test() =
  tokenizer ",, ,,(12 13 1)" |> List.iter (fun x -> printfn "%s" x)
  let reader = read_str ",, ,,(12 13 1)"
  0
