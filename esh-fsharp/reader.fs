module Mal.Reader

open System.Text.RegularExpressions

type Reader() = class
  member this.next = ()
  member this.peek = ()
  end

let private read_str = ()
let private read_from = ()

type private TokenAndRest = { token: string ; rest: string }

let private tokenizer str =
  let token_reg = "[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\s\[\]{}('\"`,;)]*)"
  
  // TODO: Error Processing
  let cut_down_by_regex str reg =
    let m = Regex.Match(str, "^(" + reg + ")(.*$)")
    let values = [ for x in m.Groups -> x.Value ]
    { token = values.[1] ; rest = List.last values }
    
  let rec rec_tokenizer result (rest:string) =
    if rest.Length = 0 then
      result
    else
      let tr = cut_down_by_regex rest token_reg
      rec_tokenizer (tr.token.Trim() :: result) tr.rest
  rec_tokenizer [] str |> List.rev

let private read_list = ()
let private read_atom = ()

let test() =
  tokenizer ",, ,,(12 13 1)" |> List.iter (fun token -> printfn "token: %s" token)
  0
