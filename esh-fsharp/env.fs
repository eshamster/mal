module Mal.Env

open System.Collections.Generic;
open Mal.Types

type Env() = class
  let mutable outer = Option<Env>.None
  let data = new Dictionary<string, MalType>()

  member this.Set (key:string) (value:MalType) : _ =
    data.Add(key, value)

  member this.Find (key:string) : Env option =
    if data.ContainsKey key then
      Some this
    else
      match outer with
        | Some x -> x.Find key
        | None -> Option<Env>.None

  member this.Get (key:string) : MalType =
    match this.Find key with
      | Some _ -> data.[key]
      | None -> failwith ("The symbol is not defined: " + key)
  end
