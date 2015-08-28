module Mal.Env

open System.Collections.Generic;
open Mal.Types

type Env(its_outer:Env option) = class
  let outer = its_outer
  let data = new Dictionary<string, MalType>()

  member this.Set (key:string) (value:MalType) : _ =
    if data.ContainsKey key then
      data.[key] <- value
    else
      data.Add(key, value)

  member this.Find (key:string) : Env option =
    if data.ContainsKey key then
      Some this
    else
      match outer with
        | Some x -> x.Find key
        | None -> Option<Env>.None

  member this.Data() = data

  member this.Get (key:string) : MalType =
    match this.Find key with
      | Some x -> x.Data().[key]
      | None -> failwith ("The symbol is not defined: " + key)
  end
