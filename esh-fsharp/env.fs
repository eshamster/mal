module Mal.Env

open System.Collections.Generic;
open Mal.Types

type Env (its_outer:Env option, binds:MalSymbol list option, exprs:MalType list option) = class  
  let outer = its_outer
  let data = new Dictionary<string, MalType>()
  let set (key:string) (value:MalType) : _ =
    if data.ContainsKey key then
      data.[key] <- value
    else
      data.Add(key, value)
  
  do
    let err_msg = "InternalError: the length of binds and the length of exprs is not same in Env"
    let rec bind (rest_binds:MalSymbol list) (rest_exprs:MalType list) : _ =
      assert (rest_binds.Length = rest_exprs.Length)
      if not rest_binds.IsEmpty then
        set (List.head rest_binds).Get (List.head rest_exprs)
        bind (List.tail rest_binds) (List.tail rest_exprs)
      
    match binds with
      | Some b ->
        match exprs with
          | Some e ->
            if b.Length <> e.Length then
              failwith err_msg
            else
              bind b e
          | None -> failwith err_msg
      | None ->
        match exprs with
          | Some _ -> failwith err_msg
          | None -> ()

  member this.Set (key:string) (value:MalType) : _ =
    set key value

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
      
  new (its_outer:Env option) = Env(its_outer, Option<MalSymbol list>.None, Option<MalType list>.None)
  new () = Env(Option<Env>.None)
  end
