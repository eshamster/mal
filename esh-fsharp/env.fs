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
    let bind_variadic (rest_binds:MalSymbol list) (rest_exprs:MalType list) : _ =
      assert ((List.head rest_binds).Get = "&")
      if rest_binds.Length <> 2 then
        failwith "SyntaxError: the symbol is not directed to bind variadic parameteres"
      set (List.last rest_binds).Get (new MalList(rest_exprs))
      
    let rec bind (rest_binds:MalSymbol list) (rest_exprs:MalType list) : _ =
      assert (rest_binds.Length = rest_exprs.Length)
      if (not rest_binds.IsEmpty) then
        match (List.head rest_binds).Get with
          | "&" -> bind_variadic rest_binds rest_exprs
          | key ->
            if rest_exprs.IsEmpty then
              failwith err_msg
            set key (List.head rest_exprs)
            bind (List.tail rest_binds) (List.tail rest_exprs)
      else
        if (not rest_exprs.IsEmpty) then
          failwith err_msg
      
    match binds with
      | Some b ->
        match exprs with
          | Some e -> bind b e
          | None -> bind b []
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
