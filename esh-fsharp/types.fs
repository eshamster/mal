module Mal.Types

[<AbstractClass>]
type MalType() = class
  abstract member ToString : string
  abstract member Equals : MalType -> bool
  default this.Equals (x:MalType) : bool = failwith "The Equals of this type is not implemented"
  end

type MalList(parsed_list:MalType list) = class
  inherit MalType()
  member this.list = parsed_list
  member this.Get : MalType list = this.list
  override this.ToString : string =
    let mutable str_list = []
    this.list |> List.iter(fun x ->
                             str_list <- x.ToString :: str_list)
    let internal_str : string =
      match str_list.IsEmpty with
        | false -> str_list <- List.rev str_list
                   str_list |> List.reduce (fun r s -> r + " " + s)
        | true -> ""
    "(" + internal_str + ")"

  override this.Equals (target:MalType) : bool =
    match target with
      | :? MalList as l ->
        if this.Get.Length <> l.Get.Length then
          false
        else
          let rec rec_compare_list (l1:MalType list) (l2:MalType list) : bool =
            assert (l1.Length = l2.Length)
            if l1.Length = 0 then
              true
            else
              if ((List.head l1).Equals (List.head l2)) then
                rec_compare_list (List.tail l1) (List.tail l2)
              else
                false  
          rec_compare_list this.Get l.Get
      | _ -> false
  end

type MalSymbol(symbol_name:string) = class
  inherit MalType()
  member this.name = symbol_name
  member this.Get : string = this.name
  override this.ToString : string = this.name
  end

type MalNumber(parsed_num:int) = class
  inherit MalType()
  member this.number = parsed_num
  member this.Get : int = this.number
  override this.ToString : string = string this.number
  override this.Equals (target:MalType) : bool =
    match target with
      | :? MalNumber as n -> this.Get = n.Get
      | _ -> false
  end

type MalString(parsed_str:string) = class
  inherit MalType()
  let str = parsed_str
  member this.Get : string = str
  override this.ToString : string = "\"" + str + "\""
  override this.Equals (target:MalType) : bool =
    match target with
      | :? MalString as s -> this.Get = s.Get
      | _ -> false
  end

type MalBool(parsed_bool:bool) = class
  inherit MalType()
  member this.value = parsed_bool
  member this.Get : bool = this.value
  override this.ToString : string = if this.value then "true" else "false"
  override this.Equals (target:MalType) : bool =
    match target with
      | :? MalBool as b -> this.Get = b.Get
      | _ -> false
  end

type MalNil() = class
  inherit MalType()
  override this.ToString : string = "nil"
  override this.Equals (target:MalType) : bool =
    match target with
      | :? MalNil -> true
      | _ -> false
  end

type MalBuiltinFunc(fn_process:MalType list -> MalType) = class
  inherit MalType()
  member this.fn = fn_process
  override this.ToString : string = "#<built-in function>"
  member this.Call (args:MalType list) : MalType =
    fn_process args
  end

type MalFunc(its_binds:MalSymbol list, its_procedure:MalType, its_env:obj) = class
  inherit MalType()
  let binds = its_binds
  let procedure = its_procedure
  let env : obj = its_env
  override this.ToString : string = "#<function>"
  member this.Binds = binds
  member this.Procedure = procedure
  member this.Env = env
  end

type MalError(error_msg:string) = class
  inherit MalType()
  member this.msg = error_msg
  override this.ToString : string = this.msg
  end
