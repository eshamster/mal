module Mal.Types

[<AbstractClass>]
type MalType() = class
  abstract member ToString : string
  end

type MalList(parsed_list:MalType list) = class
  inherit MalType()
  member this.list = parsed_list
  member this.Get : MalType list = this.list
  override this.ToString : string =
    let mutable str_list = []
    this.list |> List.iter(fun x ->
                             str_list <- x.ToString :: str_list)
    str_list <- List.rev str_list
    let internal_str = str_list |>
                         List.reduce (fun r s -> r + " " + s)
    "(" + internal_str + ")"
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
  end

type MalBool(parsed_bool:bool) = class
  inherit MalType()
  member this.value = parsed_bool
  member this.Get : bool = this.value
  override this.ToString : string = if this.value then "true" else "false"
  end

type MalNil() = class
  inherit MalType()
  override this.ToString : string = "nil"
  end

type MalBuiltinFunc(fn_process:MalType list -> MalType) = class
  inherit MalType()
  member this.fn = fn_process
  override this.ToString : string = "#<built-in function>"
  member this.Call (args:MalType list) : MalType =
    fn_process args
  end

type MalFunc(its_binds:MalSymbol list, its_procedure:MalType) = class
  inherit MalType()
  let binds = its_binds
  let procedure = its_procedure
  override this.ToString : string = "#<function>"
  member this.Binds = binds
  member this.Procedure = procedure
  end

type MalError(error_msg:string) = class
  inherit MalType()
  member this.msg = error_msg
  override this.ToString : string = this.msg
  end
