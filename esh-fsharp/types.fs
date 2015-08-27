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

type MalFunc(fn_process:MalType list -> MalType) = class
  inherit MalType()
  member this.fn = fn_process
  override this.ToString : string = ""
  member this.Call (args:MalType list) : MalType =
    fn_process args
  end

type MalError(error_msg:string) = class
  inherit MalType()
  member this.msg = error_msg
  override this.ToString : string = this.msg
  end
