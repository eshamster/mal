module Mal.Types

[<AbstractClass>]
type MalType() = class
  abstract member ToString : string
  abstract member Eval : MalType list -> MalType
  default this.Eval _ = this
  end

type MalList(parsed_list:MalType list) = class
  inherit MalType()
  member this.list = parsed_list
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
  override this.ToString : string = this.name
  end

type MalNumber(parsed_num:int) = class
  inherit MalType()
  member this.number = parsed_num
  override this.ToString : string = string this.number
  end

type MalFunc(fn_process:MalType list -> MalType) = class
  inherit MalType()
  member this.fn = fn_process
  override this.ToString : string = ""
  override this.Eval (args:MalType list) : MalType =
    fn_process args
  end

type MalError(error_msg:string) = class
  inherit MalType()
  member this.msg = error_msg
  override this.ToString : string = this.msg
  override this.Eval _ = failwith this.msg
  end
