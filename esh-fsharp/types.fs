module Mal.Types

[<AbstractClass>]
type MalType() = class
  abstract member toString : string
  end

type MalList(parsed_list:MalType list) = class
  inherit MalType()
  member this.list = parsed_list
  override this.toString : string =
    let mutable str_list = "("
    this.list |> List.iter(fun x ->
                             str_list <- str_list + x.toString + " ")
    str_list <- str_list + ")"
    str_list
  end

type MalSymbol(symbol_name:string) = class
  inherit MalType()
  member this.name = symbol_name
  override this.toString : string = this.name
  end

type MalNumber(parsed_num:int) = class
  inherit MalType()
  member this.number = parsed_num
  override this.toString : string = string this.number
  end
