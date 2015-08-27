module Mal.Printer

open Mal.Types

let pr_str (data:MalType) : string =
  data.ToString
