open Yojson.Safe.Util

type location =
  { start : int
  ; end_ : int
  ; filename : string
  }

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or

type term =
  | Int of int
  | Str of string
  | Bool of bool
  | Binary of binary
  | Call of call
  | Function of func
  | Let of let_
  | If of if_
  | Print of print
  | First of first
  | Second of second
  | Tuple of tuple
  | Var of var

and binary =
  { lhs : term
  ; op : binary_op
  ; rhs : term
  ; binary_location : location
  }

and call =
  { callee : term
  ; arguments : term list
  ; call_location : location
  }

and func =
  { parameters : var list
  ; value : term
  ; func_location : location
  }

and var =
  { text : string
  ; var_location : location
  }

and parameter =
  { parameter_text : string
  ; parameter_location : location
  }

and let_ =
  { name : parameter
  ; let_value : term
  ; next : term
  ; let_location : location
  }

and if_ =
  { condition : term
  ; then_ : term
  ; otherwise : term
  ; if_location : location
  }

and print =
  { print_value : term
  ; print_location : location
  }

and first =
  { first_value : term
  ; first_location : location
  }

and second =
  { second_value : term
  ; second_location : location
  }

and tuple =
  { first : term
  ; second : term
  ; tuple_location : location
  }

let binary_op_of_string = function
  | "Add" -> Add
  | "Sub" -> Sub
  | "Mul" -> Mul
  | "Div" -> Div
  | "Rem" -> Rem
  | "Eq" -> Eq
  | "Neq" -> Neq
  | "Lt" -> Lt
  | "Gt" -> Gt
  | "Lte" -> Lte
  | "Gte" -> Gte
  | "And" -> And
  | "Or" -> Or
  | _ -> failwith "Invalid binary operator"
;;

let location_of_json json =
  { start = json |> member "start" |> to_int
  ; end_ = json |> member "end" |> to_int
  ; filename = json |> member "filename" |> to_string
  }
;;

let var_of_json json =
  { text = json |> member "text" |> to_string
  ; var_location = location_of_json json
  }
;;

let parameter_of_json json =
  { parameter_text = json |> member "text" |> to_string
  ; parameter_location = location_of_json json
  }
;;

let rec term_of_json json =
  let kind = json |> member "kind" |> to_string in
  match kind with
  | "Int" -> Int (json |> member "value" |> to_int)
  | "Str" -> Str (json |> member "value" |> to_string)
  | "Bool" -> Bool (json |> member "value" |> to_bool)
  | "Binary" ->
    Binary
      { lhs = term_of_json (json |> member "lhs")
      ; op = binary_op_of_string (json |> member "op" |> to_string)
      ; rhs = term_of_json (json |> member "rhs")
      ; binary_location = location_of_json (json |> member "location")
      }
  | "Call" ->
    Call
      { callee = term_of_json (json |> member "callee")
      ; arguments =
          json |> member "arguments" |> to_list |> List.map term_of_json
      ; call_location = location_of_json (json |> member "location")
      }
  | "Function" ->
    Function
      { parameters =
          json |> member "parameters" |> to_list |> List.map var_of_json
      ; value = term_of_json (json |> member "value")
      ; func_location = location_of_json (json |> member "location")
      }
  | "Let" ->
    Let
      { name = parameter_of_json (json |> member "name")
      ; let_value = term_of_json (json |> member "value")
      ; next = term_of_json (json |> member "next")
      ; let_location = location_of_json (json |> member "location")
      }
  | "If" ->
    If
      { condition = term_of_json (json |> member "condition")
      ; then_ = term_of_json (json |> member "then")
      ; otherwise = term_of_json (json |> member "otherwise")
      ; if_location = location_of_json (json |> member "location")
      }
  | "Print" ->
    Print
      { print_value = term_of_json (json |> member "value")
      ; print_location = location_of_json (json |> member "location")
      }
  | "First" ->
    First
      { first_value = term_of_json (json |> member "value")
      ; first_location = location_of_json (json |> member "location")
      }
  | "Second" ->
    Second
      { second_value = term_of_json (json |> member "value")
      ; second_location = location_of_json (json |> member "location")
      }
  | "Tuple" ->
    Tuple
      { first = term_of_json (json |> member "first")
      ; second = term_of_json (json |> member "second")
      ; tuple_location = location_of_json (json |> member "location")
      }
  | "Var" -> Var (var_of_json json)
  | _ -> failwith "Invalid term kind"
;;