type t =
  | Int of int32
  | Str of string
  | Bool of bool
  | Tuple of t * t
  | Fn of (string, t) Hashtbl.t * string list * Ast.term

let rec to_string json =
  match json with
  | Int i -> Int32.to_string i
  | Str s -> s
  | Bool b -> string_of_bool b
  | Tuple (a, b) -> "(" ^ to_string a ^ ", " ^ to_string b ^ ")"
  | Fn (_, _, _) -> "<#closure>"
;;

let rec to_term value =
  match value with
  | Int i -> Ast.Int i
  | Str s -> Ast.Str s
  | Bool b -> Ast.Bool b
  | Tuple (a, b) ->
    Ast.Tuple
      { first = to_term a
      ; second = to_term b
      ; tuple_location =
          { start = Int32.zero; end_ = Int32.zero; filename = "" }
      }
  | Fn (_, _, _) -> failwith "Cannot convert closure to term"
;;

let to_bool value =
  match value with
  | Int i -> i <> Int32.zero
  | Str s -> s <> ""
  | Bool b -> b
  | Tuple (_, _) -> failwith "Cannot convert tuple to bool"
  | Fn (_, _, _) -> failwith "Cannot convert closure to bool"
;;
