type t =
  | Int of int32
  | Str of string
  | Bool of bool
  | Tuple of t * t
  | Nil

let rec to_string json =
  match json with
  | Int i -> Int32.to_string i
  | Str s -> s
  | Bool b -> string_of_bool b
  | Tuple (a, b) -> "(" ^ to_string a ^ ", " ^ to_string b ^ ")"
  | Nil -> ""
;;

let to_term value =
  match value with
  | Int i -> Ast.Int i
  | Str s -> Ast.Str s
  | Bool b -> Ast.Bool b
  | _ -> failwith "Invalid value to term"
;;

let to_bool value =
  match value with
  | Int i -> i <> Int32.zero
  | Str s -> s <> ""
  | Bool b -> b
  | Tuple (_, _) -> false
  | Nil -> false
;;
