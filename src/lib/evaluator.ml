type return_values =
  | Int of int
  | Str of string
  | Bool of bool
  | Tuple of return_values * return_values
  | Nil

let rec return_value_to_string json =
  match json with
  | Int i -> string_of_int i
  | Str s -> s
  | Bool b -> string_of_bool b
  | Tuple (a, b) ->
    "(" ^ return_value_to_string a ^ ", " ^ return_value_to_string b ^ ")"
  | Nil -> ""
;;

let rec eval_term term ctx call_stack =
  match term with
  | Ast.Let _t -> failwith "Not implemented"
  | Ast.Function _t -> failwith "Not implemented"
  | Ast.Call _t -> failwith "Not implemented"
  | Ast.Var _t -> failwith "Not implemented"
  | Ast.Tuple _t -> failwith "Not implemented"
  | Ast.First _t -> failwith "Not implemented"
  | Ast.Second _t -> failwith "Not implemented"
  | Ast.If _t -> failwith "Not implemented"
  | Ast.Print p ->
    let () =
      eval_term p.print_value ctx call_stack
      |> return_value_to_string
      |> print_endline
    in
    Nil
  | Ast.Binary _t -> failwith "Not implemented"
  | Ast.Int t -> Int t
  | Ast.Str t -> Str t
  | Ast.Bool t -> Bool t
;;

let eval_string ast =
  let call_stack : Ast.term Stack.t = Stack.create () in
  let ctx : (string, Ast.term) Hashtbl.t = Hashtbl.create 10 in
  let ast_json = Yojson.Safe.from_string ast in
  let expression = ast_json |> Yojson.Safe.Util.member "expression" in
  let term = Ast.term_of_json expression in
  eval_term term ctx call_stack
;;
