type return_values =
  | Int of int
  | Str of string
  | Bool of bool
  | Tuple of return_values * return_values
  | Nil

let rec value_to_string json =
  match json with
  | Int i -> string_of_int i
  | Str s -> s
  | Bool b -> string_of_bool b
  | Tuple (a, b) -> "(" ^ value_to_string a ^ ", " ^ value_to_string b ^ ")"
  | Nil -> ""
;;

let value_to_term value =
  match value with
  | Int i -> Ast.Int i
  | Str s -> Ast.Str s
  | Bool b -> Ast.Bool b
  | _ -> failwith "Invalid value to term"
;;

let value_to_bool value =
  match value with
  | Int i -> i <> 0
  | Str s -> s <> ""
  | Bool b -> b
  | Tuple (_, _) -> false
  | Nil -> false
;;

let add_or_replace_hashtbl ctx key value =
  if Hashtbl.mem ctx key
  then Hashtbl.replace ctx key value
  else Hashtbl.add ctx key value
;;

let clone_hashtbl h =
  let h' = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun k v -> add_or_replace_hashtbl h' k v) h;
  h'
;;

let rec eval_term term ctx call_stack cache =
  match term with
  | Ast.Let l ->
    add_or_replace_hashtbl ctx l.name.parameter_text l.let_value;
    eval_term l.next ctx call_stack cache
  | Ast.Function f ->
    let ctx_copy = Hashtbl.copy ctx in
    (* TODO: this is a hack *)
    let call_list = List.of_seq (Stack.to_seq call_stack) |> List.rev in
    let () =
      List.iter2
        (fun (var : Ast.var) arg ->
          add_or_replace_hashtbl ctx_copy var.text arg)
        f.parameters
        call_list
    in
    let a = Hashtbl.find_opt cache (f, ctx_copy) in
    (match a with
     | Some v -> v
     | None ->
       let value = eval_term f.value ctx_copy call_stack cache in
       add_or_replace_hashtbl cache (f, ctx_copy) value;
       value)
  | Ast.Call c ->
    let stack : Ast.term Stack.t = Stack.create () in
    let () =
      List.iter
        (fun arg ->
          let evaluated_arg = eval_term arg ctx stack cache in
          Stack.push (evaluated_arg |> value_to_term) stack)
        c.arguments
    in
    let value = eval_term c.callee ctx stack cache in
    value
  | Ast.Var v ->
    let var =
      match Hashtbl.find_opt ctx v.text with
      | Some v -> v
      | None -> failwith "Variable not found"
    in
    eval_term var ctx call_stack cache
  | Ast.Tuple t ->
    Tuple
      ( eval_term t.first ctx call_stack cache
      , eval_term t.second ctx call_stack cache )
  | Ast.First f ->
    let v = eval_term f.first_value ctx call_stack cache in
    (match v with
     | Tuple (a, _) -> a
     | _ -> failwith "Not a tuple in a first call")
  | Ast.Second s ->
    let v = eval_term s.second_value ctx call_stack cache in
    (match v with
     | Tuple (_, b) -> b
     | _ -> failwith "Not a tuple in a second call")
  | Ast.If i ->
    let value = eval_term i.condition ctx call_stack cache in
    if value_to_bool value
    then eval_term i.then_term ctx call_stack cache
    else eval_term i.otherwise ctx call_stack cache
  | Ast.Print p ->
    let value = eval_term p.print_value ctx call_stack cache in
    value |> value_to_string |> print_endline;
    value
  | Ast.Binary b ->
    let lhs = eval_term b.lhs ctx call_stack cache in
    let rhs = eval_term b.rhs ctx call_stack cache in
    (match b.op, lhs, rhs with
     | Add, Int a, Int b -> Int (a + b)
     | Add, Str a, Str b -> Str (a ^ b)
     | Add, Int a, Str b -> Str (string_of_int a ^ b)
     | Add, Str a, Int b -> Str (a ^ string_of_int b)
     | Sub, Int a, Int b -> Int (a - b)
     | Mul, Int a, Int b -> Int (a * b)
     | Div, Int a, Int b -> Int (a / b)
     | Rem, Int a, Int b -> Int (a mod b)
     | Eq, Int a, Int b -> Bool (a = b)
     | Neq, Int a, Int b -> Bool (a <> b)
     | Lt, Int a, Int b -> Bool (a < b)
     | Gt, Int a, Int b -> Bool (a > b)
     | Lte, Int a, Int b -> Bool (a <= b)
     | Gte, Int a, Int b -> Bool (a >= b)
     | And, Bool a, Bool b -> Bool (a && b)
     | Or, Bool a, Bool b -> Bool (a || b)
     | _ -> failwith "Invalid types in binary operation")
  | Ast.Int t -> Int t
  | Ast.Str t -> Str t
  | Ast.Bool t -> Bool t
;;

let eval ast =
  let cache
    : (Ast.func * (string, Ast.term) Hashtbl.t, return_values) Hashtbl.t
    =
    Hashtbl.create 100
  in
  let call_stack : Ast.term Stack.t = Stack.create () in
  let ctx : (string, Ast.term) Hashtbl.t = Hashtbl.create 100 in
  let ast_json = Yojson.Safe.from_string ast in
  let expression = ast_json |> Yojson.Safe.Util.member "expression" in
  let term = Ast.term_of_json expression in
  let _ = eval_term term ctx call_stack cache in
  ()
;;
