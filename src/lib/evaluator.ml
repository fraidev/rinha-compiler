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

let add_or_replace ctx key value =
  if Hashtbl.mem ctx key
  then Hashtbl.replace ctx key value
  else Hashtbl.add ctx key value
;;

let clone_hashtbl h =
  let h' = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun k v -> add_or_replace h' k v) h;
  h'
;;

let rec eval_term term ctx call_stack =
  match term with
  | Ast.Let l ->
    add_or_replace ctx l.name.parameter_text l.let_value;
    eval_term l.next ctx call_stack
  | Ast.Function f ->
    let ctx_copy = Hashtbl.copy ctx in
    let call_list = List.of_seq (Stack.to_seq call_stack) |> List.rev in
    let () =
      List.iter2
        (fun (var : Ast.var) arg -> add_or_replace ctx_copy var.text arg)
        f.parameters
        call_list
    in
    eval_term f.value ctx_copy call_stack
  | Ast.Call c ->
    let stack : Ast.term Stack.t = Stack.create () in
    let () =
      List.iter
        (fun arg ->
          let evaluated_arg = eval_term arg ctx stack |> value_to_term in
          Stack.push evaluated_arg stack)
        c.arguments
    in
    let value = eval_term c.callee ctx stack in
    value
  | Ast.Var v ->
    let var =
      match Hashtbl.find_opt ctx v.text with
      | Some v -> v
      | None -> failwith "Variable not found"
    in
    eval_term var ctx call_stack
  | Ast.Tuple t ->
    Tuple (eval_term t.first ctx call_stack, eval_term t.second ctx call_stack)
  | Ast.First f ->
    let v = eval_term f.first_value ctx call_stack in
    (match v with
     | Tuple (a, _) -> a
     | _ -> failwith "Not a tuple in a first call")
  | Ast.Second s ->
    let v = eval_term s.second_value ctx call_stack in
    (match v with
     | Tuple (_, b) -> b
     | _ -> failwith "Not a tuple in a second call")
  | Ast.If i ->
    let value = eval_term i.condition ctx call_stack in
    if value_to_bool value
    then eval_term i.then_term ctx call_stack
    else eval_term i.otherwise ctx call_stack
  | Ast.Print p ->
    let () =
      eval_term p.print_value ctx call_stack
      |> value_to_string
      |> print_endline
    in
    Nil
  | Ast.Binary b ->
    let lhs = eval_term b.lhs ctx call_stack in
    let rhs = eval_term b.rhs ctx call_stack in
    (match b.op with
     | Add ->
       (match lhs, rhs with
        | Int a, Int b -> Int (a + b)
        | Str a, Str b -> Str (a ^ b)
        | _ -> failwith "Invalid types in add")
     | Sub ->
       (match lhs, rhs with
        | Int a, Int b -> Int (a - b)
        | _ -> failwith "Invalid types in sub")
     | Mul ->
       (match lhs, rhs with
        | Int a, Int b -> Int (a * b)
        | _ -> failwith "Invalid types in mul")
     | Div ->
       (match lhs, rhs with
        | Int a, Int b -> Int (a / b)
        | _ -> failwith "Invalid types in div")
     | Rem ->
       (match lhs, rhs with
        | Int a, Int b -> Int (a mod b)
        | _ -> failwith "Invalid types in rem")
     | Eq -> Bool (lhs = rhs)
     | Neq -> Bool (lhs <> rhs)
     | Lt ->
       (match lhs, rhs with
        | Int a, Int b -> Bool (a < b)
        | _ -> failwith "Invalid types in lt")
     | Gt ->
       (match lhs, rhs with
        | Int a, Int b -> Bool (a > b)
        | _ -> failwith "Invalid types in gt")
     | Lte ->
       (match lhs, rhs with
        | Int a, Int b -> Bool (a <= b)
        | _ -> failwith "Invalid types in lte")
     | Gte ->
       (match lhs, rhs with
        | Int a, Int b -> Bool (a >= b)
        | _ -> failwith "Invalid types in gte")
     | And ->
       (match lhs, rhs with
        | Bool a, Bool b -> Bool (a && b)
        | _ -> failwith "Invalid types in and")
     | Or ->
       (match lhs, rhs with
        | Bool a, Bool b ->
          let result = a || b in
          Bool result
        | _ -> failwith "Invalid types in or"))
  | Ast.Int t -> Int t
  | Ast.Str t -> Str t
  | Ast.Bool t -> Bool t
;;

let eval_string ast =
  let call_stack : Ast.term Stack.t = Stack.create () in
  let ctx : (string, Ast.term) Hashtbl.t = Hashtbl.create 100 in
  let ast_json = Yojson.Safe.from_string ast in
  let expression = ast_json |> Yojson.Safe.Util.member "expression" in
  let term = Ast.term_of_json expression in
  let _ = eval_term term ctx call_stack in
  ()
;;
