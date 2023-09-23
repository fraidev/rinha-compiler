let add_or_replace_hashtbl ctx key value =
  if Hashtbl.mem ctx key
  then Hashtbl.replace ctx key value
  else Hashtbl.add ctx key value
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
          Stack.push (evaluated_arg |> Value.to_term) stack)
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
    if Value.to_bool value
    then eval_term i.then_term ctx call_stack cache
    else eval_term i.otherwise ctx call_stack cache
  | Ast.Print p ->
    let value = eval_term p.print_value ctx call_stack cache in
    value |> Value.to_string |> print_endline;
    value
  | Ast.Binary b ->
    let lhs = eval_term b.lhs ctx call_stack cache in
    let rhs = eval_term b.rhs ctx call_stack cache in
    (match b.op, lhs, rhs with
     | Add, Int a, Int b -> Int (Int32.add a b)
     | Add, Str a, Str b -> Str (a ^ b)
     | Add, Int a, Str b -> Str (Int32.to_string a ^ b)
     | Add, Str a, Int b -> Str (a ^ Int32.to_string b)
     | Sub, Int a, Int b -> Int (Int32.sub a b)
     | Mul, Int a, Int b -> Int (Int32.mul a b)
     | Div, Int a, Int b -> Int (Int32.div a b)
     | Rem, Int a, Int b -> Int (Int32.rem a b)
     | Eq, Int a, Int b -> Bool (Int32.equal a b)
     | Eq, Bool a, Bool b -> Bool (Bool.equal a b)
     | Eq, Str a, Str b -> Bool (String.equal a b)
     | Neq, Int a, Int b -> Bool (not (Int32.equal a b))
     | Neq, Bool a, Bool b -> Bool (not (Bool.equal a b))
     | Neq, Str a, Str b -> Bool (not (String.equal a b))
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
  let cache : (Ast.func * (string, Ast.term) Hashtbl.t, Value.t) Hashtbl.t
    =
    Hashtbl.create 100
  in
  let call_stack : Ast.term Stack.t = Stack.create () in
  let ctx : (string, Ast.term) Hashtbl.t = Hashtbl.create 100 in
  let expression =
    ast
    |> Yojson.Safe.from_string
    |> Yojson.Safe.Util.member "expression"
    |> Ast.term_of_json
  in
  let _ = eval_term expression ctx call_stack cache in
  ()
;;
