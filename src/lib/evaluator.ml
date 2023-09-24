let add_or_replace_hashtbl ctx key value =
  if Hashtbl.mem ctx key
  then Hashtbl.replace ctx key value
  else Hashtbl.add ctx key value
;;

type ctx = (string, Value.t) Hashtbl.t
type memoize = (Value.func * ctx, Value.t) Hashtbl.t

let rec eval_term term ctx memoize output =
  match term with
  | Ast.Let l ->
    let value = eval_term l.let_value ctx memoize output in
    (match value with
     | Value.Fn _ as fn ->
       let is_underscore =
         String.starts_with ~prefix:"_" l.name.parameter_text
       in
       (match is_underscore with
        | true -> eval_term l.next ctx memoize output
        | false ->
          let _ = add_or_replace_hashtbl ctx l.name.parameter_text fn in
          eval_term l.next ctx memoize output)
     | _ ->
       let is_underscore =
         String.starts_with ~prefix:"_" l.name.parameter_text
       in
       (match is_underscore with
        | true -> eval_term l.next ctx memoize output
        | false ->
          let _ = add_or_replace_hashtbl ctx l.name.parameter_text value in
          eval_term l.next ctx memoize output))
  | Ast.Function f ->
    let ps = List.map (fun (p : Ast.var) -> p.text) f.parameters in
    let is_pure = not (Ast.func_contains_print f) in
    Value.Fn { ctx; args = ps; value = f.value; is_pure }
  | Ast.Call c ->
    (match eval_term c.callee ctx memoize output with
     | Value.Fn fn ->
       let ctx_copy = Hashtbl.copy fn.ctx in
       let _ =
         List.combine fn.args c.arguments
         |> List.iter (function s, term ->
           add_or_replace_hashtbl
             ctx_copy
             s
             (eval_term term ctx memoize output))
       in
       (* Cache: Check if we have already evaluated this function with the same arguments *)
       if not fn.is_pure
       then eval_term fn.value ctx_copy memoize output
       else (
         match Hashtbl.find_opt memoize (fn, ctx_copy) with
         | Some v -> v
         | None ->
           let value = eval_term fn.value ctx_copy memoize output in
           let _ = add_or_replace_hashtbl memoize (fn, ctx_copy) value in
           value)
     | _ -> failwith "Can't call non-function value")
  | Ast.Var v ->
    (match Hashtbl.find_opt ctx v.text with
     | Some v -> v
     | None ->
       let err_msg = Printf.sprintf "Variable %s not found" v.text in
       failwith err_msg)
  | Ast.Tuple t ->
    (* Here I learned that OCaml execute tuple expressions from right to left *)
    let first = eval_term t.first ctx memoize output in
    let second = eval_term t.second ctx memoize output in
    Tuple (first, second)
  | Ast.First f ->
    let v = eval_term f.first_value ctx memoize output in
    (match v with
     | Tuple (a, _) -> a
     | _ -> failwith "Not a tuple in a first call")
  | Ast.Second s ->
    let v = eval_term s.second_value ctx memoize output in
    (match v with
     | Tuple (_, b) -> b
     | _ -> failwith "Not a tuple in a second call")
  | Ast.If i ->
    let value = eval_term i.condition ctx memoize output in
    if Value.to_bool value
    then eval_term i.then_term ctx memoize output
    else eval_term i.otherwise ctx memoize output
  | Ast.Print p ->
    let value = eval_term p.print_value ctx memoize output in
    let new_output = value |> Value.to_string |> Printf.sprintf "%s\n" in
    Printf.printf "%s" new_output;
    output := !output ^ new_output;
    value
  | Ast.Binary b ->
    let lhs = eval_term b.lhs ctx memoize output in
    let rhs = eval_term b.rhs ctx memoize output in
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
  | Ast.Int t -> Value.Int t
  | Ast.Str t -> Value.Str t
  | Ast.Bool t -> Value.Bool t
;;

let eval ast =
  let output = ref "" in
  let memoize : memoize = Hashtbl.create 100 in
  let ctx : ctx = Hashtbl.create 100 in
  let expression =
    ast
    |> Yojson.Safe.from_string
    |> Yojson.Safe.Util.member "expression"
    |> Ast.term_of_json
  in
  let _ = eval_term expression ctx memoize output in
  output
;;
