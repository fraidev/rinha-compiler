type return_values =
  | Int of int32
  | String of string
  | Bool of bool
  | Tuple of return_values list
  | Nil

let eval_string ast =
  let ast_json = Yojson.Safe.from_string ast in
  let expression = ast_json |> Yojson.Safe.Util.member "expression" in
  let term = Ast.term_of_json expression in
  (* let () = Format.printf "Parsed to %a" Yojson.Safe.pp ast_json in *)
  let b =
    match term with
    | Ast.Print p ->
      let value = p.print_value in
      (match value with
       | Ast.Str s ->
         let () = Format.printf "%s" s in
         Nil
       | _ -> failwith "Not a string")
    | _ -> failwith "Not a term"
  in
  b
;;
