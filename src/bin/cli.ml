open Rinha

let () =
  (* Check if entrypoint path is given *)
  let entrypoint =
    match Array.length Sys.argv with
    | 0 | 1 -> failwith "No entrypoint path given"
    | _ -> Sys.argv.(1)
  in
  (* Check if file exists *)
  let () =
    if not (Sys.file_exists entrypoint)
    then failwith "Entrypoint file does not exist"
  in
  (* Read file *)
  let entrypoint_txt = Utils.read_lines entrypoint in
  (* print_string(entrypoint_txt); *)
  let _return = Evaluator.eval_string entrypoint_txt in
  ()
;;
