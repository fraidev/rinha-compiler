let path = Sys.getcwd () ^ "/../../../files/"

let cases =
  [ "sum_print.json", "1\n2\n3\n4\n(3, 4)\n5\n6\n11\n"; "fib.json", "55\n"
  ; "sum.json", "15\n"; "print.json", "Hello world\n"
  ; "combination.json", "45\n"; "fib_print.json", "hey\nhey\nhey\nhey\nhey\nhey\nhey\n5\n" ]
;;

let run_files () =
  List.iter
    (fun (file, expected) ->
      let output =
        In_channel.with_open_text (path ^ file) In_channel.input_all
        |> Rinha.Evaluator.eval
      in
      Check.check_string expected !output)
    cases
;;

let suite =
  let open Alcotest in
  ( "Integration compiler tests"
  , [ test_case "Run all files in the files directory" `Quick run_files ] )
;;
