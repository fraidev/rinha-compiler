let _ =
  In_channel.with_open_text Sys.argv.(1) In_channel.input_all
  |> Rinha.Evaluator.eval
;;
