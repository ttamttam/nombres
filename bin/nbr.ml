let rec parse_and_print () =
  print_string "Entier ? > ";
  flush stdout;
  input_line stdin |> function
  | "q" | "Q" -> ()
  | "h" | "?" | "H" ->
      print_endline "Je n'accepte que des entiers. 'q' ou 'Q' pour quitter.";
      flush stdout;
      parse_and_print ()
  | str ->
      str |> int_of_string
      |> (fun i ->
           match i with
           | i -> i |> Nombres.nombre_of_int |> print_endline
           | exception _ -> ())
      |> parse_and_print

let () = parse_and_print ()
