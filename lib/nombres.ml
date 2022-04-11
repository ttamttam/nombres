type options =
  | Septante
  | Huitante
  | Nonante
  | Belgique (* Septante Nonante *)
  | VVF (* Septante Huitante Nonante *)

(* let int_of_options = function *)
(*   | Septante -> 0x1 *)
(*   | Huitante -> 0x2 *)
(*   | Nonante -> 0x8 *)
(*   | Belgique -> 0x1 lor 0x8 *)
(*   | VVF -> 0x1 lor 0x2 lor 0x8 *)

(* type mode = [ `Belgique | `VVF | `France ] *)

(* let options_of_mode = function *)
(*   | `Belgique -> [ Belgique ] *)
(*   | `VVF -> [ VVF ] *)
(*   | `France -> [] *)

(* let int_of_mode m = *)
(*   m |> options_of_mode |> List.map int_of_options *)
(*   |> ListLabels.fold_left ~f:( + ) ~init:0 *)

(* let mode_of_int = function *)
(*   | 0 -> `France *)
(*   | 1 -> `Belgique *)
(*   | 2 -> `VVF *)
(*   | _ -> assert false *)

let opts options lst = List.exists (fun opt -> List.mem opt options) lst

let nombre ?(options = []) n =
  let has_opts = opts options in
  if n = min_int then invalid_arg "Nombre hors domaine"
  else
    (* if debug then Fmt.pr "@[<v 2>nombre %d:@," n; *)
    let rec schu =
      [|
        "";
        "un";
        "deux";
        "trois";
        "quatre";
        "cinq";
        "six";
        "sept";
        "huit";
        "neuf";
        "dix";
        "onze";
        "douze";
        "treize";
        "quatorze";
        "quinze";
        "seize";
      |]
    and schd =
      [|
        "";
        "dix";
        "vingt";
        "trente";
        "quarante";
        "cinquante";
        "soixante";
        "soixante";
      |]
    and nombre_of_int ?(singular = false) ?(accu = []) current () =
      match current with
      | 0 ->
          if accu <> [] then accu |> List.rev |> String.concat " " else "zéro"
      | n when n < 0 -> nombre_of_int ~accu (-n) ()
      | n when n < 17 -> unroll (schu.(n) :: accu)
      | n when n mod 10 = 0 && n < 61 -> unroll (schd.(n / 10) :: accu)
      | 80 ->
          let nom, noms =
            if has_opts [ Huitante; VVF ] then ("huitante", "huitante")
            else ("quatre-vingt", "quatre-vingts")
          in
          unroll ((if singular then nom else noms) :: accu)
      | 100 -> unroll ("cent" :: accu)
      | 1000 -> unroll ("mille" :: accu)
      | n when n < 70 ->
          let dizaines = nombre_of_int (n / 10 * 10) () in
          let unites = n mod 10 in
          let accu =
            ([
               dizaines;
               (if unites = 1 then " et " else "-");
               nombre_of_int unites ();
             ]
            |> String.concat "")
            :: accu
          in
          unroll accu
      | n when n < 80 ->
          let unites = n mod 10 in
          let accu =
            if has_opts [ Septante; Belgique; VVF ] then
              ([
                 (if n < 70 then "soixante" else "septante");
                 (if unites = 1 && n < 80 then " et "
                 else if unites > 0 then "-"
                 else "");
                 (if unites > 0 then nombre_of_int unites () else "");
               ]
              |> String.concat "")
              :: accu
            else
              ([
                 "soixante";
                 (if unites = 1 && n < 80 then " et " else "-");
                 nombre_of_int (n mod 20) ();
               ]
              |> String.concat "")
              :: accu
          in
          unroll accu
      | n when n < 100 ->
          let unites = n mod 10 in
          let accu =
            if has_opts [ Huitante; VVF ] && n < 90 then
              ([
                 "huitante";
                 (if unites = 1 then " et " else if unites > 0 then "-" else "");
                 (if unites > 0 then nombre_of_int unites () else "");
               ]
              |> String.concat "")
              :: accu
            else if has_opts [ Nonante; VVF; Belgique ] && n >= 90 then
              ([
                 "nonante";
                 (if unites = 1 then " et " else if unites > 0 then "-" else "");
                 (if unites > 0 then nombre_of_int unites () else "");
               ]
              |> String.concat "")
              :: accu
            else
              ([
                 "quatre-vingt";
                 (if unites = 1 && n < 80 then " et " else "-");
                 nombre_of_int (n mod 20) ();
               ]
              |> String.concat "")
              :: accu
          in
          unroll accu
      | n when n < 1_000 -> jusqua_x ~singular accu 100 ~nom:"cent" ~un:false n
      | n when n < 1_000_000 -> jusqua_x accu 1000 ~nom:"mille" ~un:false n
      | n when n < 1_000_000_000 ->
          jusqua_x accu 1_000_000 ~nom:"million" ~un:true n
      | n when n < 1_000_000_000_000 ->
          jusqua_x accu 1_000_000_000 ~nom:"milliard" ~un:true n
      | n when n < 1_000_000_000_000_000 ->
          jusqua_x accu 1_000_000_000_000 ~nom:"billion" ~un:true n
      | n when n < 1_000_000_000_000_000_000 ->
          jusqua_x accu 1_000_000_000_000_000 ~nom:"billiard" ~un:true n
      | n when n <= max_int ->
          jusqua_x accu 1_000_000_000_000_000_000 ~nom:"trillion" ~un:true n
      | _ -> assert false
    and unroll accu = List.rev accu |> String.concat " "
    and jusqua_x ?(singular = false) accu lim ~nom ~un n =
      let un = if un then "un" else "" in
      let noms =
        match (nom, singular) with
        | "mille", _ -> "mille"
        | nom, true -> nom
        | nom, false -> nom ^ "s"
      in
      let count = n / lim and reste = n mod lim in

      let accu =
        match count with
        | 1 -> nom :: un :: accu |> List.filter (( <> ) "")
        | _ ->
            let noms = if lim = 100 && reste <> 0 then nom else noms in
            noms
            :: nombre_of_int ~singular:(singular || lim = 1000) count ()
            :: accu
      in

      nombre_of_int ~singular ~accu reste ()
    in

    nombre_of_int n ()

(*$= nombre & ~printer:(fun x -> "\"" ^ x ^ "\"")
     "zéro"                                    (nombre 0)
     "un"                                      (nombre 1)
     "deux"                                    (nombre 2)
     "trois"                                   (nombre 3)
     "trente et un"                            (nombre 31)
     "trente-deux"                             (nombre 32)
     "soixante-dix"                            (nombre 70)
     "quatre-vingts"                           (nombre 80)
     "quatre-vingt-un"                         (nombre 81)
     "quatre-vingt-trois"                      (nombre 83)
     "quatre-vingt-dix"                        (nombre 90)
     "quatre-vingt-onze"                       (nombre 91)
     "cent"                                    (nombre 100)
     "quatre cents"                            (nombre 400)
     "quatre cent vingt et un"                 (nombre 421)
     "mille"                                   (nombre 1000)
     "mille cent"                              (nombre 1100)
     "mille deux cents"                        (nombre 1200)
     "mille deux cent trente"                  (nombre (-1230))
     "mille deux cent trente"                  (nombre 1230)
     "quatre-vingt mille"                      (nombre 80_000)
     "deux cent mille"                         (nombre 200_000)
     "deux cent mille trois cent quarante"     (nombre 200_340)
     "quatre-vingts millions"                  (nombre 80_000_000)
     "quatre-vingts millions un"               (nombre 80_000_001)
     "deux cents millions"                     (nombre 200_000_000)
     "deux cents millions trois cent quarante" (nombre 200_000_340)
     "deux cents milliards"                    (nombre 200_000_000_000)
     "deux cents billiards"                    (nombre 200_000_000_000_000_000)
     "deux trillions"                          (nombre 2_000_000_000_000_000_000)
     "deux trillions un"                       (nombre 2_000_000_000_000_000_001)
     "deux trillions quatre cent mille un"     (nombre 2_000_000_000_000_400_001)
     "deux trillions quatre cents millions un" (nombre 2_000_000_000_400_000_001)
     "deux trillions cent billions quatre cents millions un" (nombre 2_000_100_000_400_000_001)
*)
