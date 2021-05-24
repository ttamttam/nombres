let rec nombre_of_int = function
  | 0 -> "zéro"
  | 1 -> "un"
  | 2 -> "deux"
  | 3 -> "trois"
  | 4 -> "quatre"
  | 5 -> "cinq"
  | 6 -> "six"
  | 7 -> "sept"
  | 8 -> "huit"
  | 9 -> "neuf"
  | 10 -> "dix"
  | 11 -> "onze"
  | 12 -> "douze"
  | 13 -> "treize"
  | 14 -> "quatorze"
  | 15 -> "quinze"
  | 16 -> "seize"
  | 20 -> "vingt"
  | 30 -> "trente"
  | 40 -> "quarante"
  | 50 -> "cinquante"
  | 60 -> "soixante"
  | 80 -> "quatre-vingts"
  | 100 -> "cent"
  | 1000 -> "mille"
  | n when n < 70 -> jusqua_70 n
  | n when n < 80 -> jusqua_100 "soixante" n
  | n when n < 100 -> jusqua_100 "quatre-vingt" n
  | n when n < 1_000 -> jusqua_1000 n
  | n when n < 1_000_000 -> jusqua_1000000 n
  | _ -> "--------------------"

and jusqua_70 n =
  let dizaine = nombre_of_int (n / 10 * 10) in
  let unite = n mod 10 in
  [ dizaine; nombre_of_int unite ]
  |> String.concat (if unite = 1 then " et " else "-")

and jusqua_100 dizaine n =
  let unite = n mod 10 in
  [ dizaine; nombre_of_int (n mod 20) ]
  |> String.concat (if unite = 1 && n < 80 then " et " else "-")

and jusqua_1000 n =
  let centaine = n / 100 and reste = n mod 100 in
  let cent = if centaine > 1 && reste = 0 then "cents" else "cent" in
  ((if centaine = 1 then [ cent ] else [ nombre_of_int centaine; cent ])
  @ if reste = 0 then [] else [ nombre_of_int reste ])
  |> String.concat " "

and jusqua_1000000 n =
  let milliers = n / 1000 and reste = n mod 1000 in
  ((if milliers = 1 then [ "mille" ] else [ nombre_of_int milliers; "mille" ])
  @ if reste = 0 then [] else [ nombre_of_int reste ])
  |> String.concat " "
