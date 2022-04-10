(lang dune 3.0)
(name nombres)
(version 0.9)

(generate_opam_files true)

(source (github ttamttam/nombres))
(license ISC)
(authors "Matthieu Dubuget")
(maintainers "matthieu.dubuget@gmail.com")

(package
 (name nombres)
  (synopsis "Conversion of integers to french.")
  (description
"\| Convert integers into french.
"\|
"\| Some references:
"\| - https://leconjugueur.lefigaro.fr/frlesnombres.php
"\| - https://www.miakinen.net/vrac/nombres
"\| - https://www.dcode.fr/ecriture-nombre-lettres
))
