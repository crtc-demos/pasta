#!/bin/sh
set -e
OCAMLC=ocamlopt
SUF=cmx
rm -f parser.ml parser.mli lexer.ml lexer.mli lexer.cm? parser.cm?
menhir --table --infer parser.mly
$OCAMLC -c parser.mli
$OCAMLC -c parser.ml
ocamllex lexer.mll
$OCAMLC -c lexer.ml
$OCAMLC -c timesup.ml
$OCAMLC lexer.$SUF parser.$SUF timesup.$SUF -o timesup
