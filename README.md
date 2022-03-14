# Ocaml playground

My little OCaml projects. IMHO OCaml is great for manipulating recursive data
structures, thus I'd like to get the hang of writing small programs fast.

## regex.ml
DFA to regex converter.
TODO:
- Reduce regexes that are concatenated with the empty regex
- Expand the string_of_regex function, for example omit parenthesis around 
  concatenated letters
- Make some automaton parser