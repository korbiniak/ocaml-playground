# Ocaml playground

My little OCaml projects. IMHO OCaml is great for manipulating recursive data
structures, thus I'd like to get the hang of writing small programs fast.

## regex.ml
DFA to regex converter.
TODO:
- Reduce regexes that are concatenated with the empty regex
  - Try adding more simple rules for reducing, for example `(e+a)((a*)(b)) == (a*b)`
- Expand the string_of_regex function, for example omit parenthesis around 
  concatenated letters (not as easy as I thought)
- Make some automaton parser

It created a not-so-long regex such that some online regex-to-dfa converter has successfully converted to DFA. Regex can be found in [results](/results).