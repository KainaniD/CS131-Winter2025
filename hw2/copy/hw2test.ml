
(* test grammar *)

let accept_correct = function
| [] -> Some []
| _ -> None;;

type test_grammar_nonterminals = | Dp | Dpb | Determiner | Noun | Adjective;;

let test_grammar = (Dp,
    function
    | Dp -> [[N Determiner; N Noun]; [N Dp; N Dpb]]
    | Dpb -> [[T "and"; N Dp]]
    | Determiner -> [[T "the"]; [T "a"]; [T "that"]; [T "this"]]
    | Noun -> [[N Adjective; N Noun]; [T "quokka"]; [T "koala"]; [T "giraffe"]]
    | Adjective -> [[T "blue"]; [T "brown"]; [T "green"]; [T "red"]]);;

(* make_matcher_test *)

let make_matcher_test = make_matcher test_grammar accept_correct ["the"; "blue"; "brown"; "green"; "quokka"; "and"; "a"; "green"; "koala"] = Some [];;

(* make_parser_test *)

let make_parser_test = match make_parser test_grammar ["a"; "blue"; "brown"; "green"; "giraffe"; "and"; "that"; "green"; "red"; "giraffe"; "and"; "this"; "brown"; "quokka"] with
| Some tree -> parse_tree_leaves tree = ["a"; "blue"; "brown"; "green"; "giraffe"; "and"; "that"; "green"; "red"; "giraffe"; "and"; "this"; "brown"; "quokka"]
| _ -> false;;
