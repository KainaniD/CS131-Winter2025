
(* question 1 - convert_grammar gram *)

let convert_grammar gram1 =
 (fst gram1),
 (let filter rule_list nonterminal =
     (let filtered_rules = List.filter (fun rule -> (fst rule) = nonterminal) rule_list
     in List.map (fun (nt, rule) -> rule) filtered_rules)
     in filter (snd gram1));;



(* question 2 - parse_tree_leaves tree *)

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal;;

let parse_tree_leaves tree =
(let rec accumulator a node = match node with
     | Leaf t -> a @ [t]
     | Node (_, array) -> a @ (List.flatten (List.map (accumulator a) array))
     in accumulator [] tree);;



(* question 3 - make_matcher gram *)

(* match first element with rule and use parser to match consecutive elements *)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let rec choose_rule gram rule_list frag acceptor = match rule_list with
| [] -> None
| h::t ->
    (let rec match_rule rule frag acceptor = match rule, frag with
    | [], _ -> acceptor frag
    | _, [] -> None
    | (T s)::rt, fh::ft ->
        if s = fh then match_rule rt ft acceptor else None
    | (N s)::rt, _ ->
        choose_rule gram ((snd gram) s) frag (fun new_frag -> match_rule rt new_frag acceptor)
        in
        match match_rule h frag acceptor with
        | None -> choose_rule gram t frag acceptor
        | result -> result );;

let make_matcher gram accept frag = choose_rule gram ((snd gram) (fst gram)) frag accept;;

(* question 4 - make_parser gram *)

let parse_tree_acceptor frag tree =
  match frag with
  | [] -> Some tree
  | _ -> None;;

let rec choose_rule_parser gram rule_list frag acceptor = match rule_list with
| [] -> None
| h::t ->
    (let rec match_rule_parser rule frag acceptor = match rule, frag with
    | [], _ -> acceptor frag []
    | _, [] -> None
    | (T s)::rt, fh::ft ->
        if s = fh then match_rule_parser rt ft (fun new_frag subtree -> acceptor new_frag (Leaf s :: subtree))
        else None
    | (N s)::rt, _ ->
    choose_rule_parser gram ((snd gram) s) frag (fun new_frag child_tree -> match_rule_parser rt new_frag (fun final_frag subtree -> acceptor final_frag (Node (s, child_tree)::subtree)))
        in
        match match_rule_parser h frag (fun new_frag tree -> acceptor new_frag tree) with
        | None -> choose_rule_parser gram t frag acceptor
        | result -> result );;

let make_parser gram frag =
  match choose_rule_parser gram ((snd gram) (fst gram)) frag parse_tree_acceptor with
  | Some tree -> Some (Node ((fst gram), tree))  
  | None -> None;; 
