

(* Question 1 - subset *)

let rec element_in = fun e -> function
| [] -> false
| h::t -> if e = h
          then true
          else element_in e t;;

let rec subset = fun a -> fun b -> match a with
| [] -> true
| h::t -> if element_in h b
          then subset t b
          else false;;



(* Question 2 - equal_sets *)

let equal_sets a b = subset b a && subset a b;;




(* Question 3 - set_union *)

let rec set_union a b = a @ b;;




(* Question 4 - set_all_union *)

let set_all_union a =
     (let rec accumulate = fun b -> function
     |[] -> b
     |h::t -> accumulate (set_union b h) t
     in accumulate [] a);;




(* Question 5 - Russell's Paradox *)

(* This is impossible to check for in a program because we would have an infinite amount of recursive calls if the set contains itself because in that case, it needs to contain itself an infinite number of times. *)



(* Question 6 - computed_fixed_point *)

let computed_fixed_point eq f x =
     (let rec approximate = fun eq -> fun f -> fun x -> function
     |a -> if (eq) a x
           then x else approximate eq f a (f a)
           in approximate eq f x (f x));;



(* Question 7 - computed_periodic_point *)

let rec apply_function_p_times = fun f -> fun p -> fun x -> match p with
| 0 -> x
| _ -> apply_function_p_times f (p-1) (f x);;

let rec computed_periodic_point eq f p x =
     (let next = apply_function_p_times f p x
     in
           if (eq) next x
           then x
           else computed_periodic_point eq f p (f x));;



(* Question 8 - whileseq *)

let whileseq s p x =
     (let rec accumulate = fun a -> fun s -> fun p -> fun x ->
      if (p x)
      then accumulate (a @ [x]) s p (s x)
      else a
      in accumulate [] s p x);;


(* Question 9 - filter_blind_alleys *)

type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal;;

let is_terminal = fun terminals -> function
| T _ -> true
| N s -> element_in s terminals;;

(* checks if the rules of a non-terminal symbol only have terminal elements*)
let rec check_rules_terminal = fun rules -> fun terminals -> match rules with
| [] -> true
| h::t -> if is_terminal terminals h
          then check_rules_terminal t terminals
          else false;;

(* adds all the non-terminal symbols which only have terminal elements to the terminal list *)
let rec add_currently_terminal = fun grammar -> fun terminals -> match grammar with
| [] -> terminals
| (f, s)::t -> if check_rules_terminal s terminals
               then add_currently_terminal t (f::terminals)
               else add_currently_terminal t terminals;;

(* fit form to allow use of computed_fixed_point *)
let add_currently_terminal_one_arg (grammar, terminal) = (grammar, add_currently_terminal grammar terminal);;

(* required with new form *)
let equal_sets_second (_, a) (_ ,b) = equal_sets a b;;

(* filters out all non-terminal symbols *)
let rec filter_grammar = fun a -> fun rules -> fun terminals -> match rules with
| [] -> a
| (f, s)::t -> if (check_rules_terminal s terminals)
               then filter_grammar (a@[f,s]) t terminals
               else filter_grammar a t terminals;;

let all_terminal_list g = (snd(computed_fixed_point equal_sets_second add_currently_terminal_one_arg ((snd g), [])));;

let filter_blind_alleys g = ((fst g),(filter_grammar [] (snd g) (all_terminal_list g)));;
