let my_subset_test0 = subset [] []
let my_subset_test1 = subset [3;3;3;3;3;3;3] [3]
let my_subset_test2 = not (subset [1;2;3] [1;2])

let my_equal_sets_test0 = equal_sets [5;5;5;5;5] [5]
let my_equal_sets_test1 = not (equal_sets [] [1])
let my_equal_sets_test2 = equal_sets [] []

let my_set_union_test0 = equal_sets (set_union [1] [1]) [1]
let my_set_union_test1 = equal_sets (set_union [7;8;9;0] []) [0;9;7;8]
let my_set_union_test2 = equal_sets (set_union [1;2] [3;4]) [1;2;3;4]

let my_set_all_union_test0 =
  equal_sets (set_all_union [[];[];[];[]]) []
let my_set_all_union_test1 =
  equal_sets (set_all_union [[3;2]; []; [3]]) [2;3]
let my_set_all_union_test2 =
  equal_sets (set_all_union [[3;3;3]; [4;4;4]; [5;5;5]; [1;2;3;4;5;6]]) [6;5;4;3;2;1]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x * 1) 20 = 20
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x /. 1.) 2. = 2.
let my_computed_fixed_point_test2 =
  computed_fixed_point (<) sqrt 10. = 10.


let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x * 0) 0 (-123401) = -123401
let my_computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> x *. 10.) 2 1. = infinity

let my_whileseq_test0 =
  whileseq ((+) 2) ((>) 20) 3 = [3;5;7;9;11;13;15;17;19]

type awksub_nonterminals =
  | Noun | Verb | Sentence | Determiner | Aux | Adjective

let fake_english_rules =
   [
    Adjective, [N Adjective];
    Noun, [N Adjective; N Noun];
    Sentence, [N Noun; N Verb];
    Sentence, [N Noun; N Aux; N Verb];
    Noun, [N Determiner; N Noun];
    Determiner, [T"The"];
    Aux, [T"Will"];
    Aux, [T"Is"];
    Noun, [T"Dog"];
    Noun, [T"Cat"];
    Noun, [T"Computer"];
    Noun, [T"Class"];
    Noun, [T"Egg"];
    Verb, [T"Teaching"];
    Verb, [T"Learn"];
    Verb, [T"Coded"];
    Verb, [T"Ran"];
    Verb, [T"Sit"]]

let fake_english_grammar = Sentence, fake_english_rules

let my_filter_blind_alleys_test0 =
  not(filter_blind_alleys fake_english_grammar = fake_english_grammar)

let my_filter_blind_alleys_test1 =
  filter_blind_alleys (Noun, List.tl (List.tl fake_english_rules)) = (Noun, List.tl (List.tl fake_english_rules))

let my_filter_blind_alleys_test2 =
  filter_blind_alleys (Sentence,
      [
    Sentence, [N Noun; N Verb];
    Sentence, [N Noun; N Aux; N Verb];
    Adjective, [N Adjective];
    Noun, [N Adjective; N Noun];
    Noun, [N Determiner; N Noun];
    Determiner, [T"The"];
    Aux, [T"Will"];
    Aux, [T"Is"];
    Noun, [T"Dog"];
    Noun, [T"Cat"];
    Noun, [T"Computer"];
    Noun, [T"Class"];
    Noun, [T"Egg"];
    Verb, [T"Teaching"];
    Verb, [T"Learn"];
    Verb, [T"Coded"];
    Verb, [T"Ran"];
    Verb, [T"Sit"]])
  = (Sentence,
     [
    Sentence, [N Noun; N Verb];
    Sentence, [N Noun; N Aux; N Verb];
    Noun, [N Determiner; N Noun];
    Determiner, [T"The"];
    Aux, [T"Will"];
    Aux, [T"Is"];
    Noun, [T"Dog"];
    Noun, [T"Cat"];
    Noun, [T"Computer"];
    Noun, [T"Class"];
    Noun, [T"Egg"];
    Verb, [T"Teaching"];
    Verb, [T"Learn"];
    Verb, [T"Coded"];
    Verb, [T"Ran"];
    Verb, [T"Sit"]]);;
