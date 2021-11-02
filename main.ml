open Ast
open Microkanren
open Printstream
open Examples

let examples = [
  "a_and_b",a_and_b;
  "fives",fives;
  "fives_sixes",fives_sixes;
  "find_list_x",find_list_x;
  "find_list_y",find_list_y;
  "find_list_xy",find_list_xy;
  "find_tree_x1",find_tree_x1;
  "find_tree_x2",find_tree_x2;
  "find_tree_y",find_tree_y;
  "find_tree_xy",find_tree_xy;
  "find_tree_complex",find_tree_complex;
  "find_2trees",find_2trees;
]

let empty_state = [], 0

let () = List.iter (fun (name,f) -> print_stream name (f empty_state) 9) examples
