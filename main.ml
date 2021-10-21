open Ast
open Microkanren
open Printstream
open Examples

let examples = [
  a_and_b;
  fives;
  fives_sixes;
  find_list_x;
  find_list_y;
  find_list_xy;
  find_tree_x1;
  find_tree_x2;
  find_tree_y;
  find_tree_xy;
  find_tree_complex;
  find_2trees
]

let empty_state = [], 0

let () = List.iter (fun e -> print_stream (e empty_state) 9) examples
