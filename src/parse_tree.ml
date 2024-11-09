open Token

type parse_node = Leaf of token | Node of token * parse_node list

let rec display_tree_aux indent = function
  | Leaf x ->
      print_token x;
      Printf.printf "\n"
  | Node (x, children) ->
      print_token x;
      Printf.printf " -> \n";
      List.iter
        (fun element ->
          Printf.printf "%s" indent;
          display_tree_aux (indent ^ "\t") element)
        children

let display_tree tree = display_tree_aux "\t" tree
