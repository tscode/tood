open Types

type 'a t = {
    name   : symbol
  ; leaves : 'a list
  ; nodes  : 'a t list
}

type path = Symbol.t list

let create ?(leaves=[]) ?(nodes=[]) name = {name; leaves; nodes}

let name tree   = tree.name
let leaves tree = tree.leaves
let nodes tree  = tree.nodes

let has_name name node = (node.name = name)

let rec add_leaf ?(path=[]) t leaf = match path with
  | [] -> { t with leaves = leaf :: t.leaves }
  | h :: tl ->
    match List.exists (has_name h) t.nodes with
    | false -> { t with nodes = add_leaf ~path:tl (create h) leaf :: t.nodes }
    | true  -> 
      let f node = match has_name h node with
        | true  -> add_leaf ~path:tl node leaf
        | false -> node
      in
      { t with nodes = List.map f t.nodes }

let of_leaves ?(name="") ~paths leaves =
  let f t leaf = List.fold_left
    (fun tree path -> add_leaf ~path tree leaf) t (paths leaf)
  in
  List.fold_left f (create name) (List.rev leaves)

let rec to_leaves t = t.leaves @ List.concat_map to_leaves t.nodes

let rec map f tree = { tree with
    leaves = List.map f tree.leaves
  ; nodes  = List.map (map f) tree.nodes
}

let rec collect ?(path=[]) ?fname f tree =
  let collection =
    let path = path @ [tree.name] in
    let cleaves = List.map (f path) tree.leaves in
    cleaves @ List.concat_map (collect ~path ?fname f) tree.nodes
  in
  match fname with
  | None -> collection
  | Some f -> f path tree.name :: collection

