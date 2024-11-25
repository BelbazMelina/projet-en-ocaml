open Sig

module Make (N : NODE) : QUERY_STRUCTURE = struct
  type tree = N.node Sig.binary_tree
  type data = int
  type answer = N.answer

  let to_string = N.to_string

  (* Création de l’arbre *)
  let create : data list -> tree =
   fun list ->
    let make_inner_node : int -> N.node =
   
     fun s -> { answer = N.create (List.nth list s); left = s; right = s }
    in
    let getNode1 : tree -> N.node =
      fun tree -> match tree with Leaf { node } | Node { node; _ } -> node
     in
    let rec create_aux : int -> int -> tree =
     
     fun start_i end_i ->
      if start_i = end_i then
        
                    Leaf { node = make_inner_node start_i }
                  else
                    let middle_i = (start_i + end_i) / 2 in
                    let left_child = create_aux start_i middle_i in
                    let right_child = create_aux(middle_i + 1) end_i in
                    let node =
                      N.combine
                        (getNode1 left_child)
                        (getNode1 right_child)
                    in
                    Node { node; left_child; right_child }
                in
                create_aux 0 (List.length list - 1)

(* Mise a jour d'un element de la liste *)
let getnode : tree -> N.node = 
  fun tree ->
  match tree with Leaf { node } -> node | Node { node; _ } -> node
let combiner : tree -> tree -> tree =
  fun left_child right_child ->
  let left_node = getnode left_child in
     let right_node = getnode right_child in
     let node = N.combine left_node right_node in
      Node { node; left_child; right_child }
let update : tree -> data -> int -> tree =
  fun tree data i ->
    let rec updaterec : tree -> tree =
    fun tree -> 
      match tree with
      | Leaf { node = _ } -> Leaf { node = { answer = N.create data; left = i; right = i } }
      | Node { node; left_child; right_child } -> 
        let moyenne = (node.left + node.right) / 2 in
           if i <= moyenne then
               combiner(updaterec left_child) right_child
           else
               combiner left_child (updaterec right_child)
                    
              in
              updaterec tree


let rec query : tree -> int -> int -> answer =
  fun tree left right ->
    match tree with
      | Leaf { node } ->
        if node.left = left && node.right = right then node.answer
           else failwith "Invalid interval"
      | Node { node; left_child; right_child } ->
         if node.left = left && node.right = right then
            node.answer
         else
          let middle = (node.left + node.right) / 2 in
            if right <= middle then
              query left_child left right
            else if left > middle then
              query right_child left right
            else
                let query_left = query left_child left middle in
                let query_right = query right_child (middle + 1) right in
                let query_combine =
                    N.combine
                      { answer = query_left; left; right = middle }
                      { answer = query_right; left = middle + 1; right }
                         in
                         query_combine.answer
end   
 