type data = int
type answer = int
type node = { answer : answer; left : int; right : int }

let create : data -> answer = fun data -> data 

let combine : node -> node -> node =
 fun left right ->
  {
    answer = left.answer + right.answer;
    left = min left.left right.left;
    right = max left.right right.right;
  }

let to_string : answer -> string =  
 fun answer -> string_of_int answer  