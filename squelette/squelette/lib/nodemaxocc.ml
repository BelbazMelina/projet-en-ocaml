type data = int
type answer = int * int 
type node = { answer : answer; left : int; right : int }

let create : data -> answer = fun data -> (data, 1)

let combine : node -> node -> node =
 fun left right ->
  let answer1= if fst left.answer > fst right.answer  
    then left.answer
  else if fst left.answer < fst right.answer 
    then right.answer
  else (fst left.answer, snd left.answer + snd right.answer) in
  {answer =answer1;left = min left.left right.left;right = max left.right right.right;}

let to_string : answer -> string =
 fun answer ->
  "(" ^ string_of_int (fst answer) ^ ", " ^ string_of_int (snd answer) ^ ")"
