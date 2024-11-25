type data = int
type answer = { sum : int; prefix : int; suffix : int; subseg : int }
type node = { answer : answer; left : int; right : int }


let create : data -> answer =
 fun data -> { sum = data; prefix = data; suffix = data; subseg = data }


 let combine : node -> node -> node =
 fun left right ->
  let max' =fun a1 a2 a3 ->  max a1 (max a2 a3) in
  let sum1= left.answer.sum + right.answer.sum in 
  let prefix1=max left.answer.sum (left.answer.sum + right.answer.prefix) in
  let suffix1=max right.answer.sum(right.answer.sum + left.answer.suffix) in
  let subgeg1=  max' left.answer.subseg right.answer.subseg
                     (left.answer.suffix + right.answer.prefix) in
  {
    answer = {sum = sum1;prefix = prefix1;suffix =suffix1;subseg = subgeg1;};
    left = min left.left right.left;
    right = max left.right right.right;
  } 

let to_string : answer -> string =
 fun answer ->
  let max' = fun a1 a2 a3 a4 -> max a1 (max a2 (max a3 a4))in 
  string_of_int (max' answer.sum answer.prefix answer.suffix answer.subseg)