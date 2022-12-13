open Card
open Fifo
type colonne={capacity:int;size:int;cards :Fifo of Card}



let init capacity=
  let cards= Fifo.empty 
  in (capacity,0,cards)
;;

let add card c = match c with 
  |(capacity,size,cards) -> 
    if (size < capacity) then let cards= Card.push card cards in (capacity,size+1,cards)
    else raise "colonne is full"
;;

let remove card c = match c with 
  |(capacity,size,cards)->let card= Card.pop card cards in (capasity,size-1,cards)
  |failwith "this is not a colonne"
;;

let isFull c = 
  if (capacity == size) then true 
  else false 
;;

let isEmpty c = 
  if (size==0) then true 
  else false 
;;