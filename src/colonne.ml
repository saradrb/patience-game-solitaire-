open Card
open Fifo

type colonne= {capacity : int; size : int; cards: card Fifo.t}


let init capacity=
  let cards= Fifo.empty 
  in (capacity,0,cards)
;;

let add card c = match c with 
  |(capacity,size,cards) -> 
    if (size < capacity) then let cards= Fifo.push card cards in (capacity,size+1,cards)
    else failwith "colonne is full"
;;

let remove card c = match c with 
  |(capacity,size,cards)->let card= Fifo.pop cards in (capacity,size-1,cards)
  |_ -> failwith "this is not a colonne"
;;

let isFull c = 
  if (capacity == size) then true 
  else false 
;;

let isEmpty c = 
  if (size==0) then true 
  else false 
;;