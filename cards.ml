open Core

let draw cards n =
  List.take (List.permute cards) n

let draw1 cards =
  match draw cards 1 with
    [c] -> c
  | _ -> failwith "Invariant violated: Insufficient cards."

let draw2 cards =
  match draw cards 2 with
    [c1; c2] -> (c1, c2)
  | _ -> failwith "Invariant violated: Insufficient cards."
