open Core

type view = {
    card : Role.t
  ; player_or_center : string
  }

type t = View of view
       | Swap of string * string
       | Ready

let selfify me card =
  if card = me then "yourself" else card

let to_string action ~me =
  match action with
    View {card; player_or_center} ->
     (match player_or_center with
        "center" -> sprintf "Viewed a %s card in the center." (Role.to_string card)
      | player ->
         if player = me
         then sprintf "Viewed that you were the %s." (Role.to_string card)
         else sprintf "Viewed that %s was the %s." player (Role.to_string card))
  | Swap (card1, card2) -> sprintf "Swapped %s with %s" (selfify me card1) (selfify me card2)
  | Ready -> sprintf "Ready"
