open Core

type view = {
    card : Role.t
  ; player_or_center : string
  }

type vote = Center
          | Player of string

type t = View of view
       | ViewLoneMason
       | ViewNoWerewolves
       | Swap of string * string
       | Ready
       | VoteReady
       | Vote of vote

let selfify me card third_person =
  if card = me
  then (if third_person
        then "themselves"
        else "yourself")
  else card

let article = function
    Role.Werewolf -> "a"
  | Role.DreamWolf -> "a"
  | Role.Tanner -> "a"
  | Role.Minion -> "a"
  | Role.Seer -> "the"
  | Role.Robber -> "the"
  | Role.Villager -> "a"
  | Role.Troublemaker -> "the"
  | Role.Mason -> "a"
  | Role.Insomniac -> "an"
  | Role.Unassigned -> failwith "No role assigned before printing?"

let to_string ?(third_person=false) action ~me =
  match action with
    View {card; player_or_center} ->
     (match player_or_center with
        "center" -> sprintf "Viewed a %s card in the center." (Role.to_string card)
      | player ->
         if player = me
         then (if third_person
               then sprintf "Viewed that they were %s %s." (article card) (Role.to_string card)
               else sprintf "Viewed that you were %s %s." (article card) (Role.to_string card))
         else sprintf "Viewed that %s was %s %s." player (article card) (Role.to_string card))
  | Swap (card1, card2) -> sprintf "Swapped %s with %s"
                             (selfify me card1 third_person)
                             (selfify me card2 third_person)
  | Ready -> "Ready"
  | ViewLoneMason -> "Viewed that you were the lone mason."
  | ViewNoWerewolves -> "Viewed that there were no werewolves."
  | VoteReady -> "Is ready to vote."
  | Vote Center -> "Voted that there was no werewolf."
  | Vote (Player name) -> sprintf "Voted for %s." (selfify me name third_person)
