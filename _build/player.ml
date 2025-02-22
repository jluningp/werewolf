open Core

type t = {
    name : string
  ; id : string
  ; evening_role : Role.t
  ; morning_role : Role.t
  ; log : Action.t list
  } [@@deriving fields]

let gen_code n =
  let a = int_of_char 'A' in
  List.init n ~f:(fun _ -> char_of_int (a + Random.int 26))
  |> String.of_char_list

let create name =
  {name
  ; id=gen_code 8
  ; evening_role=Role.Unassigned
  ; morning_role=Role.Unassigned
  ; log=[]}

let is_ready player =
  List.exists player.log ~f:(function
        Action.Ready -> true
      | _ -> false)

let is_ready_for_second_action player =
  List.exists player.log ~f:(function
        Action.ReadyForSecondAction -> true
      | _ -> false)

let is_vote_ready player =
  List.exists player.log ~f:(function
        Action.VoteReady -> true
      | _ -> false)

let vote player =
  List.find_map player.log ~f:(function
        Action.Vote vote -> Some vote
      | _ -> None)

let views player =
  List.filter_map player.log ~f:(function
        Action.View view -> Some view
      | Action.ViewLoneMason -> Some {Action.card=Role.Mason
                                    ; Action.player_or_center=player.name}
      | Action.ViewNoWerewolves -> Some {Action.card=Role.Minion
                                       ; Action.player_or_center=player.name}
      | _ -> None)

let mystic_wolf_view player =
  List.find_map player.log ~f:(function
        Action.ViewAsMysticWolf view -> Some view
      | _ -> None)

let swap player =
  List.find_map player.log ~f:(function
        Action.Swap (card1, card2) -> Some (card1, card2)
      | _ -> None)

let is_evening_role player role =
  Role.equal player.evening_role role

let is_morning_role player role =
  Role.equal player.morning_role role

let update_role player role =
  {player with morning_role=role}

let add_action player ~action =
  {player with log=action::player.log}
