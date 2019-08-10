type t = Werewolf
       | Seer
       | Robber
       | Villager
       | Troublemaker
       | Mason
       | Insomniac
       | Unassigned

let of_string = function
    "werewolf" -> Some Werewolf
  | "seer" -> Some Seer
  | "robber" -> Some Robber
  | "villager" -> Some Villager
  | "troublemaker" -> Some Troublemaker
  | "mason" -> Some Mason
  | "insomniac" -> Some Insomniac
  | _ -> None

let to_string = function
    Werewolf -> "werewolf"
  | Seer -> "seer"
  | Robber -> "robber"
  | Villager -> "villager"
  | Troublemaker -> "troublemaker"
  | Mason -> "mason"
  | Insomniac -> "insomniac"
  | Unassigned -> "no role yet"

let to_string_plural = function
    Werewolf -> "werewolves"
  | Seer -> "seers"
  | Robber -> "robbers"
  | Villager -> "villagers"
  | Troublemaker -> "troublemakers"
  | Mason -> "masons"
  | Insomniac -> "insomniacs"
  | Unassigned -> "no roles yet"

let equal r1 r2 = r1 = r2
