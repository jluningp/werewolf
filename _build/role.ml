module Team = struct
  type t = Villagers
         | Werewolves
         | Tanner
         | NoOne

  let to_string = function
      Villagers -> "Villagers"
    | Werewolves -> "Werewolves"
    | Tanner -> "Tanner"
    | NoOne -> "No One"

  let equal t1 t2 = t1 = t2
end

type t = Werewolf
       | Tanner
       | Minion
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
  | "tanner" -> Some Tanner
  | "minion" -> Some Minion
  | _ -> None

let to_string = function
    Werewolf -> "werewolf"
  | Seer -> "seer"
  | Robber -> "robber"
  | Villager -> "villager"
  | Troublemaker -> "troublemaker"
  | Mason -> "mason"
  | Insomniac -> "insomniac"
  | Tanner -> "tanner"
  | Minion -> "minion"
  | Unassigned -> "no role yet"

let to_string_plural = function
    Werewolf -> "werewolves"
  | Seer -> "seers"
  | Robber -> "robbers"
  | Villager -> "villagers"
  | Troublemaker -> "troublemakers"
  | Mason -> "masons"
  | Insomniac -> "insomniacs"
  | Tanner -> "tanners"
  | Minion -> "minions"
  | Unassigned -> "no roles yet"

let team = function
    Werewolf -> Team.Werewolves
  | Seer -> Team.Villagers
  | Robber -> Team.Villagers
  | Villager -> Team.Villagers
  | Troublemaker -> Team.Villagers
  | Mason -> Team.Villagers
  | Insomniac -> Team.Villagers
  | Tanner -> Team.Tanner
  | Minion -> Team.Werewolves
  | Unassigned -> failwith "Cannot get team of unassigned."

let equal r1 r2 = r1 = r2
