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
       | DreamWolf
       | Tanner
       | Minion
       | Seer
       | ApprenticeSeer
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
  | "dream wolf" -> Some DreamWolf
  | "apprentice seer" -> Some ApprenticeSeer
  | _ -> None

let to_string = function
    Werewolf -> "werewolf"
  | DreamWolf -> "dream wolf"
  | Seer -> "seer"
  | ApprenticeSeer -> "apprentice seer"
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
  | ApprenticeSeer -> "apprenticeseers"
  | Robber -> "robbers"
  | Villager -> "villagers"
  | Troublemaker -> "troublemakers"
  | Mason -> "masons"
  | Insomniac -> "insomniacs"
  | Tanner -> "tanners"
  | Minion -> "minions"
  | DreamWolf -> "dreamwolves"
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
  | DreamWolf -> Team.Werewolves
  | ApprenticeSeer -> Team.Villagers
  | Unassigned -> failwith "Cannot get team of unassigned."

let equal r1 r2 = r1 = r2
