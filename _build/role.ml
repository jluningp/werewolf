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

let description = function
  | Werewolf -> "You are on the werewolf team. Your goal is to trick the villagers into believing that you and your fellow werewolves are not werewolves. You get to see who the other werewolves are, and if there are no other werewolves, you can view a card from the center."
  | Seer -> "You are on the villager team. You can see the card of another player, or two of the center cards."
  | ApprenticeSeer -> "You are on the villager team. You see a card from the center."
  | Robber -> "You are on the villager team. You swap roles with another player of your choice and get to view your new role."
  | Villager -> "You are on the villager team. You do nothing."
  | Troublemaker -> "You are on the villager team. You swap the roles of two other players."
  | Mason -> "You are on the villager team. You get to see who the other masons are, if there are any."
  | Insomniac -> "You are on the villager team. At the end of the night, you get to see what your new role is."
  | Tanner -> "You are on your own team. Your goal is get voted for."
  | Minion -> "You are on the werewolf team. You get to see who the werewolves are, but they do not know who you are. You win if the werewolves are not voted for, even if you are voted for."
  | DreamWolf -> "You are on the werewolf team. You are like a werewolf, except you do not get to know who the other werewolves are, if there are any."
  | Unassigned -> "There has been an error, and you have not been assigned a role. Please start a new game."

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
