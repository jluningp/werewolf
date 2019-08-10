type t = Werewolf
       | Seer
       | Robber
       | Villager
       | Troublemaker
       | Mason
       | Insomniac
       | Unassigned

val of_string : string -> t option

val to_string : t -> string

val to_string_plural : t -> string

val equal : t -> t -> bool
