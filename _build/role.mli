module Team : sig
  type t = Villagers
         | Werewolves
         | Tanner
         | NoOne

  val to_string : t -> string

  val equal : t -> t -> bool
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

val of_string : string -> t option

val to_string : t -> string

val to_string_plural : t -> string

val equal : t -> t -> bool

val team : t -> Team.t
