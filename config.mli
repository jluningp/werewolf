type t = {
    werewolf : int
  ; dream_wolf : int
  ; mystic_wolf : int
  ; tanner : int
  ; minion : int
  ; seer : bool
  ; apprentice_seer : bool
  ; robber : bool
  ; villager : int
  ; troublemaker : bool
  ; mason : int
  ; insomniac : int
  }

val empty : t

val count : t -> int

val to_list_dups : t -> Role.t list

val to_alist : t -> (Role.t * int) list

val update : t -> role:Role.t -> count:int -> t option
