type view = {
    card : Role.t
  ; player_or_center : string
  }

type vote = Center
          | Player of string

type t = View of view
       | ViewLoneMason
       | ViewNoWerewolves
       | ViewAsMysticWolf of view
       | Swap of string * string
       | ReadyForSecondAction
       | Ready
       | VoteReady
       | Vote of vote

val to_string : ?third_person:bool -> t -> me:string -> string
