type view = {
    card : Role.t
  ; player_or_center : string
  }

type t = View of view
       | Swap of string * string
       | Ready

val to_string : t -> me:string -> string
