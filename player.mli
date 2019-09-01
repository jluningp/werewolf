type t = {
    name : string
  ; id : string
  ; evening_role : Role.t
  ; morning_role : Role.t
  ; log : Action.t list
  } [@@deriving fields]

val create : string -> t

val is_ready : t -> bool

val is_ready_for_second_action : t -> bool

val is_vote_ready : t -> bool

val vote : t -> Action.vote option

val views : t -> Action.view list

val mystic_wolf_view : t -> Action.view option

val swap : t -> (string * string) option

val is_evening_role : t -> Role.t -> bool

val is_morning_role : t -> Role.t -> bool

val add_action : t -> action:Action.t -> t

(* Updates the morning role. Evening roles cannot be updated *)
val update_role : t -> Role.t -> t
