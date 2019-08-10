type t = {
    name : string
  ; id : string
  ; evening_role : Role.t
  ; morning_role : Role.t
  ; log : Action.t list
  } [@@deriving fields]

val create : string -> t

val is_ready : t -> bool

val views : t -> Action.view list

val swap : t -> (string * string) option

val is_evening_role : t -> Role.t -> bool

val is_morning_role : t -> Role.t -> bool

val add_action : t -> action:Action.t -> t

(* Updates the morning role. Evening roles cannot be updated *)
val update_role : t -> Role.t -> t
