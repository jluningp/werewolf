open Core

type results = {
    winner : Role.Team.t
  ; voted_for : Action.vote
  ; votes : int String.Map.t
  }

type state = Config
           | Role
           | Night
           | Morning (* Insomniac's turn *)
           | Debate
           | Vote
           | Results of results


type t = {
    code : string
  ; owner : string
  ; players : Player.t String.Map.t
  ; config : Config.t
  ; unassigned : Role.t list
  ; state : state;
  } [@@deriving fields]

val create : string -> t

val add_player : ?exn:(string -> exn) -> t -> Player.t -> t

val set_player : t -> Player.t -> t

val player_count : t -> int

val assign_roles : t -> t

val set_state : t -> state -> t

val make_moves : t -> t

val is_player : t -> string -> bool

val cast_votes : t -> results
