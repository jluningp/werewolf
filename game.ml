open Core
open Async

let debug = true
let debug_log = if debug then printf else sprintf
let dl = debug_log

let gen_code n =
  let a = int_of_char 'A' in
  List.init n ~f:(fun _ -> char_of_int (a + Random.int 26))
  |> String.of_char_list

module type ROLE = sig
  type t = Werewolf
         | Seer
         | Robber
         | Villager
         | Troublemaker
         | Mason

  val of_string : string -> t option
  val to_string : t -> string
  val to_string_plural : t -> string
  val equal : t -> t -> bool
end


module Role : ROLE = struct
  type t = Werewolf
         | Seer
         | Robber
         | Villager
         | Troublemaker
         | Mason

  let of_string = function
      "werewolf" -> Some Werewolf
    | "seer" -> Some Seer
    | "robber" -> Some Robber
    | "villager" -> Some Villager
    | "troublemaker" -> Some Troublemaker
    | "mason" -> Some Mason
    | _ -> None

  let to_string = function
      Werewolf -> "werewolf"
    | Seer -> "seer"
    | Robber -> "robber"
    | Villager -> "villager"
    | Troublemaker -> "troublemaker"
    | Mason -> "mason"

  let to_string_plural = function
      Werewolf -> "werewolves"
    | Seer -> "seers"
    | Robber -> "robbers"
    | Villager -> "villagers"
    | Troublemaker -> "troublemakers"
    | Mason -> "masons"

  let equal r1 r2 = r1 = r2
end

module type EVENT = sig
  type view = {
      card : string
    ; player_or_center : string
    }

  type t = View of view
         | Swap of string * string
         | Ready

  val to_string : t -> string
end

module Event : EVENT = struct
  type view = {
      card : string
    ; info : string (* Could be "center" or "player" for example *)
    }

  type t = View of view
         | Swap of string * string
         | Ready

  let to_string = function
      View {card; info} -> sprintf "Viewed %s (%s)" card info
    | Swap (card1, card2) -> sprintf "Swapped %s with %s" card1 card2
    | Ready -> sprintf "Ready"
end

module type PLAYER = sig
  type t = {
      name : string
    ; id : string
    ; evening_role : role
    ; morning_role : role
    ; log : event list
    } [@@deriving fields]

  val create : string -> t
  val is_ready : player -> bool
  val views : player -> view list
  val swap : player -> (string * string) option
  val is_evening_role : player -> Role.t -> bool
  val is_morning_role : player -> Role.t -> bool
  val add_event : t -> ~event:Event.t -> t
  (* Updates the morning role. Evening roles cannot be updated *)
  val update_role : player -> Role.t -> player
end

module Player : PLAYER = struct
  type t = {
      name : string
    ; id : string
    ; evening_role : role
    ; morning_role : role
    ; log : event list
    } [@@deriving fields]

  let create name =
    {name
    ; id=gen_code 8
    ; evening_role=""
    ; morning_role=""
    ; log=[]}

  let is_ready player =
    List.exists player.log ~f:(function
          Event.Ready -> true
        | _ -> false)

  let views player =
    List.filter_map player.log ~f:(function
          View {card; info} -> Some card
        | _ -> None)

  let swap player =
    List.find_map player.log ~f:(function
          Swap (card1, card2) -> Some (card1, card2)
        | _ -> None)

  let is_evening_role player role =
    Role.equal player.evening_role role

  let is_morning_role player role =
    Role.equal player.morning_role role

  let update_role player role =
    {player with morning_role=role}

  let add_event player ~event =
    {player with log=event::player.log}
end

module type CONFIG = sig
  type t = {
      werewolf : int
    ; seer : bool
    ; robber : bool
    ; villager : int
    ; troublemaker : bool
    ; mason : int
    }

  val empty : t
  val count : t -> int
  val to_list_dups : t -> Role.t list
  val to_alist : t -> (Role.t * int) list
  val update_role : t -> Role.t -> int -> t
end

module Config : CONFIG = struct
  type t = {
      werewolf : int
    ; seer : bool
    ; robber : bool
    ; villager : int
    ; troublemaker : bool
    ; mason : int
    }

  let empty = {
      werewolf=0
    ; seer=false
    ; robber=false
    ; villager=0
    ; troublemaker=false
    ; mason=0
    }

  let count {werewolf; seer; robber; villager; troublemaker; mason} =
    werewolf + seer + robber + villager + troublemaker + mason

  let to_list_dups {werewolf; seer; robber; villager; troublemaker; mason} =
    List.concat [List.init werewolf ~f:(fun _ -> Role.Werewolf)
               ; if seer then [Role.Seer] else []
               ; if robber then [Role.Robber] else []
               ; List.init villager ~f:(fun _ -> Role.Villager)
               ; if troublemaker then [Role.Troublemaker] else []
               ; List.init mason ~f:(fun _ -> Role.Mason)]

  let to_alist {werewolf; seer; robber; villager; troublemaker; mason} =
    [(Role.Werewolf, werewolf)
    ; (Role.Seer, if seer then 1 else 0)
    ; (Role.Robber, if robber then 1 else 0)
    ; (Role.Villager, villager)
    ; (Role.Troublemaker, if troublemaker then 1 else 0)
    ; (Role.Mason, mason)]

  let update_role config role count =
    let valid_bool n = n = 1 || n = 0 in
    if count < 0 then None else
      match role with
        Role.Werewolf -> Some {config with werewolf=count}
      | Role.Seer -> Option.some_if (valid_bool count) {config with seer=(count = 1)}
      | Role.Robber -> Option.some_if (valid_bool count) {config with robber=(count = 1)}
      | Role.Villager -> Some {config with villager=count}
      | Role.Troublemaker -> Option.some_if (valid_bool count) {config with troublemaker=(count = 1)}
      | Role.Mason -> Some {config with mason=count}

end

module Cards = struct
  type cards = string list

  let draw cards n =
    List.take (List.permute cards) n

  let draw1 cards =
    match draw cards 1 with
      [c] -> c
    | _ -> failwith "Invariant violated: Insufficient cards."

  let draw2 cards =
    match draw cards 2 with
      [c1; c2] -> (c1, c2)
    | _ -> failwith "Invariant violated: Insufficient cards."

end

type state = Config
           | Role
           | Night of moves
           | Morning (* Insomniac's turn *)
           | Debate


type game = {
    code : string
  ; owner : string
  ; players : player String.Map.t
  ; config : config
  ; unassigned : string list
  ; state : state;
  }

let create owner = {
    code=gen_code 4
  ; owner
  ; players=String.Map.empty
  ; config=Config.empty
  ; unassigned=[]
  ; state=Config
  }

let add_player ?(exn=fun s -> Failure s) game player =
  match game.state with
    Config -> (match String.Map.add game.players ~key:player.name ~data:player with
                 `Duplicate -> raise (exn "Duplicate player name.")
               | `Ok players -> {game with players})
  | _ -> raise (exn "Cannot add player while game in progress.")

let set_player game player =
  {game with players=String.Map.set game.players ~key:player.name ~data:player}

let player_count game = String.Map.length game.players

let assign_roles game =
  let roles = Config.to_list_dups game.config in
  let (unassigned, assigned) = List.split roles 3 in
  let pairs = List.zip_exn (String.Map.keys game.players) (assigned) in
  dl "%s" (List.to_string ~f:(fun (name, role) -> sprintf "(%s, %s)" name role pairs));
  let players = List.fold pairs ~init:String.Map.empty ~f:(fun players (name, role) ->
                    match String.Map.find game.players name with
                      None -> failwith "Invariant violated: Role pairing not a valid player"
                    | Some player -> String.Map.set
                                       players
                                       ~key:name
                                       ~data:{player with morning_role=role; evening_role=role})
  in {game with players; state=Role}

let set_state game state =
  {game with state}

let set2 m (k1, d1) (k2, d2) =
  String.Map.set (String.Map.set m ~key:k1 ~data:d1) ~key:k2 ~data:d2

let make_swap role game =
  match String.Map.filter game.players ~f:(is_evening_role role) with
    [] -> game
  | [swapper] -> (let swapped = begin
                      let open Option.Let_syntax in
                      let%bind (p1, p2) = Player.swap swapper in
                      let%bind player1 = String.Map.find game.players p1 in
                      let%map player2 = String.Map.find game.players p2 in
                      set2 game.players
                        (p1, Player.update_role player1 player2.morning_role)
                        (p2, Player.update_role player2 player1.morning_role)
                    end
                  in match swapped with
                       Some players -> {game with players}
                     | None -> failwith ("Invariant violated: " ^ Role.to_string role
                                         ^  " swapped nonexistant player or did not make a swap."))
  | _ -> (failwith "Invariant violated. More than one " ^ Role.to_string role ^ " in the game.")

let make_moves game =
  game |> make_swap Role.Robber |> make_swap Role.Troublemaker
