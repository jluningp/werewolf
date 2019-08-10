open Core

type state = Config
           | Role
           | Night
           | Morning (* Insomniac's turn *)
           | Debate


type t = {
    code : string
  ; owner : string
  ; players : Player.t String.Map.t
  ; config : Config.t
  ; unassigned : Role.t list
  ; state : state;
  } [@@deriving fields]

let gen_code n =
  let a = int_of_char 'A' in
  List.init n ~f:(fun _ -> char_of_int (a + Random.int 26))
  |> String.of_char_list

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
    Config -> (match String.Map.add game.players ~key:(Player.name player) ~data:player with
                 `Duplicate -> raise (exn "Duplicate player name.")
               | `Ok players -> {game with players})
  | _ -> raise (exn "Cannot add player while game in progress.")

let set_player game player =
  {game with players=String.Map.set game.players ~key:(Player.name player) ~data:player}

let is_player game name =
  String.Map.mem game.players name

let player_count game = String.Map.length game.players

let assign_roles game =
  let roles = Config.to_list_dups game.config in
  let (unassigned, assigned) = List.split_n roles 3 in
  let pairs = List.zip_exn (String.Map.keys game.players) (assigned) in
  let players = List.fold pairs ~init:String.Map.empty ~f:(fun players (name, role) ->
                    match String.Map.find game.players name with
                      None -> failwith "Invariant violated: Role pairing not a valid player"
                    | Some player -> String.Map.set
                                       players
                                       ~key:name
                                       ~data:{player with morning_role=role; evening_role=role})
  in {game with players; unassigned; state=Role}

let set_state game state =
  {game with state}

let set2 m (k1, d1) (k2, d2) =
  String.Map.set (String.Map.set m ~key:k1 ~data:d1) ~key:k2 ~data:d2

let make_swap role game =
  let actors = game.players
               |> String.Map.filter ~f:(fun p -> Role.equal (Player.evening_role p) role)
               |> String.Map.data
  in
  match actors with
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
  | _ -> failwith ("Invariant violated. More than one " ^ Role.to_string role ^ " in the game.")

let make_moves game =
  game |> make_swap Role.Robber |> make_swap Role.Troublemaker
