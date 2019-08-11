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
  let (unassigned, assigned) = List.split_n (List.permute roles) 3 in
  let pairs = List.zip_exn (List.permute (String.Map.keys game.players)) (assigned) in
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
  printf "We're actually doing the make swap\n";
  let actors = game.players
               |> String.Map.filter ~f:(fun p -> Role.equal (Player.evening_role p) role)
               |> String.Map.data
  in
  match actors with
    [] -> ((printf "Make swap did not occur because there is no %s\n" (Role.to_string role)); Out_channel.flush stdout; game)
  | [swapper] ->
     (let swapped = begin
          let open Option.Let_syntax in
          let%bind (p1, p2) = Player.swap swapper in
          printf "Swapped %s and %s\n" p1 p2;
          Out_channel.flush stdout;
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

(*
let get_player game player =
  String.Map.find_exn game.players player

let check_swaps game =
  let swapped = String.Map.filter game.players ~f:(fun p ->
                    not(Role.equal (Player.evening_role p) (Player.morning_role p))) in
  let swaps = List.filter_map (String.Map.data game.players) ~f:(fun p ->
                  Option.map (Player.swap p) ~f:(fun (p1, p2) ->
                      (Player.evening_role p, p1, p2))) in
  printf "Made swaps:%s\n" (List.to_string swaps ~f:(fun (p1, p2) -> sprintf "(%s, %s)" p1 p2));
  printf "Roles changed:%s\n" (List.to_string swapped ~f:Player.name);
  match swaps with
    [(Role.Robber, p1, p2)] -> (let player1 = get_player p1 in
                                let player2 = get_player p2 in
                                if Player.morning_role player1 = Player.evening_role player2
                                   && Player.morning_role player2 = Player.evening_role player1
                                then printf "Roles correctly swapped when there is only a robber.\n"
                                else
                                  printf "Roles Incorrect: P1: %s %s\n P2: %s %s\n"
                                    (Player.evening_role player1)
                                    (Player.morning_role player1)
                                    (Player.evening_role player2)
                                    (Player.morning_role player2))
  |
 *)

let make_moves game =
  game |> make_swap Role.Robber |> make_swap Role.Troublemaker
