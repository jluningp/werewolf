open Core
open Async

exception InvalidInput of string

module Page = struct
  let process_map page values =
    let regex = Str.regexp "%([a-zA-Z0-9_]+)" in
    let drop = function
      | ' ' | '%' | '(' | ')' -> true
      | _ -> false in
    Str.global_substitute
      regex
      (fun whole_page ->
        let variable = String.strip ~drop (Str.matched_string whole_page) in
        match String.Map.find values variable with
          None -> printf "Undefined variable %s" variable; "undefined"
        | Some value -> value)
      page

  let process page values =
    process_map page (String.Map.of_alist_exn values)
end


let games : game String.Table.t = String.Table.create ~size:5 ()

let update_game game =
  match String.Table.find game.code with
    None -> raise (InvalidInput "Game does not exist.")
  | Some _ -> String.Table.set games ~key:game.code ~data:game

let not_found =
  let%map page = Reader.file_contents "pages/not_found.html" in
  Http.response Http.NotFound ~content:page ~content_type:"text/html"

let page name replacements =
  let%map template = Reader.file_contents name in
  let content = Page.process template replacements in
  Http.response Http.Ok ~content:content ~content_type:"text/html"

let html ~content =
  Http.response Http.Ok ~content ~content_type:"text/html" |> return

let json content =
  Http.response Http.Ok ~content ~content_type:"text/json" |> return

let index () =
  let%map page = Reader.file_contents "pages/index.html" in
  Http.response Http.Ok ~content:page ~content_type:"text/html"

let favicon =
  let%map image = Reader.file_contents "images/favicon.jpg" in
  Http.response Http.Ok ~content:image ~content_type:"image/jpg"

let empty_response = html ~content:""

let game variables =
  let open Option.Let_syntax in
  let%bind code = String.Map.find variables "game" in
  String.Map.find games code

let game_exn ?(error="Invalid or missing game code.") variables =
  match game variables with
    None -> raise (InvalidInput error)
  | Some game -> game

let player game variables =
  let open Option.Let_syntax in
  let%bind name = String.Map.find variables "name" in
  let%bind id = String.Map.find variables "id" in
  let%bind player = String.Map.find game.players name in
  Option.some_if (name = player.name && id = player.id) player

let player_exn ?(error="Invalid player name or id.") game variables =
  match player variables game with
    None -> raise (InvalidInput error)

let config_exn ?(error="Invalid config options.") variables role =
  match String.Map.find variables str with
    None -> raise (InvalidInput error)
  | Some value -> (try Int.of_string value
                   with Failure _ -> raise (InvalidInput error))

let display_players players ~wrap =
  players
  |> String.Map.keys
  |> List.map ~f:wrap
  |> String.concat ~sep:""

let config_info game =
  let player_count = Game.player_count game in
  let role_count = Config.count game.config in
  let roles = List.map (Config.to_alist game.config) ~f:(fun (role, count) ->
                  (Role.to_string role, Int.to_string count)) in
  let players = display_players game.players ~wrap:(fun p ->
                    "<span class='player'>" ^ p ^ "</span><br>")
  in
  [("game", game.code)
  ; ("players", players)
  ; ("player_number", Int.to_string player_count)
  ; ("rolenum", Int.to_string role_count)
  ; ("roleneeded", Int.to_string (player_count + 3 - role_count))]
  @ roles

let other_werewolves game player =
  game.players
  |> String.Map.filter ~f:(fun p -> Role.equal p.evening_role Role.Werewolf
                                    && p.name <> player.name)
  |> String.Map.keys

let other_masons game player =
  game.players
  |> String.Map.filter ~f:(fun p -> Role.equal p.evening_role Role.Masons
                                    && p.name <> player.name)
  |> String.Map.keys

let player_choice game player =
  game.players
  |> String.Map.filter ~f:(fun p -> p.name <> player.name)
  |> display_players ~wrap:(fun p ->
         "<input type='radio' id='" ^ p ^ "'>" ^ p ^ "<br>")

let player_choices game player =
  game.players
  |> String.Map.filter ~f:(fun p -> p.name <> player.name)
  |> display_players ~wrap:(fun p ->
         "<input type='checkbox' id='" ^ p ^ "'>" ^ p ^ "<br>")

let update_state game =
  match game.stage with
    Game.Night -> if String.Map.for_all game.players ~f:(fun p ->
                         Player.is_ready p || Role.equal p.evening_role Role.Insomniac)
                  then Game.set_state game Game.Morning
                  else game
  | Game.Morning -> if String.Map.for_all game.players ~f:Player.is_ready
                    then Game.set_state game Game.Debate
                    else game
  | _ -> game


let config_page game player =
  let conditional_page = if player.name = game.owner
                         then "pages/config.html"
                         else "pages/config_view.html"
  in
  page conditional_page (config_info game)

let role_page game player =
  let conditional_element =
    if player.name = game.owner
    then "<button onclick='beginNight()' id='night'>Begin Night!</button>"
    else "<input type='hidden' id='view_role'>"
  in
  page "pages/role.html" [("role", player.evening_role)
                        ; ("button", conditional_element)]


let werewolf_see game player =
  let card = Cards.draw1 game.unassigned in
  (player
   |> Player.add_event ~event:(Event.View card)
   |> Game.set_player game
   |> update_game);
  card

let werewolf_page game player =
  match other_werewolves game player with
    [] -> let card = werewolf_see game player in
          page "pages/lone_werewolf.html" [("card", card)]
  | wolves -> let wolf_list = String.concat ~sep:"<br>" wolves in
              page "pages/werewolves.html" [("names", wolf_list)]

let mason_page game player =
  match other_masons game player with
    [] -> page "pages/lone_mason.html" []
  | masons -> let mason_list = String.concat ~sep:"<br>" masons in
              page "pages/masons.html" [("names", mason_list)]

let seer_page game player =
  match Player.views player with
    [] -> page "pages/seer.html" [("players", player_choice game player)]
  | [{card; player_or_center}] -> page "pages/seer_reveal_player.html"
                                    [("name", player_or_center)
                                    ; ("role", card)]
  | [card1; card2] -> page "pages/seer_reveal_center.html"
                        [("role1", card1.card)
                        ; ("role2", card2.card)]
  | _ -> failwith "Invariant violated: Seer saw more than 2 cards"

let robber_page game player =
  match Player.swap player with
    None -> page "pages/robber.html" [("players", player_choice game player)]
  | Some (_, p2) -> page "pages/robber.html" [("role", p2)]

let troublemaker_page game player =
  match Player.swap player with
    None -> page "pages/troublemaker.html" [("players", player_choices game player)]
  | Some _ -> page "pages/wait.html"

let villager_page game player =
 (if Player.is_ready player
  then ()
  else player
       |> Player.add_event ~event:(Event.Ready)
       |> Game.set_player game
       |> update_game);
 page "pages/wait.html" []

let insomniac_page game player =
  if Role.equal player.evening_role player.morning_role
  then page "pages/insomniac_same.html" []
  else page "page/insomniac_change.html" [("role", player.morning_role)]

let night_page game player =
  if Player.is_ready player
  then page "pages/wait.html" []
  else
    match player.evening_role with
      Role.Werewolf -> werewolf_page game player
    | Role.Mason -> mason_page game player
    | Role.Seer -> seer_page game player
    | Role.Robber -> robber_page game player
    | Role.Troublemaker -> troublemaker_page game player
    | Role.Villager -> villager_page game player
    | Role.Insomniac -> page "pages/wait.html" []

let morning_page game player =
  if Player.is_ready player
  then page "pages/wait.html" []
  else
    match player.evening_role with
      Role.Insomniac -> insomniac_page game player
    | _ -> page "pages/wait.html"

let debate_page game player =
  page "pages/debate.html" []

let current_page old_game player =
  let game = update_state old_game in
  game |> update_game;
  match game.state with
    Config -> config_page game player
  | Role -> role_page game player
  | Night -> night_page game player
  | Morning -> morning_page game player
  | Debate -> debate_page game player

let player_section game =
  sprintf "%s<br><span id=\"playerNum\">%i</span> Players<br>"
    (display_players game.players ~wrap:(fun p ->
         "<span class='player'>" ^ p ^ "</span><br>"))
    (String.Map.length game.players)

let game_info game player =
  json (sprintf "{\"game\" : \"%s\", \"name\" : \"%s\", \"id\" : \"%s\"}"
          game.code
          player.name
          player.id)

let join_game variables =
  let opt = begin
      let open Option.Let_syntax in
      let%bind code = String.Map.find variables "game" in
      let%bind game = String.Table.find games code in
      let%map name = String.Map.find variables "name" in
      (game, name)
    end
  in
  match opt with
  | Some (game, name) -> begin
      let player = Player.create name in
      Game.add_player ~exn:(fun s -> InvalidInput s) game player |> update_game;
      game_info game player
    end
  | None -> raise (InvalidInput "Game code or name is invalid or missing.")

let unique_game game =
  match String.Table.find games game.code with
    None -> true
  | Some _ -> false

let rec new_game variables =
  match String.Map.find variables "name" with
    None -> raise (InvalidInput "No username given.")
  | Some name -> let game = Game.create name in
                 if unique_game game
                 then join_game (String.Map.add
                                   variables
                                   ~key:"game"
                                   ~data:game.code)
                 else new_game variables

let refresh variables =
  let game = game_exn variables ~error:"No game found." in
  let player = player_exn variables game ~error:"You have not joined this game yet." in
  current_page game player

let update_config variables =
  let game = game_exn variables in
  let config = {
      werewolf=config_exn "werewolves"
    ; robber=config_exn "robbers"
    ; troublemaker=config_exn "troublemakers"
    ; seer=config_exn "seers"
    ; insomniac=config_exn "insomniacs"
    ; mason=config_exn "masons"
    ; villager=config_exn "villagers"
    }
  in {game with config} |> update_game

let start_game variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  update_config variables (* renamed update_config *);
  Game.assign_roles game |> update_game

let begin_night variables =
  let game = game_exn variables in
  {game with state=Night} |> update_game

let players variables =
  let game = game_exn variables in
  html ~content:(player_section game)

let ready variables =
  let game = game_exn variables in
  let player = player_exn variables in
  Game.set_player game (Player.add_event player Event.Ready)
  |> update_game

let rob variables =
  let game = game_exn variables in
  let player = player_exn variables in
  if Role.equal player.evening_role Role.Robber && Option.is_none (Player.swap player)
  then match String.Map.find variables "target" with
       | Some name -> (player
                       |> Player.add_event ~event:(Event.swap (player.name, name))
                       |> Game.set_player game
                       |> update_game)
       | None -> raise (InvalidInput "No target given for robber action.")
  else raise (InvalidInput "You cannot rob (or rob again).")

let troublemake variables =
  let game = game_exn variables in
  let player = player_exn variables in
  if Role.equal player.evening_role Role.Troublemaker && Option.is_none (Player.swap player)
  then match (String.Map.find variables "target1", String.Map.find variables "target2") with
         (Some name1, Some name2) -> (player
                                      |> Player.add_event ~event:(Event.Swap (name1, name2))
                                      |> Player.add_event ~event:(Event.Ready)
                                      |> Game.set_player game
                                      |> update_game)
       | _ -> raise (InvalidInput "Invalid or missing targets for troublemaker action.")
  else raise (InvalidInput "You cannot troublemake (or re-troublemake).")

let see variables =
  let game = game_exn variables in
  let player = player_exn variables in
  if Role.equal player.evening_role Role.Seer && List.null (Player.views player)
  then match String.Map.find variables "target" with
         Some "center" -> let (card1, card2) = Cards.draw2 game.unassigned in
                          (player
                           |> Player.add_event ~event:(Event.View card1)
                           |> Player.add_event ~event:(Event.View card2)
                           |> Game.set_player game
                           |> update_game)
       | Some name -> (player
                       |> Player.add_event ~event:(Event.View name)
                       |> Game.set_player game
                       |> update_game)
       | None -> raise (InvalidInput "Invalid or missing targets for seer action.")
  else raise (InvalidInput "You cannot see (or see twice).")

let direct url variables =
  let v = variables in
  let parts = Str.split (Str.regexp "/+") url in
  printf (List.to_string parts ~f:(fun s -> s));
  try
    match parts with
    | ["refresh"] -> refresh variables
    | ["newgame"] -> new_game variables
    | ["joingame"] -> join_game variables
    | ["savesettings"] -> update_config variables; refresh variables
    | ["startgame"] -> start_game variables; refresh variables
    | ["beginnight"] -> begin_night variables; empty_response
    | ["favicon.ico"] -> favicon
    | ["players"] -> players variables
    | ["ready"] -> ready variables; empty_response
    | ["rob"] -> rob variables; empty_response
    | ["troublemake"] -> troublemake variables; empty_response
    | ["see"] -> see variables; empty_response
    | [] -> index ()
    | _ -> not_found
  with InvalidInput s -> page "pages/start.html" [("error", s)]

let run () =
  let%bind server =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 8000)
      (fun _addr r w ->
        let%bind req = Http.parse_request r in
        let%map response = match req with
            None -> return (Http.response BadRequest ~content:"" ~content_type:"text/html")
          | Some request -> (match request.Http.http_method with
                               Get variables -> direct request.uri variables
                             | _ -> not_found)
        in
        Writer.write w response)
  in
  Tcp.Server.close_finished server

let () =
  Command.run (Command.async ~summary:"An echo server" (Command.Param.return run))
