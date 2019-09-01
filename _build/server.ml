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


let games : Game.t String.Table.t = String.Table.create ~size:5 ()

let test_player name role =
  let n = name in
  let open Player in
  {name=n
  ; id=n
  ; evening_role=role
  ; morning_role=role
  ; log=[]}

let test_game =
  let open Game in
  { code="TEST"
  ; owner="B"
  ; players = String.Map.of_alist_exn [("B", test_player "B" Role.Robber)
                                     ; ("H", test_player "H" Role.Mason)
                                     ; ("I", test_player "I" Role.Insomniac)]
  ; unassigned=[Role.Villager; Role.Werewolf; Role.Mason]
  ; state=Game.Role
  ; config={
      werewolf=0
    ; dream_wolf=0
    ; mystic_wolf=0
    ; seer=false
    ; apprentice_seer=false
    ; robber=true
    ; villager=1
    ; troublemaker=false
    ; mason=2
    ; insomniac=1
    ; tanner=0
    ; minion=0
    }
  }

let () = String.Table.set games ~key:"TEST" ~data:test_game

let add_game game =
  String.Table.add games ~key:(Game.code game) ~data:game |> ignore

let update_game game =
  match String.Table.find games (Game.code game) with
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

let image img ~extension =
  let%map image = Reader.file_contents ("images/" ^ img) in
  Http.response Http.Ok ~content:image ~content_type:("image/" ^ extension)

let favicon = image "favicon.jpg" ~extension:"jpg"

let empty_response = html ~content:""

let game variables =
  let open Option.Let_syntax in
  let%bind code = String.Map.find variables "game" in
  String.Table.find games code

let game_exn ?(error="Invalid or missing game code.") variables =
  match game variables with
    None -> raise (InvalidInput error)
  | Some game -> game

let player game variables =
  let open Option.Let_syntax in
  let%bind name = String.Map.find variables "name" in
  let%bind id = String.Map.find variables "id" in
  let%bind player = String.Map.find (Game.players game) name in
  Option.some_if (name = player.name && id = player.id) player

let player_exn ?(error="Invalid player name or id.") game variables =
  match player variables game with
    None -> raise (InvalidInput error)
  | Some player -> player

let config_exn ?(error="Invalid config options.") variables role =
  match String.Map.find variables role with
    None -> raise (InvalidInput (error ^ " Config: " ^ role))
  | Some value -> (try Int.of_string value
                   with Failure _ -> raise (InvalidInput (error ^ " Config: " ^ role)))

let name_to_role game name =
  match String.Map.find (Game.players game) name with
    None -> raise (InvalidInput "Viewed a card that doesn't exist.")
  | Some player -> Player.morning_role player

let display_players players ~wrap =
  players
  |> String.Map.keys
  |> List.map ~f:wrap
  |> String.concat ~sep:""

let int_to_checkbox i =
  match i with
    0 -> ""
  | _ -> "checked"

let config_info ?(edit=false) game =
  let player_count = Game.player_count game in
  let role_count = Config.count game.config in
  let roles = List.map (Config.to_alist game.config) ~f:(fun (role, count) ->
                  (Role.to_string_plural role,
                   match role with
                     (Role.Troublemaker | Role.Robber
                     | Role.Seer | Role.ApprenticeSeer) ->
                      if edit
                      then int_to_checkbox count
                      else Int.to_string count
                   | _ -> Int.to_string count)) in
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
  game
  |> Game.players
  |> String.Map.filter ~f:(fun p -> (Role.equal (Player.evening_role p) Role.Werewolf
                                     || Role.equal (Player.evening_role p) Role.MysticWolf)
                                    && (Player.name p <> Player.name player))
  |> String.Map.keys

let dream_wolves game =
  game
  |> Game.players
  |> String.Map.filter ~f:(fun p -> Role.equal (Player.evening_role p) Role.DreamWolf)
  |> String.Map.keys

let other_masons game player =
  game
  |> Game.players
  |> String.Map.filter ~f:(fun p -> Role.equal (Player.evening_role p) Role.Mason
                                    && Player.name p <> Player.name player)
  |> String.Map.keys

let player_choice ?(keep_self=false) game player =
  game
  |> Game.players
  |> String.Map.filter ~f:(fun p -> keep_self || (Player.name p <> Player.name player))
  |> display_players ~wrap:(fun p ->
         "<input type='radio' name='playerchoice' id='" ^ p ^ "'>&nbsp;&nbsp;" ^ p ^ "<br>")

let player_choices game player =
  game
  |> Game.players
  |> String.Map.filter ~f:(fun p -> Player.name p <> Player.name player)
  |> display_players ~wrap:(fun p ->
         "<input type='checkbox' id='" ^ p ^ "'>&nbsp;&nbsp;" ^ p ^ "<br>")

let update_state game =
  let readies = String.Map.filter (Game.players game) ~f:(fun p ->
                    not(Player.is_ready p)) in
  match Game.state game with
    Game.Night -> if String.Map.for_all game.players ~f:(fun p ->
                         Player.is_ready p || Role.equal (Player.evening_role p) Role.Insomniac)
                  then (printf "Making moves\n"; Game.set_state game Game.Morning |> Game.make_moves)
                  else (printf "Not ready: %s\n"
                          (List.to_string (String.Map.keys readies) ~f:(fun i -> i));
                        game)
  | Game.Morning -> if String.Map.for_all game.players ~f:Player.is_ready
                    then Game.set_state game Game.Debate
                    else game
  | Game.Debate -> if String.Map.for_all game.players ~f:Player.is_vote_ready
                   then Game.set_state game Game.Vote
                   else game
  | Game.Vote -> if String.Map.for_all game.players ~f:(fun p ->
                        Option.is_some (Player.vote p))
                 then Game.set_state game (Game.Results (Game.cast_votes game))
                 else game
  | _ -> game


let config_page game player =
  let conditional_page = if Player.name player = Game.owner game
                         then "pages/config.html"
                         else "pages/config_view.html"
  in
  page conditional_page (config_info ~edit:(Player.name player = Game.owner game) game)

let role_page game player =
  let conditional_element =
    if Player.name player = Game.owner game
    then "<button onclick='beginNight()' id='night'>Begin Night!</button>"
    else "Waiting for owner to begin game.<input type='hidden' id='view_role'>"
  in
  page "pages/role.html" [("role", Role.to_string_plural (Player.evening_role player))
                        ; ("button", conditional_element)
                        ; ("description", Role.description (Player.evening_role player))]


let see_one_center game player =
  let card = Cards.draw1 (Game.unassigned game) in
  (player
   |> Player.add_action ~action:(Action.View {card; Action.player_or_center="center"})
   |> Game.set_player game
   |> update_game);
  card

let werewolf_see_others game player wolves dream_wolves =
  let player = List.fold wolves ~init:player ~f:(fun p wolf ->
                   Player.add_action
                     p
                     ~action:(Action.View {card=Role.Werewolf;
                                           Action.player_or_center=wolf})) in
  let player = List.fold dream_wolves ~init:player ~f:(fun p wolf ->
                   Player.add_action
                     p
                     ~action:(Action.View {card=Role.DreamWolf;
                                           Action.player_or_center=wolf})) in
  Game.set_player game player |> update_game


let mason_see_others game player masons =
  let player = List.fold masons ~init:player ~f:(fun p mason ->
                   Player.add_action
                     p
                     ~action:(Action.View {card=Role.Mason;
                                           Action.player_or_center=mason})) in
  Game.set_player game player |> update_game

let mason_alone game player =
  player
  |> Player.add_action ~action:Action.ViewLoneMason
  |> Game.set_player game
  |> update_game

let insomniac_see_self game player =
  player
  |> Player.add_action ~action:(Action.View {card=Player.morning_role player
                                           ; Action.player_or_center=Player.name player})
  |> Game.set_player game
  |> update_game

let apprentice_seer_page game player =
  match Player.views player with
    [] -> let card = see_one_center game player in
          page "pages/apprentice_seer.html" [("card", Role.to_string_plural card)]
  | [card] -> page "pages/apprentice_seer.html" [("card", Role.to_string_plural card.Action.card)]
  | _ -> failwith "Broken invariant: apprentice seer saw multiple cards"

let werewolf_page game player =
  match other_werewolves game player, dream_wolves game with
    ([], []) -> (match Player.views player with
                   [] -> let card = see_one_center game player in
                         page "pages/lone_werewolf.html" [("card", Role.to_string_plural card)]
                 | [card] -> page "pages/lone_werewolf.html" [("card", Role.to_string_plural card.Action.card)]
                 | _ -> failwith "Broken invariant: lone werewolf saw multiple cards")
  | (wolves, dream_wolves) ->
     (match Player.views player with
        [] -> werewolf_see_others game player wolves dream_wolves
      | _ -> ());
     let maybe_wolf_list =
       if List.is_empty wolves
       then "<br>No other werewolves.<br><br>"
       else sprintf "<ul>%s</ul>"
              (String.concat ~sep:""
                 (List.map wolves ~f:(fun s ->
                      sprintf "<li>%s</li>" s))) in
     let maybe_dream_wolf_list =
       if List.is_empty dream_wolves
       then ""
       else sprintf "<center><large><strong>Dream Wolves</strong></large></center><ul>%s</ul>"
              (String.concat ~sep:""
                 (List.map dream_wolves ~f:(fun s ->
                      sprintf "<li>%s</li>" s))) in
     page "pages/werewolves.html" [("werewolves", maybe_wolf_list);
                                   ("dream_wolves", maybe_dream_wolf_list)]


let mystic_wolf_page game player =
  if Player.is_ready_for_second_action player
  then (match Player.mystic_wolf_view player with
          None -> page "pages/mystic_wolf.html" [("players", player_choice game player)]
        | Some {card; player_or_center} -> page "pages/reveal_player.html"
                                             [("name", player_or_center)
                                             ; ("role", Role.to_string_plural card)])
  else werewolf_page game player

let mason_page game player =
  match other_masons game player with
    [] -> (match Player.views player with
             [] -> mason_alone game player
           | _ -> ());
          page "pages/lone_mason.html" []
  | masons -> (match Player.views player with
                 [] -> mason_see_others game player masons
               | _ -> ());
              let mason_list = sprintf "<ul>%s</ul>" (String.concat ~sep:""
                                                        (List.map masons ~f:(fun s ->
                                                             sprintf "<li>%s</li>" s))) in
              page "pages/masons.html" [("names", mason_list)]

let seer_page game player =
  match Player.views player with
    [] -> page "pages/seer.html" [("players", player_choice game player)]
  | [{card; player_or_center}] -> page "pages/reveal_player.html"
                                    [("name", player_or_center)
                                    ; ("role", Role.to_string_plural card)]
  | [card1; card2] -> page "pages/seer_reveal_center.html"
                        [("role1", Role.to_string_plural card1.Action.card)
                        ; ("role2", Role.to_string_plural card2.Action.card)]
  | _ -> failwith "Invariant violated: Seer saw more than 2 cards"

let robber_page game player =
  match Player.swap player with
    None -> page "pages/robber.html" [("players", player_choice game player)]
  | Some (_, p2) -> page "pages/robber_reveal.html" [("role", Role.to_string_plural (name_to_role game p2))]

let troublemaker_page game player =
  match Player.swap player with
    None -> page "pages/troublemaker.html" [("players", player_choices game player)]
  | Some _ -> page "pages/wait.html" []

let no_action_page game player =
 (if Player.is_ready player
  then ()
  else player
       |> Player.add_action ~action:(Action.Ready)
       |> Game.set_player game
       |> update_game);
 page "pages/wait.html" []

let double_check_insomniac game player =
  let name = Player.name player in
  String.Map.for_all (Game.players game) ~f:(fun p ->
      match Player.swap p with
        None -> true
      | Some (p1, p2) -> not((p1 = name) || (p2 = name)))

let insomniac_page game player =
  (match Player.views player with
     [] -> insomniac_see_self game player
   |  _ -> ());
  if Role.equal (Player.evening_role player) (Player.morning_role player)
  then if double_check_insomniac game player
       then page "pages/insomniac_same.html" []
       else failwith "Game failed to make insomniac swap correctly."
  else page "pages/insomniac_change.html" [("role", Role.to_string_plural (Player.morning_role player))]

let all_werewolves game =
  String.Map.keys
    (String.Map.filter (Game.players game) ~f:(fun p ->
         Role.equal (Player.evening_role p) Role.Werewolf))


let minion_see_werewolves game player werewolves =
  let player = List.fold werewolves ~init:player ~f:(fun p werewolf ->
                   Player.add_action
                     p
                     ~action:(Action.View {card=Role.Werewolf;
                                           Action.player_or_center=werewolf})) in
  Game.set_player game player |> update_game

let minion_no_werewolves game player =
  player
  |> Player.add_action ~action:Action.ViewNoWerewolves
  |> Game.set_player game
  |> update_game


let minion_page game player =
  match all_werewolves game @ dream_wolves game with
    [] -> (match Player.views player with
             [] -> minion_no_werewolves game player
           | _ -> ());
          page "pages/minion_no_werewolves.html" []
  | werewolves -> (match Player.views player with
                     [] -> minion_see_werewolves game player werewolves
                   | _ -> ());
                  let ww_list = sprintf "<ul>%s</ul>" (String.concat ~sep:""
                                                         (List.map werewolves ~f:(fun s ->
                                                              sprintf "<li>%s</li>" s))) in
                  page "pages/minion_see_werewolves.html" [("names", ww_list)]

let night_page game player =
  if Player.is_ready player
  then page "pages/wait.html" []
  else
    match player.evening_role with
      Role.Werewolf -> werewolf_page game player
    | Role.MysticWolf -> mystic_wolf_page game player
    | Role.DreamWolf -> no_action_page game player
    | Role.Tanner -> no_action_page game player
    | Role.Minion -> minion_page game player
    | Role.Mason -> mason_page game player
    | Role.Seer -> seer_page game player
    | Role.ApprenticeSeer -> apprentice_seer_page game player
    | Role.Robber -> robber_page game player
    | Role.Troublemaker -> troublemaker_page game player
    | Role.Villager -> no_action_page game player
    | Role.Insomniac -> page "pages/wait.html" []
    | Role.Unassigned -> failwith "Invariant violation: A player was not assigned a role"

let morning_page game player =
  if Player.is_ready player
  then page "pages/wait.html" []
  else
    match player.evening_role with
      Role.Insomniac -> insomniac_page game player
    | _ -> page "pages/wait_morning.html" []

let action_list ?(third_person=false) player =
  let acts = List.filter_map (List.rev (Player.log player)) ~f:(function
                   Action.Ready -> None
                 | Action.VoteReady -> None
                 | Action.ReadyForSecondAction -> None
                 | action -> Some (Action.to_string
                                     ~third_person
                                     ~me:(Player.name player)
                                     action)) in
  if List.is_empty acts
  then " did nothing during the night."
  else sprintf "<ul>%s</ul>" (String.concat ~sep:""
                                (List.map acts ~f:(fun s ->
                                     sprintf "<li>%s</li>" s)))


let debate_page player =
  match Player.is_vote_ready player with
    false -> page "pages/debate.html" [("actions", action_list player)
                                     ; ("original", Role.to_string_plural (Player.evening_role player))]
  | true -> page "pages/wait.html" []

let vote_page game player =
  match Player.vote player with
    None -> page "pages/vote.html" [("players", player_choice ~keep_self:true game player)]
  | Some _ -> page "pages/wait.html" []

let results_page game player results =
  let%bind result_template = Reader.file_contents "pages/player_result.html" in
  let all_players = (String.Map.filter_map (Game.players game) ~f:(fun p ->
                         Option.some_if (not(Player.name p = Player.name player))
                           (Page.process result_template
                              [("player", Player.name p)
                              ; ("original", Role.to_string_plural (Player.evening_role p))
                              ; ("new", Role.to_string_plural (Player.morning_role p))
                              ; ("actions", action_list ~third_person:true p)])))
                    |> String.Map.data
                    |> String.concat ~sep:"\n"
  in
  let result = if Role.Team.equal (results.Game.winner) (Role.team (Player.morning_role player))
               then "won"
               else "lost"
  in
  let votes = String.Map.to_alist results.Game.votes
              |> List.map ~f:(fun (name, n) -> sprintf "%s: %i" name n)
              |> String.concat ~sep:"<br>"
  in
  page "pages/results.html" [("result", result)
                           ; ("winner", Role.Team.to_string results.Game.winner)
                           ; ("votes", votes)
                           ; ("original", Role.to_string_plural (Player.evening_role player))
                           ; ("new", Role.to_string_plural (Player.morning_role player))
                           ; ("player_results", all_players)
                           ; ("actions", action_list player)]


let current_page old_game player =
  let game = update_state old_game in
  game |> update_game;
  match game.state with
    Config -> config_page game player
  | Role -> role_page game player
  | Night ->  night_page game player
  | Morning -> morning_page game player
  | Debate -> debate_page player
  | Vote -> vote_page game player
  | Results r -> results_page game player r

let player_section game =
  sprintf "%s<hr><span id=\"playerNum\">%i</span> Players<br>"
    (display_players (Game.players game) ~wrap:(fun p ->
         "<span class='player'>" ^ p ^ "</span><br>"))
    (String.Map.length game.players)

let game_info game player =
  json (sprintf "{\"game\" : \"%s\", \"name\" : \"%s\", \"id\" : \"%s\"}"
          (Game.code game)
          (Player.name player)
          (Player.id player))

let join_game variables =
  printf "%s\n" (List.to_string ~f:(fun s -> s) (String.Map.keys variables));
  let opt = begin
      let open Option.Let_syntax in
      let%bind raw_code = String.Map.find variables "game" in
      let code = String.uppercase raw_code in
      let%bind game = String.Table.find games code in
      let%map name = String.Map.find variables "name" in
      (game, name)
    end
  in
  match opt with
  | Some (game, name) -> begin
      let player = Player.create name in
      let joined_game = Game.add_player ~exn:(fun s -> InvalidInput s) game player in
      joined_game |> update_game;
      game_info joined_game player
    end
  | None -> raise (InvalidInput "Game code or name is invalid or missing.")

let unique_game game =
  match String.Table.find games (Game.code game) with
    None -> true
  | Some _ -> false

let rec new_game variables =
  match String.Map.find variables "name" with
    None -> raise (InvalidInput "No username given.")
  | Some name -> let game = Game.create name in
                 if unique_game game
                 then (game |> add_game;
                       join_game (String.Map.set
                                    variables
                                    ~key:"game"
                                    ~data:game.code))
                 else new_game variables

let refresh variables =
  let game = game_exn variables ~error:"You're not currently in a game. Start one or join to get started!" in
  let player = player_exn variables game ~error:"You have not joined this game yet." in
  current_page game player

let update_config variables =
  let game = game_exn variables in
  let config_opt =
    let open Option in
    Config.empty
    |> Config.update ~role:Role.Werewolf ~count:(config_exn variables "werewolves")
    >>= Config.update ~role:Role.DreamWolf ~count:(config_exn variables "dreamwolves")
    >>= Config.update ~role:Role.MysticWolf ~count:(config_exn variables "mysticwolves")
    >>= (Config.update ~role:Role.Tanner ~count:(config_exn variables "tanners"))
    >>= (Config.update ~role:Role.Minion ~count:(config_exn variables "minions"))
    >>= (Config.update ~role:Role.Robber ~count:(config_exn variables "robbers"))
    >>= (Config.update ~role:Role.Troublemaker ~count:(config_exn variables "troublemakers"))
    >>= (Config.update ~role:Role.Seer ~count:(config_exn variables "seers"))
    >>= (Config.update ~role:Role.ApprenticeSeer ~count:(config_exn variables "apprenticeseers"))
    >>= (Config.update ~role:Role.Insomniac ~count:(config_exn variables "insomniacs"))
    >>= (Config.update ~role:Role.Mason ~count:(config_exn variables "masons"))
    >>= (Config.update ~role:Role.Villager ~count:(config_exn variables "villagers"))
  in
  match config_opt with
    None -> raise (InvalidInput "Config invalid: negative values or more than one troublemaker, robber, or seer")
  | Some config -> {game with config}

let start_game variables =
  try
    update_config variables
    |> Game.assign_roles
    |> update_game
  with _ -> raise (InvalidInput "Wrong number of roles for players.")

let begin_night variables =
  let game = game_exn variables in
  {game with state=Night} |> update_game

let players variables =
  let game = game_exn variables in
  html ~content:(player_section game)

let ready variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  player
  |> Player.add_action ~action:Action.Ready
  |> Game.set_player game
  |> update_game

let wolf_ready variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  let action =
    if Role.equal (Player.evening_role player) Role.MysticWolf
    then Action.ReadyForSecondAction
    else Action.Ready
  in
  player
  |> Player.add_action ~action
  |> Game.set_player game
  |> update_game

let rob variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  if Role.equal (Player.evening_role player) Role.Robber && Option.is_none (Player.swap player)
  then match String.Map.find variables "target" with
       | Some name -> if Game.is_player game name
                      then (player
                            |> Player.add_action ~action:(Action.Swap (player.name, name))
                            |> Player.add_action ~action:(Action.View {card=name_to_role game name
                                                                     ; player_or_center=player.name})
                            |> Game.set_player game
                            |> update_game)
                      else raise (InvalidInput "Target is not a valid player.")
       | None -> raise (InvalidInput "No target given for robber action.")
  else raise (InvalidInput "You cannot rob (or rob again).")

let troublemake variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  if Role.equal player.evening_role Role.Troublemaker && Option.is_none (Player.swap player)
  then match (String.Map.find variables "target1", String.Map.find variables "target2") with
         (Some name1, Some name2) -> if Game.is_player game name1 && Game.is_player game name2
                                     then (player
                                           |> Player.add_action ~action:(Action.Swap (name1, name2))
                                           |> Player.add_action ~action:(Action.Ready)
                                           |> Game.set_player game
                                           |> update_game)
                                     else raise (InvalidInput "Target is not a valid player.")
       | _ -> raise (InvalidInput "Invalid or missing targets for troublemaker action.")
  else raise (InvalidInput "You cannot troublemake (or re-troublemake).")

let see variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  if Role.equal player.evening_role Role.Seer && List.is_empty (Player.views player)
  then match String.Map.find variables "target" with
         Some "center" -> let (card1, card2) = Cards.draw2 game.unassigned in
                          (player
                           |> Player.add_action
                                ~action:(Action.View {card=card1
                                                    ; player_or_center="center"})
                           |> Player.add_action
                                ~action:(Action.View {card=card2
                                                    ; player_or_center="center"})
                           |> Game.set_player game
                           |> update_game)
       | Some name -> (match String.Map.find (Game.players game) name with
                         None -> raise (InvalidInput "Seer target is not a player or center card.")
                       | Some target_player -> (player
                                                |> Player.add_action
                                                     ~action:(Action.View {card=Player.evening_role target_player
                                                                         ; player_or_center=name})
                                                |> Game.set_player game
                                                |> update_game))
       | None -> raise (InvalidInput "Invalid or missing targets for seer action.")
  else raise (InvalidInput "You cannot see (or see twice).")

let mystic_see variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  if Role.equal player.evening_role Role.MysticWolf && Option.is_none (Player.mystic_wolf_view player)
  then match String.Map.find variables "target" with
         Some name -> (match String.Map.find (Game.players game) name with
                         None -> raise (InvalidInput "Mystic wolf target is not a player.")
                       | Some target_player -> (player
                                                |> Player.add_action
                                                     ~action:(Action.ViewAsMysticWolf
                                                                {card=Player.evening_role target_player
                                                                ; player_or_center=name})
                                                |> Game.set_player game
                                                |> update_game))
       | None -> raise (InvalidInput "Invalid or missing targets for mystic wolf action.")
  else raise (InvalidInput "You cannot see as a mystic wolf (or see twice as one).")

let leave variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  if Player.name player = Game.owner game
  then String.Table.remove games (Game.code game)
  else
    {game with players = String.Map.filter (Game.players game) ~f:(fun p ->
                             not(Player.name p = Player.name player))}
    |> update_game


let vote_ready variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  player
  |> Player.add_action ~action:Action.VoteReady
  |> Game.set_player game
  |> update_game

let vote variables =
  let game = game_exn variables in
  let player = player_exn variables game in
  match String.Map.find variables "vote" with
    None -> raise (InvalidInput "No vote selected.")
  | Some name ->
     let voteCast =
       if Game.is_player game name
       then Action.Player name
       else if name = "center"
       then Action.Center
       else raise (InvalidInput "Vote invalid. Vote for a player or no werewolf.")
     in
     player
     |> Player.add_action ~action:(Action.Vote voteCast)
     |> Game.set_player game
     |> update_game


let direct url variables =
  let parts = Str.split (Str.regexp "/+") url in
  printf "%s\n" (List.to_string parts ~f:(fun s -> s));
  let%bind maybe_page =
    try_with ~extract_exn:true (fun () ->
        match parts with
        | ["refresh"] -> refresh variables
        | ["newgame"] -> new_game variables
        | ["joingame"] -> join_game variables
        | ["savesettings"] -> update_config variables |> update_game; refresh variables
        | ["startgame"] -> start_game variables; refresh variables
        | ["beginnight"] -> begin_night variables; empty_response
        | ["favicon.ico"] -> favicon
        | ["images"; img] -> image img ~extension:"png"
        | ["players"] -> players variables
        | ["ready"] -> ready variables; empty_response
        | ["wolfready"] -> wolf_ready variables; empty_response
        | ["mysticsee"] -> mystic_see variables; empty_response
        | ["rob"] -> rob variables; empty_response
        | ["troublemake"] -> troublemake variables; empty_response
        | ["see"] -> see variables; empty_response
        | ["leavegame"] -> leave variables; empty_response
        | ["voteready"] -> vote_ready variables; empty_response
        | ["vote"] -> vote variables; empty_response
        | [] -> index ()
        | _ -> not_found)
  in
  match maybe_page with
    Ok page -> return page
  | Error (InvalidInput s) -> (printf "Error: %s \n" s; page "pages/start.html" [("error", s)])
  | Error e -> (printf "%s\n" (Exn.to_string e)); not_found

let run port () =
  let%bind server =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
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
  Command.run (Command.async
                 ~summary:"The One Night Werewolf server"
                 (Command.Spec.map
                    (Command.Param.flag "port"
                       ~doc:"port"
                       (Command.Param.required Command.Param.int)) ~f:run))
