open Core
open Async

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

type night_moves = {
    mutable robber : (string * string) option;
    mutable werewolf : int;
    mutable seer : int * int * (string option);
    mutable troublemaker : (string * string) option;
  }

type state = Config
           | Role
           | Night of night_moves
           | Morning
           | Debate


type player = {
    name : string;
    id : string;
    original_role : string;
    mutable ready : bool;
    mutable new_role : string
  }

type game_info = {
    code : string;
    owner : string;
    players : player String.Table.t;
    roles : int String.Table.t;
    mutable unassigned : string * string * string;
    mutable stage : state;
  }

let games : game_info String.Table.t = String.Table.create ~size:5 ()

let gen_code n =
  let a = int_of_char 'A' in
  List.init n ~f:(fun _ -> char_of_int (a + Random.int 26))
  |> String.of_char_list

let assign_roles game =
  let roles = String.Table.fold game.roles ~init:[] ~f:(fun ~key ~data acc ->
                  List.init data ~f:(fun _ -> key) @ acc) in
  match List.permute roles with
    r1::r2::r3::rest -> game.unassigned <- (r1, r2, r3);
                        game.stage <- Role;
                        let pairs = List.zip_exn (String.Table.keys game.players) rest in
                        printf "[%s]" (String.concat ~sep:"," (List.map ~f:(fun (name, role) -> name ^ ":" ^ role) pairs));
                        List.iter pairs ~f:(fun (name, role) ->
                            String.Table.update game.players name ~f:(function None -> failwith "Invalid player"
                                                                             | Some player ->
                                                                                {player with
                                                                                 original_role=role;
                                                                                 new_role=role}))
  | _ -> failwith "invalid game"


let make_moves game (moves : night_moves) =
  let find = String.Table.find game.players in
  let open Option.Let_syntax in
  begin
    let%bind (r1, r2) = moves.robber in
    let%bind p1 = find r1 in
    let%map p2 = find r2 in
    p1.new_role <- p2.original_role;
    p2.new_role <- p1.original_role
  end |> ignore;
  begin
    let%bind (t1, t2) = moves.troublemaker in
    let%bind p1 = find t1 in
    let%map p2 = find t2 in
    let p1_role = p1.new_role in
    p1.new_role <- p2.new_role;
    p2.new_role <- p1_role
  end |> ignore

let get_game code = String.Table.find games code

let get_game_exn code = String.Table.find_exn games code

let player_names game = String.Table.keys game.players

let get_player_exn game name = String.Table.find_exn game name

let print_url parts =
  printf "[%s]\n "  (List.fold ~init:"" ~f:(fun s1 s2 -> if s1 = "" then s2 else s1 ^ "," ^ s2) parts)

let not_found =
  let%map page = Reader.file_contents "pages/not_found.html" in
  Http.response Http.NotFound ~content:page ~content_type:"text/html"

let print_players game =
  sprintf "[%s] " (String.concat ~sep:","
                     (List.map (String.Table.keys game.players) ~f:(fun s ->
                          "\"" ^ s ^ "\"")))

let roles_list code =
  List.map (String.Table.to_alist (get_game_exn code).roles) ~f:(fun (x, y) -> (x, Int.to_string y))

let html name replacements =
  let%map template = Reader.file_contents name in
  let content = Page.process template replacements in
  Http.response Http.Ok ~content:content ~content_type:"text/html"

let index () =
  let%map page = Reader.file_contents "pages/index.html" in
  Http.response Http.Ok ~content:page ~content_type:"text/html"

let role_number roles =
  String.Table.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data) roles

let new_player name =
    {name;
     id = gen_code 8;
     original_role = "";
     ready = false;
     new_role = ""}

let add_player game name =
  let player = new_player name in
  match String.Table.add game.players ~key:name ~data:player with
    `Duplicate -> Error "Duplicate name."
  | `Ok -> Ok (game, player)

let join_game game name =
  match game.stage with
    Config -> add_player game name
  | _ -> Error "Game already in progress"

let favicon =
  let%map image = Reader.file_contents "images/favicon.jpg" in
  Http.response Http.Ok ~content:image ~content_type:"image/jpg"

let users code =
  match String.Table.find games code with
    None -> Http.response Http.Ok ~content:"[Invalid Game Code]" ~content_type:"text/html"
  | Some game -> Http.response Http.Ok ~content:(print_players game) ~content_type:"text/html"

let text_response text = return (Http.response Http.Ok ~content:text ~content_type:"text/html")

let empty_response = text_response ""

let print_roles roles =
  String.concat ~sep:"," (List.map ~f:(fun (key, value) -> sprintf "(%s, %i)" key value) (String.Table.to_alist roles))

let get_user game (name, id) =
  Option.bind (String.Table.find game.players name) ~f:(fun player ->
      Option.some_if (id = player.id) player)

let request_player ?(user_error="") game variables ~f =
  let find = String.Map.find variables in
  let info = Option.both (find "name") (find "id") in
  match Option.bind info ~f:(get_user game) with
  | Some player -> f player
  | None -> html "pages/start.html" [("error", user_error)]

let request_game_and_player ?(game_error="") ?(user_error="") variables ~f  =
  let find = String.Map.find variables in
  match Option.bind (find "game") ~f:get_game with
    None -> html "pages/start.html" [("error", game_error)]
  | Some game -> request_player game variables ~user_error ~f:(fun player ->
                     f game player)

let request_game ?(game_error="") variables ~f  =
  let find = String.Map.find variables in
  match Option.bind (find "game") ~f:get_game with
    None -> html "pages/start.html" [("error", game_error)]
  | Some game -> f game

let request_name ?(user_error="") variables ~f =
  match String.Map.find variables "name" with
    None -> html "pages/start.html" [("error", user_error)]
  | Some name -> f name

let display_player_list players wrap =
  players
  |> String.Table.keys
  |> List.map ~f:wrap
  |> String.concat ~sep:""


let display_players game wrap =
  game.players
  |> String.Table.keys
  |> List.map ~f:wrap
  |> String.concat ~sep:""

let config_info game =
  let player_count = String.Table.length game.players in
  let role_count = role_number game.roles in
  [("game", game.code);
   ("players", display_players game (fun p -> "<span class='player'>" ^ p ^ "</span><br>"));
   ("player_number", Int.to_string player_count);
   ("rolenum", Int.to_string role_count);
   ("roleneeded", Int.to_string (player_count + 3 - role_count))]
  @ roles_list game.code

let other_werewolves game player =
  String.Table.keys (String.Table.filter game.players ~f:(fun p -> p.original_role = "werewolves" && p.name <> player.name))

let other_masons game player =
  String.Table.keys (String.Table.filter game.players ~f:(fun p -> p.original_role = "masons" && p.name <> player.name))

let player_choice game player =
  display_player_list (String.Table.filter game.players ~f:(fun p -> p.name <> player.name))
    (fun p -> "<input type='radio' id='" ^ p ^ "'>" ^ p ^ "<br>")

let player_choices game player =
  display_player_list (String.Table.filter game.players ~f:(fun p -> p.name <> player.name))
    (fun p -> "<input type='checkbox' id='" ^ p ^ "'>" ^ p ^ "<br>")

let ready _game player =
  player.ready <- true

let center_card (c1, c2, c3) index =
  match index with
    0 -> c1
  | 1 -> c2
  | _ -> c3

let current_page game player =
  printf "refreshing now for %s\n" player.name;
  let players = String.Table.filter game.players ~f:(fun p -> not(p.ready)) in
  printf "Players not ready: %s\n" (List.to_string ~f:(fun x -> x) (String.Table.keys players));
  if String.Table.length players = 0
  then (match game.stage with
          Night moves -> (make_moves game moves;
                          game.stage <- Morning)
        | Morning -> game.stage <- Debate
        | _ -> ())
  else (match game.stage with
          Night moves -> if String.Table.for_all players ~f:(fun p -> p.original_role = "insomniacs")
                         then (make_moves game moves; game.stage <- Morning)
                         else ()
        | _ -> ());
  match game.stage with
    Config -> if player.name = game.owner
              then (printf "Config!\n"; html "pages/config.html" (config_info game))
              else (printf "Config View!\n"; html "pages/config_view.html" (config_info game))
  | Role -> html "pages/role.html" [("role", player.original_role);
                                    ("button", if player.name = game.owner
                                               then "<button onclick='beginNight()' id='night'>Begin Night!</button>"
                                               else "<input type='hidden' id='view_role'>")]
  | Night moves -> if player.ready
                   then html "pages/wait.html" []
                   else (match player.original_role with
                           "werewolves" -> (match other_werewolves game player with
                                              [] -> html "pages/lone_werewolf.html"
                                                      [("card", center_card game.unassigned (moves.werewolf))]
                                            | wolves -> html "pages/werewolves.html"
                                                          [("names", String.concat ~sep:"<br>" wolves)])
                         | "masons" -> (match other_masons game player with
                                          [] -> html "pages/lone_mason.html" []
                                        | masons -> html "pages/masons.html"
                                                      [("names", String.concat ~sep:"<br>" masons)])
                         | "seers" -> (match moves.seer with
                                       | (_, _, None) -> html "pages/seer.html"
                                                           [("players", player_choice game player)]
                                       | (c1, c2, Some "center") -> html "pages/seer_reveal_center.html"
                                                                      [("role1", center_card game.unassigned c1);
                                                                       ("role2", center_card game.unassigned c2)]
                                       | (_, _, Some player) -> html "pages/seer_reveal_player.html"
                                                                  [("name", player);
                                                                   ("role", (get_player_exn game.players player).original_role)])
                         | "robbers" -> (match moves.robber with
                                           None -> html "pages/robber.html" [("players", player_choice game player)]
                                         | Some (_, p2) -> html "pages/robber_reveal.html"
                                                             [("role", (get_player_exn game.players p2).original_role)])
                         | "troublemakers" -> (match moves.troublemaker with
                                               | None -> html "pages/troublemaker.html" [("players", player_choices game player)]
                                               | Some _ -> html "pages/wait.html" [])
                         | "villagers" -> ready game player; html "pages/wait.html" []
                         | _ -> html "pages/wait.html" [])
  | Morning -> if player.ready
               then html "pages/wait.html" []
               else
                 (match player.original_role with
                    "insomniacs" -> if player.original_role = player.new_role
                                    then html "pages/insomniac_same.html" []
                                    else html "pages/insomniac_change.html" [("role", player.new_role)]
                  | _ -> html "pages/wait.html" [])
  | Debate -> html "pages/debate.html" []


let rob game player name =
  match game.stage with
    Night moves -> moves.robber <- Some (player.name, name)
  | _ -> ()

let troublemake game player name1 name2 =
  printf "well we got here\n";
  match game.stage with
    Night moves -> moves.troublemaker <- Some (name1, name2);
                   ready game player
  | _ -> printf "it's not night??"; ()

let see game name =
  match game.stage with
    Night moves -> let (c1, c2, _) = moves.seer in
                   moves.seer <- (c1, c2, Some name)
  | _ -> ()

let new_role_table () =
  let roles = [("werewolves", 0);
               ("robbers", 0);
               ("troublemakers", 0);
               ("seers", 0);
               ("insomniacs", 0);
               ("masons", 0);
               ("villagers", 0)]
  in String.Table.of_alist_exn roles

let new_player_table () = String.Table.create ~size:10 ()

let rec create_game owner =
  let code = gen_code 4 in
  let game = String.Table.add
               games
               ~key:code
               ~data:{code;
                      owner;
                      unassigned=("","","");
                      stage=Config;
                      players=new_player_table ();
                      roles=new_role_table ()}
  in
  match game with
    `Duplicate -> create_game owner
  | `Ok -> code


let new_game variables =
  let find = String.Map.find variables in
  match find "name" with
    None -> Error "Please enter a username."
  | Some name -> let code = create_game name in
                 let game = get_game_exn code in
                 join_game game name

let error message = html "pages/start.html" [("error", message)]

let please_refresh = Http.response
                       Http.Ok
                       ~content:"<h3>Please refresh the page.</h3>"
                       ~content_type:"text/html"

let respond_json content =
    Http.response
      Http.Ok
      ~content
      ~content_type:"text/json"


let load_game game_or_error =
  match game_or_error with
    Ok (game, player) -> return (respond_json (sprintf "{\"game\" : \"%s\", \"name\" : \"%s\", \"id\" : \"%s\"}"
                                                 game.code
                                                 player.name
                                                 player.id))
  | Error message -> error message


let start_game game =
  game.stage <- Role;
  assign_roles game

let update_roles variables =
  (let find_int str = try Option.map (String.Map.find variables str) ~f:Int.of_string with
                       Failure _ -> None
   in
   let open Option.Let_syntax in
   let%bind code = String.Map.find variables "game" in
   let%bind game = get_game code in
   let%bind w = find_int "werewolves" in
   let%bind r = find_int "robbers" in
   let%bind t = find_int "troublemakers" in
   let%bind s = find_int "seers" in
   let%bind i = find_int "insomniacs" in
   let%bind m = find_int "masons" in
   let%map v = find_int "villagers" in
   let update key data = String.Table.set game.roles ~key ~data in
   update "werewolves" w;
   update "robbers" r;
   update "troublemakers" t;
   update "seers" s;
   update "insomniacs" i;
   update "masons" m;
   update "villagers" v) |> ignore

let player_section game =
  sprintf "%s<br><span id=\"playerNum\">%i</span> Players<br>"
    (display_players game (fun p -> "<span class='player'>" ^ p ^ "</span><br>"))
    (String.Table.length game.players)

let rec two_distinct () =
  let r1 = Random.int 3 in
  let r2 = Random.int 3 in
  if r1 = r2 then two_distinct () else (r1, r2)

let begin_night game =
  let (r1, r2) = two_distinct () in
  game.stage <- Night {robber=None; werewolf=Random.int 2; seer=(r1,r2, None); troublemaker=None}


let direct url variables =
  let v = variables in
  let parts = Str.split (Str.regexp "/+") url in
  print_url parts;
  match parts with
  | ["refresh"] -> request_game_and_player v ~user_error:"You have not joined this game yet." ~f:current_page
  | ["newgame"] -> new_game v |> load_game
  | ["joingame"] -> request_game v ~f:(fun game -> request_name v ~f:(fun name -> join_game game name |> load_game))
  | ["savesettings"] -> update_roles v; request_game_and_player v ~f:current_page
  | ["startgame"] -> update_roles v; request_game_and_player v ~f:(fun game player -> start_game game;
                                                                                      current_page game player)
  | ["beginnight"] -> request_game v ~f:(fun game -> begin_night game; empty_response)
  | ["favicon.ico"] -> printf "favicon!\n"; favicon
  | ["players"] -> request_game v ~f:(fun game -> return (Http.response Http.Ok
                                                            ~content:(player_section game)
                                                            ~content_type:"text/html"))
  | ["ready"] -> request_game_and_player v ~f:(fun game player -> ready game player; empty_response)
  | ["rob"] -> request_game_and_player v ~f:(fun game player -> if player.original_role = "robbers"
                                                                then match String.Map.find v "target" with
                                                                       None -> empty_response
                                                                     | Some t -> rob game player t; empty_response
                                                                else empty_response)
  | ["troublemake"] -> request_game_and_player v ~f:(fun game player -> if player.original_role = "troublemakers"
                                                                        then (match (String.Map.find v "target1", String.Map.find v "target2") with
                                                                                (Some t1, Some t2) -> troublemake game player t1 t2; empty_response
                                                                              | _ -> printf "can't find them??\n"; empty_response)
                                                                        else empty_response)
  | ["see"] -> request_game_and_player v ~f:(fun game player -> if player.original_role = "seers"
                                                                then (match String.Map.find v "target" with
                                                                        None -> empty_response
                                                                      | Some t -> see game t; empty_response)
                                                                else empty_response)
  | [] -> printf "index!\n"; index ()
  | _ -> printf "not found!\n"; not_found

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
