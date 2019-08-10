open Core

type t = {
    werewolf : int
  ; seer : bool
  ; robber : bool
  ; villager : int
  ; troublemaker : bool
  ; mason : int
  ; insomniac : int
  }

let empty = {
    werewolf=0
  ; seer=false
  ; robber=false
  ; villager=0
  ; troublemaker=false
  ; mason=0
  ; insomniac=0
  }

let to_list_dups {werewolf; seer; robber; villager; troublemaker; mason; insomniac} =
  List.concat [List.init werewolf ~f:(fun _ -> Role.Werewolf)
             ; if seer then [Role.Seer] else []
             ; if robber then [Role.Robber] else []
             ; List.init villager ~f:(fun _ -> Role.Villager)
             ; if troublemaker then [Role.Troublemaker] else []
             ; List.init mason ~f:(fun _ -> Role.Mason)
             ; List.init insomniac ~f:(fun _ -> Role.Insomniac)]

let to_alist {werewolf; seer; robber; villager; troublemaker; mason; insomniac} =
  [(Role.Werewolf, werewolf)
  ; (Role.Seer, if seer then 1 else 0)
  ; (Role.Robber, if robber then 1 else 0)
  ; (Role.Villager, villager)
  ; (Role.Troublemaker, if troublemaker then 1 else 0)
  ; (Role.Mason, mason)
  ; (Role.Insomniac, insomniac)]

let count config =
  List.fold (to_alist config) ~init:0 ~f:(fun acc (_, count) -> acc + count)

let update config ~role ~count =
  let valid_bool n = n = 1 || n = 0 in
  if count < 0 then None else
    match role with
      Role.Werewolf -> Some {config with werewolf=count}
    | Role.Seer -> Option.some_if (valid_bool count) {config with seer=(count = 1)}
    | Role.Robber -> Option.some_if (valid_bool count) {config with robber=(count = 1)}
    | Role.Villager -> Some {config with villager=count}
    | Role.Troublemaker -> Option.some_if (valid_bool count) {config with troublemaker=(count = 1)}
    | Role.Mason -> Some {config with mason=count}
    | Role.Insomniac -> Some {config with insomniac=count}
    | Role.Unassigned -> None (* can't unassign a role *)
