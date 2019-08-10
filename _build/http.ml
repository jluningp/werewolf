open Core
open Async

type http_method = Get of string String.Map.t
                 | Post
                 | Put
                 | Head
                 | Delete
                 | Patch
                 | Options

type status = Ok | BadRequest | NotFound

type request = {
    http_method : http_method;
    version : string;
    uri : string;
    headers : string String.Map.t
  }

let method_of_string = function
  | "POST" -> Post
  | "PUT" -> Put
  | "HEAD" -> Head
  | "DELETE" -> Delete
  | "PATCH" -> Patch
  | "OPTIONS" -> Options
  | _ -> failwith "Impossible"

let process_variables variables =
  let pairs = Str.split (Str.regexp "&") variables in
  List.fold pairs ~init:String.Map.empty
    ~f:(fun acc pair -> match Str.split (Str.regexp "=") pair with
                        | [var; value] -> (match String.Map.add acc ~key:var ~data:value with
                                             `Ok map -> map
                                           | `Duplicate -> printf "%s duplicate var" pair; acc)
                        | _ -> printf "%s invalid get var" pair; acc)

let process_url unsplit_url =
  let parts = Str.split (Str.regexp "?") unsplit_url in
  printf "[%s] "  (List.fold ~init:"" ~f:(fun s1 s2 -> if s1 = "" then s2 else s1 ^ "," ^ s2) parts);
  match parts with
    [url] -> (url, String.Map.empty)
  | [url; variables] -> (url, process_variables variables)
  | url::_::_ -> printf "Many ? in url: %s" url; (url, process_variables (List.last_exn parts))
  | [] -> printf "Invalid url?? %s" unsplit_url; (unsplit_url, String.Map.empty)


let first_line request =
  let%map first = Reader.read_line request in
  match first with
  | `Eof -> None
  | `Ok line -> match Str.split (Str.regexp "[ \t]+") line with
                | ["GET"; url; http] -> let (url, variables) = process_url url in
                                        Some {http_method=Get variables;
                                              version=http;
                                              uri=url;
                                              headers=String.Map.empty}
                | [("POST"
                    | "PUT"
                   | "HEAD"
                   | "DELETE"
                   | "PATCH"
                   | "OPTIONS" as meth); url; http]-> printf "Method other than GET not supported %s" meth;
                                                      Some {http_method=method_of_string meth;
                                                            version=http;
                                                            uri=url;
                                                            headers=String.Map.empty}
                | _ -> printf "Invalid request\n"; None

let rec headers request =
  match%bind Reader.read_line request with
    `Eof -> return String.Map.empty
  | `Ok line -> match Str.split (Str.regexp ":") line with
                  name::values -> let%map h = headers request in
                                  let value = String.concat ~sep:"" values in
                                  (match String.Map.add h ~key:name ~data:value with
                                     `Ok map -> map
                                   | `Duplicate -> printf "Duplicate header %s %s" name value; h)
                | [] -> return String.Map.empty

let parse_request request =
  match%bind first_line request with
    Some result -> let%map h = headers request in
                   Some {result with headers=h}
  | None -> return None

let status_to_string = function
    Ok -> "200 OK"
  | NotFound -> "404 Not Found"
  | BadRequest -> "400 Bad Request"

let response status ~content ~content_type =
  let len = String.length content in
  (sprintf "HTTP/1.1 %s\r\n" (status_to_string status))
  ^ (sprintf "Content-Length: %i\r\n" len)
  ^ (sprintf "Content-Type: %s\r\n" content_type)
  ^ "Connection: close\r\n\r\n"
  ^ content
