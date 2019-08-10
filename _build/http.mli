type http_method = Get of string Core.String.Map.t
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
    headers : string Core.String.Map.t
  }

val parse_request : Async.Reader.t -> request option Async.Deferred.t

val response : status -> content:string -> content_type:string -> string
