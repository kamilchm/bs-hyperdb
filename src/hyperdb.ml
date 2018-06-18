open Belt

type t

type storage

type key = string

type error = string

module Js_rai = struct
  external make : string -> storage = "random-access-idb" [@@bs.new] [@@bs.module]
end

module Js_hyperdb = struct
  type node = {
    clock : int array;
    deleted : bool;
    feed : int;
    key: key;
    seq : string;
    value : Js.Json.t;
  } [@@bs.deriving abstract]

  type options = {
    valueEncoding: string;
  } [@@bs.deriving abstract]

  external make : storage -> options -> t = "hyperdb" [@@bs.new] [@@bs.module]
  external put : t -> string -> 'a -> ('e Js.nullable -> unit [@bs]) -> unit = "" [@@bs.send]
  external get : t -> string -> ('a Js.nullable -> node array -> unit [@bs]) -> unit = "" [@@bs.send]
  external list : t -> string -> ('a Js.nullable -> node array array -> unit [@bs]) -> unit = "" [@@bs.send]
end

let make name =
  let open Js_hyperdb in 
  Js_rai.make name
  |. make (options ~valueEncoding:"json")

let put key value ~callback hdb =
  Js_hyperdb.put hdb key value (fun [@bs] err ->
    match Js.Null_undefined.toOption err with
    | Some e -> callback (Result.Error {j|Error while trying to put value=$value: $e|j})
    | _ -> callback (Result.Ok ())
  )

let get key ~callback hdb =
  Js_hyperdb.get hdb key (fun [@bs] err nodes ->
    match Js.Null_undefined.toOption err with
    | Some e -> callback (Result.Error {j|Error while trying to get value: $e|j})
    | _ -> callback (Result.Ok (Array.getExn nodes 0 |> Js_hyperdb.value))
  )

let list key ~callback hdb =
  Js_hyperdb.list hdb key (fun [@bs] err nodes ->
    match Js.Null_undefined.toOption err with
    | Some e -> callback (Result.Error {j|Error while trying to list values: $e|j})
    | _ -> callback (Result.Ok (Array.map nodes (fun n -> Array.getExn n 0 |> Js_hyperdb.value)))
  )
