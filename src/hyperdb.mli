open Belt

type t

type key = string

type error = string

val make : string -> t

val put : key -> 'a -> callback:((unit, error) Result.t -> unit) -> t -> unit

val get : key -> callback:((Js.Json.t, error) Result.t -> unit) -> t -> unit

val list : key -> callback:((Js.Json.t array, error) Result.t -> unit) -> t -> unit
