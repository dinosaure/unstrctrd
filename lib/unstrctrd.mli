type elt =
  [ `Uchar of Uchar.t
  | `WSP of wsp
  | `LF
  | `CR
  | `FWS of wsp
  | `d0
  | `OBS_NO_WS_CTL of obs ]
and wsp = private string
and obs = private char

type t = private elt list

type error = [ `Msg of string ]

val empty : t

val of_string : string -> (int * t, [> error ]) result
val of_list : elt list -> (t, [> error ]) result
val to_utf_8_string : t -> string

val iter : f:(elt -> unit) -> t -> unit
val fold : f:('a -> elt -> 'a) -> 'a -> t -> 'a
val map : f:(elt -> elt) -> t -> t

val wsp : len:int -> elt
val tab : len:int -> elt
val fws : ?tab:bool -> int -> elt

val without_comments : t -> (t, [> error ]) result
val split_at : index:int -> t -> t * t
val split_on : on:[ `WSP | `FWS | `Uchar of Uchar.t | `Char of char | `LF | `CR ] -> t -> t * t
