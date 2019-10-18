type elt =
  [ `Uchar of Uchar.t
  | `WSP of string
  | `LF of int
  | `CR of int
  | `FWS of string
  | `CRLF
  | `d0
  | `OBS_NO_WS_CTL of char ]

type t = elt list

let error_msgf fmt = Fmt.kstrf (fun err -> Error (`Msg err)) fmt

let of_string str =
  let module B = struct
    type t = bytes

    let length = Bytes.length
    let blit_to_bytes = Bytes.blit
    let buf = Bytes.create 4096
  end in
  let module M = struct
    type 'a t =
      | Read of { buffer : Bytes.t; length : int; continue : int -> 'a t }
      | Fail of string
      | Done of 'a

    type buffer = bytes

    let return x = Done x

    let rec bind : 'a t -> ('a -> 'b t) -> 'b t = fun x f -> match x with
      | Read { buffer; length; continue; } ->
        let continue len = bind (continue len) f in
        Read { buffer; length; continue; }
      | Fail err -> Fail err
      | Done x -> f x

    let fail err = Fail err
    let read k buf len = Read { buffer= buf; length= len; continue= k; }
  end in
  let module Lexer = Lexer.Make(B)(M) in
  let lexbuf = Lexer.make () in
  let pos = ref 0 in
  let rec go = function
    | M.Done lst ->
      let k res = Ok (lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos, res) in
      Pp.pp k lst
    | M.Fail err -> Error (`Msg err)
    | M.Read { buffer; length; continue; } ->
      let len = min (String.length str - !pos) length in
      Bytes.blit_string str !pos buffer 0 len ; pos := !pos + len ;
      go (continue len) in
  go (Lexer.unstructured [] lexbuf)

let to_utf_8_string lst =
  let buf = Buffer.create (List.length lst) in
  let iter = function
    | `d0 -> Buffer.add_char buf '\000'
    | `WSP wsp -> Buffer.add_string buf wsp
    | `OBS_NO_WS_CTL chr -> Buffer.add_char buf chr
    | `Uchar uchar -> Uutf.Buffer.add_utf_8 buf uchar
    | `CRLF -> Buffer.add_string buf "\r\n"
    | `FWS wsp -> Buffer.add_string buf "\r\n" ; Buffer.add_string buf wsp
    | `LF -> Buffer.add_char buf '\n'
    | `CR -> Buffer.add_char buf '\r' in
  List.iter iter lst ; Buffer.contents buf

let without_comments lst =
  let rec go stack acc = function
    | [] -> if stack = 0 then Ok (List.rev acc) else error_msgf "Non-terminating comment"
    | `Uchar uchar as value :: r ->
      ( match Uchar.to_int uchar with
        | 0x28 (* '(' *) -> go (succ stack) acc r
        | 0x29 (* ')' *) -> go (pred stack) acc r
        | _ -> if stack > 0 then go stack acc r else go stack (value :: acc) r )
    | value :: r ->
      if stack > 0 then go stack acc r else go stack (value :: acc) r in
  go 0 [] lst
