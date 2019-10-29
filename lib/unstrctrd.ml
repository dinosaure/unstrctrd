type elt =
  [ `Uchar of Uchar.t
  | `WSP of wsp 
  | `LF
  | `CR
  | `FWS of wsp
  | `d0
  | `OBS_NO_WS_CTL of obs ]
and wsp = string
and obs = char

type t = elt list

type error = [ `Msg of string ]

let error_msgf fmt = Fmt.kstrf (fun err -> Error (`Msg err)) fmt

let empty = []
let length = List.length

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

let iter ~f l = List.iter f l
let fold ~f a l = List.fold_left f a l
let map ~f l = List.map f l

let wsp ~len = `WSP (String.make len ' ')
let tab ~len = `WSP (String.make len '\t')

let fws ?(tab = false) indent =
  if indent <= 0 then Fmt.invalid_arg "fws: invalid indent argument" ;
  if tab then `FWS (String.make indent '\t') else `FWS (String.make indent ' ')

let split_at ~index l =
  if index < 0 || index > List.length l then Fmt.invalid_arg "split_at: index (%d) is invalid" index ;

  let rec go n l r = match n with
    | 0 -> List.rev l, r
    | n -> match r with
      | [] -> assert false | x :: r -> go (pred n) (x :: l) r in
  go index [] l

let split_on ~on l =
  let rec go l r = match r, on with
    | [], _ -> None
    | `CR    :: r, `CR
    | `LF    :: r, `LF
    | `WSP _ :: r, `WSP
    | `FWS _ :: r, `FWS
    | `d0    :: r, `Char '\000' ->
      Some (List.rev l, r)
    | `Uchar a :: r, `Uchar b
      when Uchar.equal a b ->
      Some (List.rev l, r)
    | `Uchar a :: r, `Char b
      when Uchar.equal a (Uchar.of_char b) ->
      Some (List.rev l, r)
    | `OBS_NO_WS_CTL a :: r, `Char b
      when Char.equal a b ->
      Some (List.rev l, r)
    | x :: r, _ -> go (x :: l) r in
  go [] l

let of_list l =
  let has_cr = ref false in
  let exception Break in
  let f = function
    | `LF -> if !has_cr then raise_notrace Break ; has_cr := false
    | `CR -> has_cr := true
    | _   -> has_cr := false in
  try List.iter f l ; Ok l
  with Break -> error_msgf "of_list: An unexpected CRLF token exists"

let fold_fws t =
  let folder (fws, acc) = function
    | `FWS wsp -> if fws then (fws, acc) else (true, `WSP wsp :: acc)
    | x -> (false, x :: acc) in
  List.fold_left folder (false, []) t |> fun (_, t) -> List.rev t

module type BUFFER = Lexer.BUFFER
module type MONAD = Lexer.MONAD
module Make = Lexer.Make

let post_process = Pp.pp
