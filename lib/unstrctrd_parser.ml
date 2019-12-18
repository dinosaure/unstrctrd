module Monad = struct
  type 'a t =
    | Done of 'a
    | Read of { buffer : bytes; continue : int -> 'a t }
    | Fail of string

  type buffer = bytes

  let return x = Done x
  let rec bind x f = match x with
    | Done v -> f v
    | Read { buffer; continue; } ->
      let continue n = bind (continue n) f in
      Read { buffer; continue; }
    | Fail err -> Fail err
  let fail err = Fail err
  let read continue buffer = Read { buffer; continue; }
end

let unstrctrd buf =
  let module Buffer = struct
    type t = bytes
    let blit_to_bytes = Bytes.blit
    let buf = buf
  end in
  let module State = Unstrctrd.Make(Buffer)(Monad) in
  let open Angstrom in
  let lexbuf = Unstrctrd.lexbuf_make () in
  scan (State.unstructured [] lexbuf)
    (fun state chr -> match state with
       | Monad.Done _ -> None
       | Monad.Fail _ -> None
       | Monad.Read { buffer; continue; } ->
         Bytes.set buffer 0 chr ; match continue 1 with
         | state -> Some state
         | exception Failure _ -> None)
  >>= function
  | (_, Monad.Done v) -> Unstrctrd.post_process return v
  | (_, Monad.Fail err) -> fail err
  | (_, Monad.Read { continue; _ }) -> match continue 0 with
    | Monad.Done v -> Unstrctrd.post_process return v
    | Monad.Fail err -> fail err
    | Monad.Read _ -> assert false
    | exception Failure _ -> fail "Invalid unstructured form"

let read
  : Lexing.lexbuf -> (int -> 'a Angstrom.t) -> bytes -> 'a Angstrom.t
  = fun lexbuf continue buffer ->
  let open Angstrom in
  pos >>= fun curr_pos ->
  advance (lexbuf.Lexing.lex_curr_pos - curr_pos) *> commit >>= fun () ->
  fix @@ fun m -> available >>= function
  | 0 ->
    ( peek_char >>= function
      | Some _ -> m
      | None -> try continue 0 with Failure _ -> fail "Invalid unstructured form" )
  | n ->
    peek_string n >>= fun input ->
    let len = min n (Bytes.length buffer) in
    Bytes.blit_string input 0 buffer 0 len ;
    continue len

let fast_unstrctrd buf =
  let lexbuf = Unstrctrd.lexbuf_make () in
  let read continue buffer = read lexbuf continue buffer in
  let module Buffer = struct
    type t = bytes
    let blit_to_bytes = Bytes.blit
    let buf = buf
  end in
  let module Monad = struct
    type 'a t = 'a Angstrom.t
    type buffer = bytes

    let return = Angstrom.return
    let bind x f =
      Angstrom.(x >>= fun x -> try f x with Failure _ -> Angstrom.fail "Invalid unstructured form")
    let fail = Angstrom.fail
    let read = read
  end in
  let module State = Unstrctrd.Make(Buffer)(Monad) in
  let open Angstrom in
  let trailer v =
    pos >>= fun curr_pos ->
    advance (lexbuf.Lexing.lex_curr_pos - curr_pos) *> return v in

  State.unstructured [] lexbuf >>= trailer >>= Unstrctrd.post_process return
