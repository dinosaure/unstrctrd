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
  let lexer = State.make () in
  scan (State.unstructured [] lexer)
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
