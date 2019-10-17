module Buffer = struct
  type t = bytes

  let length = Bytes.length
  let blit_to_bytes = Bytes.blit
  let buf = Bytes.create 4096
end

module Monad = struct
  type 'a t = 'a
  type buffer = bytes

  let return x = x
  let bind x f = f x
  let fail err = failwith err
  let read buf len = input stdin buf 0 len
end

module Lexer = Lexer.Make(Buffer)(Monad)

let lexbuf = Lexer.make ()

let pp_token ppf = function
  | `OBS_UTEXT x -> Fmt.pf ppf "(`OBS_UTEXT %S)" x
  | `FWS fws -> Fmt.pf ppf "(`FWS %S)" fws
  | `VCHAR x -> Fmt.pf ppf "(`VCHAR %S)" x
  | `WSP wsp -> Fmt.pf ppf "(`WSP %S)" wsp

let () =
  let res = Lexer.unstructured [] lexbuf in
  Fmt.pr "> @[<hov>%a@]\n%!" Fmt.(Dump.list pp_token) res
