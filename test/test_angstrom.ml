let () = Printexc.record_backtrace true
let ( <.> ) f g = fun x -> f (g x)

let rest_of_input ~committed ~len buffer input =
  let head = Bigstringaf.substring buffer ~off:committed ~len:(len - committed) in
  String.concat "" (head :: input)

let valid_unstructured_flow input expect =
  Alcotest.test_case (Fmt.strf "[%a]" Fmt.(list ~sep:(always "; ") (fmt "%S")) input) `Quick @@ fun () ->
  let buffer = Bigstringaf.create 0x1000 in
  let rec go len input = function
    | Angstrom.Unbuffered.Done (committed, v) ->
      let rest = rest_of_input ~committed ~len buffer input in
      Ok (v, rest)
    | Fail (_, _, err) -> Error err
    | Partial { committed; continue; } ->
      let len = len - committed in
      Bigstringaf.blit buffer ~src_off:committed buffer ~dst_off:0 ~len ;

      match input with
      | [] -> go len [] (continue buffer ~off:0 ~len Complete)
      | x :: r ->
        Bigstringaf.blit_from_string x ~src_off:0 buffer ~dst_off:len ~len:(String.length x) ;
        go (len + String.length x) r (continue buffer ~off:0 ~len:(len + String.length x) Incomplete) in
  let buf = Bytes.create 0x1000 in
  let res = go 0 input (Angstrom.Unbuffered.parse (Unstrctrd_parser.unstrctrd buf)) in
  match res with
  | Ok (v, rest) ->
    let res = Unstrctrd.to_utf_8_string v in
    Alcotest.(check string) "result" res (fst expect) ;
    Alcotest.(check string) "rest" rest (snd expect)
  | Error err -> Alcotest.fail err

let invalid_unstructured_flow input =
  Alcotest.test_case (Fmt.strf "[%a]" Fmt.(list ~sep:(always "; ") (fmt "%S")) input) `Quick @@ fun () ->
  let buffer = Bigstringaf.create 0x1000 in
  let rec go len input = function
    | Angstrom.Unbuffered.Done (committed, v) ->
      let rest = rest_of_input ~committed ~len buffer input in
      Ok (v, rest)
    | Fail (_, _, err) -> Error err
    | Partial { committed; continue; } ->
      let len = len - committed in
      Bigstringaf.blit buffer ~src_off:committed buffer ~dst_off:0 ~len ;

      match input with
      | [] -> go len [] (continue buffer ~off:0 ~len Complete)
      | x :: r ->
        Bigstringaf.blit_from_string x ~src_off:0 buffer ~dst_off:len ~len:(String.length x) ;
        go (len + String.length x) r (continue buffer ~off:0 ~len:(len + String.length x) Incomplete) in
  let buf = Bytes.create 0x1000 in
  let res = go 0 input (Angstrom.Unbuffered.parse (Unstrctrd_parser.unstrctrd buf)) in
  match res with
  | Ok (v, rest) -> Alcotest.failf "Unexpected result: %S and %S" (Unstrctrd.to_utf_8_string v) rest
  | Error _ -> ()

let parser =
  let is_ftext = function '\033' .. '\057' | '\059' .. '\126' -> true | _ -> false in
  let is_wsp = function ' ' | '\t' -> true | _ -> false in
  let open Angstrom in
  take_while is_ftext >>= fun field_name ->
  Fmt.epr ">>> Start to recognize value of %S\n%!" field_name ;
  let buf = Bytes.create 0x7f in
  skip_while is_wsp *> char ':' *> Unstrctrd_parser.unstrctrd buf >>= fun v ->
  let res =
    let open Rresult in
    Unstrctrd.without_comments v >>| Unstrctrd.fold_fws >>| Unstrctrd.to_utf_8_string in
  match res with
  | Ok v -> return (field_name, `String v)
  | Error _ -> return (field_name, `Unstructured v)

let value =
  let pp ppf = function
    | `String v -> Fmt.string ppf v
    | `Unstructured _ -> Fmt.string ppf "#unstrctrd" in
  Alcotest.testable pp ( = )

let valid_unstructured_string input (field_name', v') =
  Alcotest.test_case (Fmt.strf "%S" input) `Quick @@ fun () ->
  let res =
    let open Rresult in
    ( R.reword_error R.msg <.> Angstrom.parse_string parser) input in
  match res with
  | Ok (field_name, v) ->
    Alcotest.(check string) "field-name" field_name field_name' ;
    Alcotest.(check value) "result" v v'
  | Error (`Msg err) -> Alcotest.failf "%s" err

let valid_unstructured_strings input lst' =
  Alcotest.test_case (Fmt.strf "%S" input) `Quick @@ fun () ->
  let res =
    let open Rresult in
    ( R.reword_error R.msg <.> Angstrom.(parse_string (many parser)) ) input in
  match res with
  | Ok lst ->
    List.iter2
      (fun (field_name', v') (field_name, v) ->
        Alcotest.(check string) "field-name" field_name field_name' ;
        Alcotest.(check value) "result" v v')
      lst' lst
  | Error (`Msg err) -> Alcotest.failf "%s" err

let input0 =
{|From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>

|}

let () =
  Alcotest.run "unstrctrd + angstrom"
    [ "valid", [ valid_unstructured_flow [ "Hello World!\r\n" ] ("Hello World!", "")
               ; valid_unstructured_flow [ "\r\r\n" ] ("\r", "")
               ; valid_unstructured_flow [ "\r\r\r\n" ] ("\r\r", "")
               ; valid_unstructured_flow [ "\r\n \r\r\n" ] ("\r\n \r", "")
               ; valid_unstructured_flow [ "\n\r\n" ] ("\n", "")
               ; valid_unstructured_flow [ "\n\n\r\n \r\n" ] ("\n\n\r\n ", "")
               ; valid_unstructured_flow [ "Hello World!\r\nSubject" ] ("Hello World!", "Subject")
               ; valid_unstructured_flow [ "Hello"; " "; "World!\r\n" ] ("Hello World!", "")
               ; valid_unstructured_flow [ "Hello"; ""; ""; ""; ""; ""; "\r\n"; "Subject" ] ("Hello", "Subject")
               ; valid_unstructured_flow [ "Hello"; "\r\n "; ""; "World!"; "\r\n" ] ("Hello\r\n World!", "")
               ; valid_unstructured_flow [ "Hello"; "\r\n"; " "; "\r\n"; ""; " "; ""; "World!\r\n" ] ("Hello\r\n \r\n World!", "")
               ; valid_unstructured_flow [ "Hello"; "\r"; "\n"; "Subject" ] ("Hello", "Subject")
               ; valid_unstructured_flow [ "Hello"; "\r"; "\n"; "\r"; "\n" ] ("Hello", "\r\n")
               ; valid_unstructured_string input0 ("From", `String " John Doe <jdoe@machine.example>")
               ; valid_unstructured_strings input0 [ ("From", `String " John Doe <jdoe@machine.example>")
                                                   ; ("To", `String " Mary Smith <mary@example.net>") ] ]
    ; "invalid", [ invalid_unstructured_flow [ "Hello" ]
                 ; invalid_unstructured_flow [ "Hello\r" ]
                 ; invalid_unstructured_flow [ "Hello\r\n " ] ] ]
