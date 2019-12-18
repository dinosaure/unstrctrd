let () = Printexc.record_backtrace true

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
  let res = go 0 input (Angstrom.Unbuffered.parse (Unstrctrd_parser.fast_unstrctrd buf)) in
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
  let res = go 0 input (Angstrom.Unbuffered.parse (Unstrctrd_parser.fast_unstrctrd buf)) in
  match res with
  | Ok (v, rest) -> Alcotest.failf "Unexpected result: %S and %S" (Unstrctrd.to_utf_8_string v) rest
  | Error _ -> ()

let () =
  Alcotest.run "unstrctrd + angstrom"
    [ "valid", [ valid_unstructured_flow [ "Hello World!\r\n" ] ("Hello World!", "")
               ; valid_unstructured_flow [ "Hello World!\r\nSubject" ] ("Hello World!", "Subject")
               ; valid_unstructured_flow [ "Hello"; " "; "World!\r\n" ] ("Hello World!", "")
               ; valid_unstructured_flow [ "Hello"; ""; ""; ""; ""; ""; "\r\n"; "Subject" ] ("Hello", "Subject")
               ; valid_unstructured_flow [ "Hello"; "\r\n "; ""; "World!"; "\r\n" ] ("Hello\r\n World!", "")
               ; valid_unstructured_flow [ "Hello"; "\r\n"; " "; "\r\n"; ""; " "; ""; "World!\r\n" ] ("Hello\r\n \r\n World!", "")
               ; valid_unstructured_flow [ "Hello"; "\r"; "\n"; "Subject" ] ("Hello", "Subject")
               ; valid_unstructured_flow [ "Hello"; "\r"; "\n"; "\r"; "\n" ] ("Hello", "\r\n") ]
    ; "invalid", [ invalid_unstructured_flow [ "Hello" ]
                 ; invalid_unstructured_flow [ "Hello\r" ]
                 ; invalid_unstructured_flow [ "Hello\r\n " ] ] ]
