let errored = Alcotest.testable Rresult.R.pp_msg (fun _ _ -> true)
let str = Alcotest.testable (fun ppf -> Fmt.pf ppf "%S") String.equal

let valid_unstructured_string input expect =
  Alcotest.test_case (Fmt.strf "%S" expect) `Quick @@ fun () ->
  let res = let open Rresult.R in Unstrctrd.of_string input >>| fun (_, t) -> Unstrctrd.to_utf_8_string t in
  Alcotest.(check (result str errored)) "expect" res (Ok expect)

let valid_unstructured_string_without_comment input expect =
  Alcotest.test_case (Fmt.strf "%S" expect) `Quick @@ fun () ->
  let res =
    let open Rresult.R in
    Unstrctrd.of_string input
    >>= fun (_, t) -> Unstrctrd.without_comments t
    >>| Unstrctrd.to_utf_8_string in
  Alcotest.(check (result str errored)) "expect" res (Ok expect)

let () =
  Alcotest.run "unstrctrd"
    [ "valid", [ valid_unstructured_string "Hello\r\n" "Hello"
               ; valid_unstructured_string "Hello\r\n World\r\n" "Hello\r\n World"
               ; valid_unstructured_string "Hello\r\n \r\n \r\n World\r\n" "Hello\r\n \r\n \r\n World"
               ; valid_unstructured_string " \r\n \r\n" " \r\n "
               ; valid_unstructured_string "\r\nHello World!" ""
               ; valid_unstructured_string " \r\nHello World!" " "
               ; valid_unstructured_string " \r\n \r\nHello World!" " \r\n "
               ; valid_unstructured_string "Hello\r\nWorld\r\n" "Hello"
               ; valid_unstructured_string "\r\n" ""
               ; valid_unstructured_string "\r\r\n" "\r"
               ; valid_unstructured_string "\r\r\r\n" "\r\r"
               ; valid_unstructured_string "\r\n \r\r\n" "\r\n \r"
               ; valid_unstructured_string "\n\r\n" "\n"
               ; valid_unstructured_string "\n\n\r\n" "\n\n"
               ; valid_unstructured_string "\n\n\r\n \r\n" "\n\n\r\n "
               ; valid_unstructured_string "\n\r\n Hello\r\n World\r\n !\r\n" "\n\r\n Hello\r\n World\r\n !" ]
    ; "comments", [ valid_unstructured_string_without_comment "Hello(World)\r\n" "Hello"
                  ; valid_unstructured_string_without_comment "Hello (a\r\n b)World!\r\n" "Hello World!"
                  ; valid_unstructured_string_without_comment "(a\r\n (b \r\n c))\r\n" ""
                  ; valid_unstructured_string_without_comment "(a)(b)Hello(c)\r\n" "Hello" ] ]
