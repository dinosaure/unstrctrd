let buffer = Bytes.create 0x1000

let () =
  Crowbar.add_test ~name:"unstrctrd" Crowbar.[ bytes ] @@ fun input ->
  let parser = Parser.unstructured buffer in
  match Angstrom.parse_string parser input with
  | Ok v ->
    let result = Unstrctrd.to_utf_8_string v in
    let part = String.sub input 0 (String.length result) in
    Crowbar.check_eq ~pp:Fmt.string ~eq:String.equal ~cmp:String.compare part result
  | Error _ -> Crowbar.bad_test ()
