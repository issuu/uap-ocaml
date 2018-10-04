open Base
open User_agent_parser

let ua_result = Alcotest.testable UAParser.pp_result UAParser.equal_result
let os_result = Alcotest.testable OSParser.pp_result OSParser.equal_result
let device_result = Alcotest.testable DeviceParser.pp_result DeviceParser.equal_result

let load_file : string -> string = fun filename ->
  let channel = Stdio.In_channel.create filename in
  let len = Stdio.In_channel.length channel |> Int64.to_int_exn in
  let buf = Bytes.create len in
  Stdio.In_channel.really_input_exn channel ~buf ~len ~pos:0;
  Stdio.In_channel.close channel;
  Caml.Printf.sprintf "%s" @@ Bytes.to_string buf

let quick_test_ua_parser () =
  let t = UAParser.init () in
  let expected = UAParser.{ family = "Firefox (Minefield)"; major = Some "4"; minor = Some "0"; patch = Some "1pre"; } in
  let actual = UAParser.parse t "Mozilla/5.0 (Windows; Windows NT 5.1; rv:2.0b3pre) Gecko/20100727 Minefield/4.0.1pre" in
  Alcotest.(check ua_result) "corectly parsed browser family" expected actual

let quick_test_os_parser () =
  let t = OSParser.init () in
  let expected = OSParser.{ family = "Windows"; major = Some "95"; minor = None; patch = None; patch_minor = None } in
  let actual = OSParser.parse t "Mozilla/5.0 (Windows; U; Win95; en-US; rv:1.1) Gecko/20020826" in
  Alcotest.(check os_result) "corretly parsed os" expected actual

let quick_test_device_parser () =
  let t = DeviceParser.init () in
  let expected = DeviceParser.{ family = "Odys PEDI PLUS W"; brand = Some "Odys"; model = Some "PEDI PLUS W" } in
  let actual = DeviceParser.parse t "Mozilla/5.0 (Linux; U; Android 4.2.2; de-de; PEDI_PLUS_W Build/JDQ39) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Safari/534.30" in
  Alcotest.(check device_result) "correctly parsed device" expected actual

let yaml_to_test_cases_assoc s =
  Yaml.of_string s
  |> Result.map_error ~f:(function `Msg s -> s)
  |> Result.ok_or_failwith
  |> function
    | `O [("test_cases", `A seq)] -> List.map seq ~f:(function
      | `O assoc -> List.Assoc.map assoc ~f:(function 
          | `String v -> v
          | _ -> failwith "bad test case yaml")
      | _ -> failwith "bad test case yaml")
    | _ -> failwith "bad test case yaml"

let test_ua_parser () =
  let t = UAParser.init () in
  List.map (yaml_to_test_cases_assoc (load_file "../uap-core/tests/test_ua.yaml")) ~f:(fun assoc ->
    let user_agent_string = List.Assoc.find_exn assoc ~equal:String.equal "user_agent_string" in
    let family = List.Assoc.find_exn assoc ~equal:String.equal "family" in
    let major = List.Assoc.find_exn assoc ~equal:String.equal "major" |> function "" -> None | s -> Some s in
    let minor = List.Assoc.find_exn assoc ~equal:String.equal "minor" |> function "" -> None | s -> Some s in
    let patch = List.Assoc.find_exn assoc ~equal:String.equal "patch" |> function "" -> None | s -> Some s in
    Alcotest.test_case user_agent_string `Slow (fun () ->
      let expected = UAParser.{ family ; major; minor; patch } in
      let actual = UAParser.parse t user_agent_string in
      Alcotest.(check ua_result) user_agent_string expected actual))

let test_os_parser () =
  let t = OSParser.init () in
  List.map (yaml_to_test_cases_assoc (load_file "../uap-core/tests/test_os.yaml")) ~f:(fun assoc ->
    let user_agent_string = List.Assoc.find_exn assoc ~equal:String.equal "user_agent_string" in
    let family = List.Assoc.find_exn assoc ~equal:String.equal "family" in
    let major = List.Assoc.find_exn assoc ~equal:String.equal "major" |> function "" -> None | s -> Some s in
    let minor = List.Assoc.find_exn assoc ~equal:String.equal "minor" |> function "" -> None | s -> Some s in
    let patch = List.Assoc.find_exn assoc ~equal:String.equal "patch" |> function "" -> None | s -> Some s in
    let patch_minor = List.Assoc.find_exn assoc ~equal:String.equal "patch_minor" |> function "" -> None | s -> Some s in
    Alcotest.test_case user_agent_string `Slow (fun () ->
      let expected = OSParser.{ family ; major; minor; patch; patch_minor } in
      let actual = OSParser.parse t user_agent_string in
      Alcotest.(check os_result) "test" expected actual))

let test_device_parser () =
  let t = DeviceParser.init () in
  List.map (yaml_to_test_cases_assoc (load_file "../uap-core/tests/test_device.yaml")) ~f:(fun assoc ->
    let user_agent_string = List.Assoc.find_exn assoc ~equal:String.equal "user_agent_string" in
    let family = List.Assoc.find_exn assoc ~equal:String.equal "family" in
    let brand = List.Assoc.find_exn assoc ~equal:String.equal "brand" |> function "" -> None | s -> Some s in
    let model = List.Assoc.find_exn assoc ~equal:String.equal "model" |> function "" -> None | s -> Some s in
    Alcotest.test_case user_agent_string `Slow (fun () ->
      let expected = DeviceParser.{ family; brand; model } in
      let actual = DeviceParser.parse t user_agent_string in
      Alcotest.(check device_result) (Caml.Printf.sprintf "'%s'" user_agent_string) expected actual))


let set = [
    Alcotest.test_case "Test useragent parser" `Quick quick_test_ua_parser;
    Alcotest.test_case "Test os parser" `Quick quick_test_os_parser;
    Alcotest.test_case "Test device parser" `Quick quick_test_device_parser;
]

let () =
  Alcotest.run "test suite" [
    ("test", set);
    ("ua_parser", test_ua_parser ());
    ("os_parser", test_os_parser ());
    ("device_parser", test_device_parser ());
  ]
