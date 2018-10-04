let test_parser () =
    Alcotest.(check int) "Should parse" 42 User_agent_parser.hest

let set = [
    Alcotest.test_case "Test parser" `Quick test_parser;
]

let () =
  Alcotest.run "test suite" [
    ("test", set);
  ]
