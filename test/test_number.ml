
module DFA = Number.DFA
module N =Number

module Testable = struct
  let result_token = Alcotest.testable (Fmt.of_to_string DFA.result_string) (=)
end

let check_accepted value =
  Alcotest.check Testable.result_token
    "Check for acceptance"
    DFA.Accepted
    (N.get_result value)

let test_accepted_values () =
  List.iter check_accepted
    [ "0.9"
    ; "7"
    ; "2352.2352"
    ]

let test =
  [ "test_accepted_values", `Quick, test_accepted_values
  ]
