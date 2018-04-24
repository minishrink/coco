
module DFA = Number.DFA
module N = Number

module Testable = struct
  let result_token = Alcotest.testable (Fmt.of_to_string N.Num.token_string) (=)
  let result = Alcotest.testable (Fmt.of_to_string DFA.result_string) (=)
  let final_state = Alcotest.testable (Fmt.of_to_string DFA.print_state) (=)
end

let test_with_debugging test =
  try
    ignore (test ())
  with
  | DFA.Automaton_failure e ->
    Printf.printf "\nAUTOMATON_FAILURE [ %s ]\n" (DFA.automaton_error_string e)

let check_accepted expected actual =
  Alcotest.check Testable.result_token
    "Check for acceptance"
    expected actual

let check_rejected actual =
  Alcotest.check Testable.result
    "Check for rejection"
    DFA.Rejected actual

let check_int input   = check_accepted N.Num.(Int input)
let check_float input = check_accepted N.Num.(Float input)


let test_accepted_values () =
  List.iter test_with_debugging
    [ (fun () -> check_float "0.9")
    ; (fun () -> check_int "7")
    ; (fun () -> check_float "2352.2352")
    ]

let test_rejected_values () =
  List.iter
    (fun input -> test_with_debugging
        (fun () -> input |> DFA.get_result |> check_rejected)
    )
    [ "."
    ; "92."
    ; ".325"
    ]

let test =
  [ "test_accepted_values", `Quick, test_accepted_values
  ; "test_rejected_values", `Quick, test_rejected_values
  ]
