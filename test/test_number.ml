
module A = Alcotest
module N = Number
module DFA = N.DFA

module Testable = struct
  let testable printer_fn = A.testable (Fmt.of_to_string printer_fn) (=)

  let result       = testable DFA.result_string
  let final_state  = testable DFA.print_state
  let result_token = testable N.Num.token_string
end

let test_with_debugging test =
  try
    ignore (test ())
  with
  | DFA.Automaton_failure e ->
    Printf.printf "\nAUTOMATON_FAILURE [ %s ]\n" (DFA.automaton_error_string e)

let check_accepted expected actual =
  A.check Testable.result_token
    "Check for acceptance"
    expected actual

let check_rejected actual =
  A.check Testable.result
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
    (fun input ->
       test_with_debugging
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
