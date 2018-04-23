open Fsa

(* --- create an automaton that accepts the language L = { 1 0* 1 1* 0 } --- *)

(* define alphabet and states *)
let binary = make_alphabet ['0';'1']
let [q1;q2;q3;q4] = List.map make_state ["q1";"q2";"q3";"q4"]

(* define transition Hashtbl *)
let transitions = Hashtbl.create 5

(* construct automaton *)
let autom = make_automaton ~alphabet:binary ~transitions ~initial:q1 ~final:[q4]

(* helper function to print result *)
let result_to_string = function
  | Rejected -> "REJECTED"
  | Accepted -> "ACCEPTED"

(* --- execution --- *)

let () = 
  let [zero;one] = binary in
  [ q1 , [one,  q2]
  ; q2 , [zero, q2 ; one,  q3]
  ; q3 , [one,  q3 ; zero, q4] 
  ] (* no transitions for final state q4 *)
  |> List.iter
    (fun (st, transition_pair) ->
       transition_pair
       |> List.iter
         (Hashtbl.add transitions st)
    );

  let print_result str result =
    Printf.printf "\t %s <- %s\n" (result_to_string result) str in

  (* test input strings *)
  [ "101"
  ; "1"
  ; ""
  ; "10110"
  ; "110"
  ; "010110"
  ; "1000010"
  ; "iamalsoastringasitsohappens"
  ]
  |> List.iter
    (fun str -> str
                |> process_string autom
                |> print_result str
    )
