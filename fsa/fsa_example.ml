open Fsa

(* --- create an automaton that accepts the language L = { 1 0* 1 1* 0 } --- *)

(* define alphabet and states *)

let binary = make_alphabet ['0';'1']
let [zero;one] = binary
let [q1,q2,q3,q4] = List.map make_state ["q1";"q2";"q3";"q4"]

(* define transition Hashtbl *)

let transitions = Hashtbl.create 5

[ one, [ q1,q2 ; q2,q3 ; q3,q3 ]
; zero, [ q2,q2 ; q3,q4 ]
]
|> List.iter
  (fun sym, state_pair_lst ->
    state_pair_lst
    |> List.iter
      (Hashtbl.add transitions sym)
  )

(* construct automaton *)

let autom = make_automaton ~alphabet:binary ~transitions ~initial:q1 ~final:[q4] in
let result_to_string = function
  | Rejected -> "REJECTED"
  | Accepted -> "ACCEPTED" in

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
  (fun str -> trace_input fsa
            |> result_to_string
            |> print_endline
  )
