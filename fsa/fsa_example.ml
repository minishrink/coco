open Fsa

(* ************** *
   example input 
 * ************** *)

(* alphabet and transition sets *)
let binary = make_alphabet ['0';'1']
let st_1 = make_trans_set ['0',"alpha";'1',"beta"]  (*  q0 -[0* 1]-> q1  *)
let st_2 = make_trans_set ['0',"beta";'1',"gamma"]  (*  q1 -[0* 1]-> q2  *)
let st_3 = make_trans_set ['1',"gamma";'0',"delta"] (*  q2 -[1* 0]-> q3  *)
let st_4 = make_trans_set ['1',"epsilon"]           (*  q3 -[   1]->   _q4_ *)
let st_5 = make_trans_set []

(* little helpers *)
let rec last = function
  | [a] -> Some a
  | [] -> None
  | a::b -> last b

let unbox = function
  | Some x -> x
  | None -> raise (Failure "failed to unbox")

(* test FSA *)
let states =
  [ ("alpha",st_1) ; ("beta",st_2) ; ("gamma",st_3) ; ("delta",st_4) ; ("epsilon",st_5) ]
  |> List.map
    (fun (uid, transitions) -> make_state uid transitions)
let binary_aut = make_fsa ?states:(Some states) binary (List.hd states) [states |> last |> unbox]

(* ****** *
    tests 
 * ****** *)


(* test assertion for truth *)
let assert_bool boolean error =
  if not boolean
  then raise error

(* test assertion for equality *)
let assert_equal x y error =
  let boolean = (x=y) in
  assert_bool boolean error

(* result parsing functions *)
let result_of_bool x = if x then Accepted else Rejected

let bool_of_result = function
  | Accepted -> true
  | Rejected -> false

let string_of_result = function
  | Accepted -> "Accepted"
  | Rejected -> "Rejected"
