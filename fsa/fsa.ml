
(*--- type declarations ---*)

type symbol = Sym of char
type alphabet = symbol list
type state = St of string
type result = Accepted | Rejected (* automaton input: string, output: result *)

type automaton =
  {
    alphabet : symbol list;
    mutable transitions : (state, symbol * state) Hashtbl.t; 
    initial : state;
    final : state list
  }

exception Illegal_transition 
exception Invalid_input

(*--- constructor helpers ---*)

let make_alphabet lst = List.map (fun ltr -> Sym ltr) lst
let make_state str = St str

let make_automaton ~alphabet ~transitions ~initial ~final =
  { alphabet ; transitions ; initial ; final }

let add_transition fsa ltr src dst =
  let key, value = St src, (Sym ltr, St dst) in
  Hashtbl.add fsa.transitions key value

(*--- execution logic ---*)

let process_string fsa str =

(* Search automaton Hashtbl for next transition on given symbol
   raises Illegal_transition on invalid input or Not_found
   automaton -> symbol -> state -> state *)
  let get_next_state fsa ltr current_state =
    try
      match ltr, current_state with
      | Sym x, St y -> 
        let transitions = Hashtbl.find_all fsa.transitions current_state in
        List.assoc ltr transitions
    with
    | Not_found -> raise Illegal_transition
    | _ -> raise Invalid_input
  in
  (* currently only used once, may be helpful in future *)
  let is_final fsa state = List.mem state fsa.final in

  (* transform string input into char list for easier traversal *)
  let str_to_char_lst str =
    let len = String.length str in
    let rec chrl x =
      if x < len
      then str.[x]::(chrl (x+1))
      else []
    in chrl 0
  in

  (* input string -> char list -> Sym list *)
  let lst = str |> str_to_char_lst |> make_alphabet in

  (* pass in string, return result *)
  let rec run st = function
    | [] -> if is_final fsa st
      then Accepted
      else Rejected
    | a::b -> run (get_next_state fsa a st) b
  in try run fsa.initial lst with _ -> Rejected

