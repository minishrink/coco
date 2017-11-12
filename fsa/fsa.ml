(* implementation of a deterministic finite state automaton because
   who doesn't love concrete implementations of abstract machinery *)

(* types are mostly wrappers for FSA tuple members*)
type symbol = Sym of char
type alphabet = symbol list
type transition = (symbol * string) (* symbol , state_uid of next state *)

(* represent state and automaton as records
  TODO: move away from concrete representations of states, perhaps use a hashtbl for transitions *)
type state =
  {
    uid : string ;
    transitions : transition list ;
  }

type fsa = 
  {
    mutable states : state list ;
    alphabet : alphabet ;
    initial : state ;
    final : state list
  }

type result = Rejected | Accepted

(* ******************* *
 * constructor helpers *
 * ******************* *)

(* wrap raw char or string to construct transitions or alphabet *)
let make_alphabet lst = lst |> List.map (fun x -> Sym x)
let make_transition chr str = (Sym chr,str)
let make_trans_set chrlst = List.map (fun (chr,str) -> make_transition chr str) chrlst
let make_state uid transitions = { uid ; transitions }

(* make FSA from pre-wrapped arguments *)
let make_fsa ?states alphabet initial final =
  let get_states states =
    match states with
    | None -> []
    | Some states -> states in
  { states = get_states states ; alphabet ; initial ; final }

(* FSA constructor helper that takes raw input and wraps it *)
let make_fsa_raw id alpha init fin =
  let alphabet = alpha |> make_alphabet in
  let initial,final = init,fin in
  { states = [] ; alphabet ; initial ; final }

(* is current state an accepting state? *)
let is_final fsa state =
  List.mem state fsa.final

(* throw this when a transition fails *)
exception Illegal_transition
exception Symbol_not_recognised of symbol

(* transform string input into char list for easier traversal *)
let str_to_char_lst str =
  let len = String.length str in
  let rec chrl x =
    if x < len
    then str.[x]::(chrl (x+1))
    else []
  in chrl 0

let sym_to_char = function
  | Sym x -> x
  | y -> raise (Symbol_not_recognised y)


(* *************** *
   execution logic
 * *************** *)

(* check if a transition is allowed *)
let valid_state fsa uid =
  fsa.states
  |> List.map (fun st -> st.uid)
  |> List.mem uid

(* give symbol to state, get uid for next state *)
let next_st_uid st sym =
  List.assoc sym st.transitions

(* given state uid, get state from FSA *)
let next_state_from_uid fsa st_uid =
  try
    List.find (fun st -> st.uid = st_uid) fsa.states
  with Not_found -> raise Illegal_transition

let format_input str =
  str |> str_to_char_lst |> make_alphabet

(* given fsa, state, symbol -> next state or exn *)
let next_state fsa st sym =
  (* check symbol belongs to FSA alphabet *)
  if List.mem sym fsa.alphabet
  then sym
       |> next_st_uid st
       |> next_state_from_uid fsa
  else raise (Symbol_not_recognised sym)

(* iterate through input string and return result *)
let trace_input fsa str =
  (* input string -> char list -> symbol list *)
  let lst = format_input str in
  let rec run st = function 
    | [] -> if is_final fsa st
            then Accepted
            else Rejected
    | a::b -> run (next_state fsa st a) b
  in try run fsa.initial lst with _ -> Rejected

