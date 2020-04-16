(*
                Component Behaviors of an ATM Machine

The functions here represent the component behaviors that an ATM
machine can take, including: prompting for and acquiring from the
customer some information (choosing an action or entering an account
id or an amount); presenting information to the customer; dispensing
cash.

Implementation of these behaviors is likely to require some database
of accounts, each with an id number, a customer name, and a current
balance.
 *)

(* Customer account identifiers *)
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;

(*....................................................................
 Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; mutable balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)

let data = ref []

let rec initialize (account_list: account_spec list) : unit =

  match account_list with
  | [] -> ()
  | hd :: tl -> let newval = (hd.id, hd) in
    data := newval :: !data;
    initialize tl ;;


(*....................................................................
 Acquiring information from the customer
*)

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let acquire_id : unit -> id =
  let id = read_int () in
  let id_list = List.map (fst) !data in
  if List.mem id id_list then (fun () -> id) else raise (Failure "invalid id") ;;

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
let acquire_amount : unit -> int =
  let amount = read_int () in
  if amount >= 0 then (fun () -> amount) else raise (Failure "invalid amount") ;;

(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
let acquire_act : unit -> action =
  match read_line () with
  | 'B' -> () -> Balance
  | '+' -> () -> Deposit _
  | '-'' -> () -> Withdraw _
  | '='' -> () -> Done
  | 'X' -> () -> Exit


(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id.
 *)

(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
let get_balance (i: id) : int =
  let user = List.assoc i !data in
    user.balance ;;

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name (i: id) :string =
  let user = List.assoc i !data in
      user.name ;;

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id,setting it to the given amount. *)
let update_balance (i: id) (amount: int) : unit =
  let user = List.assoc i !data in
      user.balance <- amount  ;;
(*....................................................................
  Presenting information and cash to the customer
 *)

(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)
val present_message : string -> unit ;;

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)
val deliver_cash : int -> unit ;;
