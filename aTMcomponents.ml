type id = int
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;
type account_spec = {name : string; id : id; balance : int} ;;

let database = ref [] ;;

let rec initialize lst : unit =
  match lst with
  | [] -> ()
  | hd :: tl -> database := (!database @ [ref hd]); initialize tl

let acquire_id () : id =
  let () = print_string "Enter customer ID: " in
  let i = read_int () in
  i ;;

let acquire_amount () : int =
  let () = print_string "Enter customer amount" in
  let i = read_int () in
  i ;;

let acquire_act () : action =
  let () = print_string "Enter customer amount" in
  let s = read_line () in
  match s with
  | "B" -> Balance
  | "-" -> let amount = acquire_amount () in Withdraw amount
  | "+" -> let amount = acquire_amount () in Deposit amount
  | "=" -> Next
  | "X" -> Finished
  | _ -> raise (Invalid_argument "Please enter a valid character")
;;

let get_balance id : int =
  let records = List.filter (fun x -> !x.id = id) !database in
  !(List.nth records 0).balance ;;

let get_name id : string =
  let records = List.filter (fun x -> !x.id = id) !database in
  !(List.nth records 0).name ;;

let update_balance id change : unit =
  let rec helper =
    match !database with
    | [] -> raise (Invalid_argument "Enter a valid ID")
    | hd :: tl -> if !hd.id = id then hd := {name = !hd.name; id = !hd.id; balance = change} in
  helper ;;

let present_message str : unit =
  Printf.printf "%s" str ;;

let deliver_cash amount : unit =
  present_message "Cash dispensed!" ;;

(* ADKFA;LKDSJFA;LKSDF;k *)
