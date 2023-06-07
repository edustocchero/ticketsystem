open Either

let tickets = ref []

let id_generator = Uuidm.v4_gen (Random.State.make_self_init ())

let find_all () = !tickets

let create_new_ticket new_ticket =
  let id = Uuidm.to_string (id_generator ()) in
  let created_ticket = Ticket.new_ticket id new_ticket in
  tickets := !tickets@[created_ticket];
  created_ticket

let find_ticket_by_id id =
  match id with
  | None -> None
  | Some id ->
    let filter =
      List.filter (fun (x : Ticket.t) -> x.id = id) !tickets
    in
    match filter with
    | x :: _ -> Some x
    | [] -> None

let update_ticket (ticket : Ticket.t) =
  List.map
    (fun (t : Ticket.t) -> if t.id = ticket.id then ticket else t)
    !tickets

let assign_ticket (assignment : Ticket.ticket_assignment) =
  match find_ticket_by_id (Some assignment.ticket_id) with
  | None -> Left "Ticket not found."
  | Some ticket ->
    match Ticket.assign_to ticket ~assignee_id:assignment.assignee_id with
    | Left msg -> Left msg
    | Right ticket ->
      tickets := update_ticket ticket;
      Right ticket

let ticket_next_status ticket_id =
  let ticket = find_ticket_by_id (Some ticket_id) in
  match ticket with
  | None -> Left "Ticket not found."
  | Some ticket ->
    match Ticket.next_status ticket with
    | Right ticket ->
      tickets := update_ticket ticket;
      Right ticket
    | left -> left
