open Either

type t = {
  id : string;
  user_id : string;
  assigned_to : string option;
  status : Status.t;
  priority : Priority.t;
  description : string;
}
[@@deriving yojson]

type new_ticket = {
  user_id : string;
  priority : Priority.t;
  description : string;
}
[@@deriving yojson]

let new_ticket id new_ticket = {
  id;
  user_id = new_ticket.user_id;
  assigned_to = None;
  status = Open;
  priority = new_ticket.priority;
  description = new_ticket.description;
}

let close_ticket ticket = { ticket with status = Closed; }

let next_status ticket =
  match (ticket.status, ticket.assigned_to) with
  | _, None -> Left "Ticket need to be assigned."
  | Closed, _ -> Left "Ticket already closed."
  | Open, _ -> Right { ticket with status = InProgress; }
  | InProgress, _ -> Right { ticket with status = Resolved; }
  | Resolved, _ -> Right { ticket with status = Closed; }

let assign_to ticket ~assignee_id =
  match (ticket.status, ticket.assigned_to) with
  | Open, None -> Right {
      ticket with
      assigned_to = Some assignee_id;
      status = InProgress;
    }
  | _, _ -> Left "Ticket already assigned."
