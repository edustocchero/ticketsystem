open Opium

let ( let* ) = Lwt.bind

(* Aux function for ticket response *)
let json_ticket_response ticket =
  ticket |> Ticket.to_yojson |> Response.of_json |> Lwt.return

(* Create Ticket *)
let create_ticket request =
  let* new_ticket = Request.to_json_exn request in
  match Ticket.new_ticket_of_yojson new_ticket with
  | Error _ -> Lwt.return (Response.make ~status:`Bad_request ())
  | Ok new_ticket ->
    new_ticket
    |> Controllers.create_new_ticket
    |> Ticket.to_yojson
    |> Response.of_json ~status:`Created
    |> Lwt.return

(* Find Ticket *)
let find_ticket request =
  let query_id = Request.query "id" request in
  match Controllers.find_ticket_by_id query_id with
  | None -> Lwt.return (Response.make ~status:`Not_found ())
  | Some ticket -> json_ticket_response ticket

(* Find All *)
let find_all _request =
  Controllers.find_all ()
  |> [%to_yojson: Ticket.t list]
  |> Response.of_json
  |> Lwt.return

(* Assign To Ticket *)
let assign_ticket request =
  let* json = Request.to_json_exn request in
  let assignment = Ticket.ticket_assignment_of_yojson json in
  match assignment with
  | Error _ -> Lwt.return (Response.make ~status:`Bad_request ())
  | Ok assignment ->
    match Controllers.assign_ticket assignment with
    | Either.Right ticket -> json_ticket_response ticket
    | Either.Left msg ->
      Lwt.return (Response.make ~body:(Body.of_string msg) ~status:`Not_found ())

(* Update Ticket Status *)
let update_ticket_status request =
  let query_id = Request.query "id" request in
  match query_id with
  | None -> Lwt.return (Response.make ~status:`Bad_request ())
  | Some id ->
    match Controllers.ticket_next_status id with
    | Either.Left msg ->
      Lwt.return (Response.make ~body:(Body.of_string msg) ~status:`Bad_request ())
    | Either.Right ticket -> json_ticket_response ticket
