open Opium

let () =
  App.empty
  |> App.post "/new" Handlers.create_ticket
  |> App.get "/find" Handlers.find_ticket
  |> App.get "/all" Handlers.find_all
  |> App.patch "/assign" Handlers.assign_ticket
  |> App.patch "/update" Handlers.update_ticket_status
  |> App.run_multicore
