
type t = Open | InProgress | Resolved | Closed

let to_yojson = function
  | Open -> `String "OPEN"
  | InProgress -> `String "IN_PROGRESS"
  | Resolved -> `String "RESOLVED"
  | Closed -> `String "CLOSED"
and of_yojson = function
  | `String "OPEN" -> Ok Open
  | `String "IN_PROGRESS" -> Ok InProgress
  | `String "RESOLVED" -> Ok Resolved
  | `String "CLOSED" -> Ok Closed
  | json -> Error (Yojson.Safe.to_string json)
