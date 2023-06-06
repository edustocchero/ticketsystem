
type t = Low | Medium | High

let to_yojson = function
  | Low -> `String "LOW"
  | Medium -> `String "MEDIUM"
  | High -> `String "HIGH"
and of_yojson = function
  | `String "LOW" -> Ok Low
  | `String "MEDIUM" -> Ok Medium
  | `String "HIGH" -> Ok High
  | json -> Error (Yojson.Safe.to_string json)
