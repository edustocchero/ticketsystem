
type field_error = {
  field : string;
  reason : string;
}
[@@deriving yojson]

type t =
  | ValidationError of { error : string; fields : field_error list; }
  | StandardError of { error : string; }
  | FieldError of field_error
  [@@deriving yojson]
let to_yojson = function
  | FieldError fe -> field_error_to_yojson fe
  | StandardError { error; } -> `Assoc [("Error", `String error)]
  | ValidationError { error; fields; } -> `Assoc [
      ("error", `String error);
      ("fields", [%to_yojson: field_error list] fields)
    ]
