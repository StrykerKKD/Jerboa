(** [Jerboa.request] module contains the type definition of the Request record and a constructor for the Response.*)

(** [Response.t] is a tuple, whose first element is the http code and it's last part is the body of the response.*)
type t = int * string

(** [Response.create status_code body] creates a reponse based on the provided arguments:
  - status_code: http code to be used for the response
  - body: http body of the response
*)
let create status_code body = (status_code, body)

let convert_to_cohttp_response response =
  let status_code, body = response in
  let status_code = Cohttp.Code.status_of_code status_code in
  (status_code, body)