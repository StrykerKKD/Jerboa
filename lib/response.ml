type t = int * string

let create status_code body = (status_code, body)

let convert_to_cohttp_response response =
  let status_code, body = response in
  let status_code = Cohttp.Code.status_of_code status_code in
  (status_code, body)