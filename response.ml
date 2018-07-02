type t = int * string

let make status_code body = (status_code, body)

let convert response =
  let status_code, body = response in
  let status_code = Cohttp.Code.status_of_code status_code in
  (status_code, body)