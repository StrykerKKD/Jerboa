type t = {
    uri_parameter: (string * string option) list;
    query_parameter: (string * string list) list;
    body: string;
}

let make uri_parameter query_parameter body = {
    uri_parameter;
    query_parameter;
    body;
}