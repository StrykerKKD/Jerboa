type t = Cohttp.Cookie.Set_cookie_hdr.t
type cookie = Cohttp.Cookie.cookie
type expiration = Cohttp.Cookie.expiration

let create ?expiration ?path ?domain ?secure ?http_only key value =
    let cookie = (key, value) in
    Cohttp.Cookie.Set_cookie_hdr.make ?expiration ?path ?domain ?secure ?http_only cookie

let serilaize = Cohttp.Cookie.Set_cookie_hdr.serialize