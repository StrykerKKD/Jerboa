include Cohttp.Header

let get_cookies = Cohttp.Cookie.Set_cookie_hdr.extract

let set_cookie ?version header cookie = 
  let key, value = Cookie.serilaize ?version cookie in
  add header key value