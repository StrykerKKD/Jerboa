type t = Cohttp.Header.t

let predefined_user_agent = Cohttp.Header.user_agent

let create = Cohttp.Header.init

let create_with_list = Cohttp.Header.of_list

let remove = Cohttp.Header.remove

let replace = Cohttp.Header.replace

let add = Cohttp.Header.add

let add_values = Cohttp.Header.add_multi

let add_list = Cohttp.Header.add_list

let get = Cohttp.Header.get

let get_values = Cohttp.Header.get_multi

let has_key = Cohttp.Header.mem

let is_empty = Cohttp.Header.is_empty

let is_form = Cohttp.Header.is_form

let get_cookies = Cohttp.Cookie.Set_cookie_hdr.extract

let set_cookie ?version header cookie = 
  let key, value = Cookie.serilaize ?version cookie in
  add header key value

let to_string = Cohttp.Header.to_string

let to_list_string = Cohttp.Header.to_frames