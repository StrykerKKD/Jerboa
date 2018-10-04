type middleware = Request.t -> Request.t

type t = middleware list

let apply_middlewares middleware_config request =
    Base.List.fold middleware_config ~init:request ~f:(fun request_accumulator middleware -> middleware request_accumulator)