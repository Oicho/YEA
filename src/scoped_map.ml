open Typer;;
class scoped_map =
  object (self)
  val  mutable  map : (string, number) Hashtbl.t list = []
  method scope_begin = map <- (Hashtbl.create 127542)::map
  method scope_end = match map with
    | e::l -> map <- l
    | _ -> failwith("no scope started")
  method set_var name value  = match map with
  | e::l -> Hashtbl.add e name value
  | _ -> failwith("no scope started")
  method get_var name = match map with
  | e::l -> begin
    try
      (Hashtbl.find e name)
    with
      | Not_found -> failwith("Not_found")
  end
  | _ -> failwith("no scope started")
  end