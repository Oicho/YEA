open Typer;;
exception No_scope_started;;

class scoped_map =
  object (self)
  val  mutable  map : (string, number) Hashtbl.t list = []
  method scope_begin = map <- (Hashtbl.create 127542)::map

  method scope_end = match map with
    | e::l -> map <- l
    | _ -> raise No_scope_started

  method set_var name value  = match map with
  | e::l -> Hashtbl.add e name value
  | _ -> raise No_scope_started

  method get_var name =
    let rec f_get_var name  = function
      | e::l ->
       begin
        try
          (Hashtbl.find e name)
        with
        | Not_found -> f_get_var name l
      end
      | _ ->  raise Not_found
    in
    try
      match map with
      | [] -> raise No_scope_started
      | _ -> f_get_var name map
    with
    | Not_found-> match map with
    | e::l -> (Hashtbl.add e name (Int(0))); Int(0)
    | _ -> raise No_scope_started
end