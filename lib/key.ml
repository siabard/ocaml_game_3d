
class keystates =
  object
    val mutable held_keys : (Sdlscancode.t, bool) Hashtbl.t = Hashtbl.create 242
    val mutable pressed_keys : (Sdlscancode.t, bool) Hashtbl.t = Hashtbl.create 242
    val mutable released_keys : (Sdlscancode.t, bool) Hashtbl.t = Hashtbl.create 242

    method begin_frame () =
      Hashtbl.reset pressed_keys;
      Hashtbl.reset released_keys;
      ()

    method keyup_event scancode =
      Hashtbl.add released_keys scancode true;
      Hashtbl.add held_keys scancode false;
      ()

    method keydown_event scancode  =
      Hashtbl.add pressed_keys scancode true;
      Hashtbl.add held_keys scancode true;
      ()

    method was_key_pressed scancode : bool =
      match Hashtbl.find_opt pressed_keys scancode with
      | Some(v) -> v
      | None -> false

    method was_key_released scancode : bool =
      match Hashtbl.find_opt released_keys scancode with
      | Some(v) -> v
      | None -> false

    method was_key_held scancode : bool =
      match Hashtbl.find_opt held_keys scancode with
      | Some(v) -> v
      | None -> false

  end
