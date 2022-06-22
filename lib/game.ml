open Sdl
open Sdlevent

class game =
  object (s)
    val mutable window = None
    val mutable renderer = None
    val mutable is_running = false
    method init width height = let _ = Sdl.init[`VIDEO] in
                  let m_window = Sdlwindow.create2
                                   ~x:`undefined ~y:`undefined
                                   ~width ~height
                                   ~title:"SDL2 tutorial"
                                   ~flags:[Window.Resizable] in
                  window <- Some(m_window);
                  let m_renderer = Sdlrender.create_renderer ~win:m_window ~index:(-1) ~flags:[Render.Accelerated; Render.PresentVSync] in
                  renderer <- Some(m_renderer);
                  is_running <- true


    method shutdown () = let _ = match window with
                        | Some(w) -> Sdlwindow.destroy w
                        | None -> ()
              in
              Sdl.quit ();
              ()


    method process_event () = let rec aux_process () =
                                let event = Event.poll_event () in
                                match event with
                                | Some Quit _ -> is_running <- false
                                | Some KeyDown ev -> if ev.scancode == Sdlscancode.ESCAPE then begin
                                                         is_running <- false;
                                                         ()
                                                       end
                                                     else ()
                                | None -> ()
                                | _ -> aux_process ()
      in
      aux_process ()



    method gameloop = let rec loop () =
                        if is_running == true then begin
                            s#process_event  () ;
                            let _ = match renderer with
                            | Some(r) -> Sdlrender.set_draw_color3 r ~r:0 ~g:0 ~b:255 ~a:255 ;
                                         Sdlrender.clear r;
                                         Sdlrender.render_present r;
                                         ()
                            | None -> ()
                            in
                          loop ()
                        end
                        else s#shutdown ()
                      in
                      loop ()

    end
