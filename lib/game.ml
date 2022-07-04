open Sdl
open Sdlevent

type point2d = { x : float; y : float }

let thickness = 15

class game =
  object (s)
    val mutable window = None
    val mutable renderer = None
    val mutable is_running = false
    val mutable height = 0
    val mutable width = 0
    val mutable ball : point2d = { x= 0.0 ; y = 0.0 }
    val mutable paddle : point2d = { x = 0.0; y = 0.0}
    val mutable keystates = new Key.keystates
    val mutable last_time = 0

    method init w h = let _ = Sdl.init[`VIDEO] in
      let m_window = Sdlwindow.create2
          ~x:`undefined ~y:`undefined
          ~width:w ~height:h
          ~title:"SDL2 tutorial"
          ~flags:[Window.Resizable] in
      window <- Some(m_window);
      width <- w;
      height <- h;
      ball <- {x = float_of_int (width / 2); y = (float_of_int (height / 2))};
      paddle <- {x = float_of_int thickness; y = (float_of_int (height / 2))};
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
                                | Some KeyDown ev -> if ev.ke_repeat == 0 then
                                    keystates#keydown_event ev.scancode
                                | Some KeyUp ev -> keystates#keyup_event ev.scancode
                                | None -> ()
                                | _ -> aux_process ()
      in
      aux_process ()


    method update dt =
      if keystates#was_key_held Sdlscancode.ESCAPE == true then begin
        is_running <- false;
        ()
      end;
      if keystates#was_key_held Sdlscancode.UP == true then begin
        paddle <- {x = paddle.x; y = paddle.y -. 150.0 *. (float_of_int dt) /. 1000.0};
        ()
      end;
      if keystates#was_key_held Sdlscancode.DOWN == true then begin
        paddle <- {x = paddle.x; y = paddle.y +. 150.0 *. (float_of_int dt) /. 1000.0};
        ()
      end;
      if paddle.y > (float_of_int (height - 30 - thickness)) then paddle <- {x = paddle.x; y = (float_of_int (height - 30 - thickness))};
      if paddle.y < (float_of_int (30 + thickness)) then paddle <- {x = paddle.x; y = (float_of_int (30 + thickness))};
      ()


    method gameloop =
      let wall = Sdlrect.make4 ~x:0 ~y:0 ~w:width ~h:thickness in
      let bottom = Sdlrect.make4 ~x:0 ~y:(height - 15) ~w:width ~h:thickness in
      let right = Sdlrect.make4 ~x:(width - thickness) ~y:0 ~w:thickness ~h:height in
      let draw_paddle renderer =
        Sdlrender.fill_rect renderer (Sdlrect.make4 ~x: (int_of_float (paddle.x -. (float_of_int thickness) /. 2.0))  ~y: (int_of_float (paddle.y -. 30.0))  ~w: thickness   ~h: 60 )
      in
      let draw_ball renderer =
        Sdlrender.fill_rect renderer (Sdlrect.make4 ~x: (int_of_float (ball.x -. (float_of_int thickness) /. 2.0)) ~y: (int_of_float (ball.y -. (float_of_int thickness) /. 2.0)) ~w: thickness ~h:thickness) in
      let rec loop () =
        if is_running == true then begin
          let start_time = Sdltimer.get_ticks () in
          let dt = start_time - last_time in
          keystates#begin_frame () ;
          s#process_event  () ;
          s#update dt;
          let _ = match renderer with
            | Some(r) -> Sdlrender.set_draw_color3 r ~r:0 ~g:0 ~b:255 ~a:255 ;
              Sdlrender.clear r;
              Sdlrender.set_draw_color3 r ~r:255 ~g:255 ~b:255 ~a:255;
              Sdlrender.fill_rect  r wall;
              Sdlrender.fill_rect  r bottom;
              Sdlrender.fill_rect  r right;
              draw_paddle r;
              draw_ball r;
              Sdlrender.render_present r;
              ()
            | None -> ()
          in
          last_time <- start_time;
          loop ()
        end
        else s#shutdown ()
      in
      loop ()

  end
