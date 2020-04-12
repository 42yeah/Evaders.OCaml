open Tsdl
open Unix


let unwrap what =
  match what with
  | Error (`Msg e) -> prerr_endline ("Unwrap failed: " ^ e); exit 2
  | Ok w -> w

let round x = (int_of_float (floor (x +. 0.5)))

type plane = { mutable x: float; mutable y: float; }

let _ =
  unwrap (Sdl.init Sdl.Init.everything);
  let (w, r) = unwrap (Sdl.create_window_and_renderer ~w:640 ~h:480 Sdl.Window.shown) in
  let plane_surfaces = unwrap (Sdl.load_bmp "assets/planes.bmp") in
  let plane_textures = unwrap (Sdl.create_texture_from_surface r plane_surfaces) in
  Sdl.set_window_title w "evaders";

  Random.init (round (gettimeofday ()));
  let player = { x = 314.0; y = 450.0 } in
  let speed = 128.0 in
  let enemies = ref [{ x = (Random.float 640.0); y = 0.0 }] in
  let last_instant = ref (gettimeofday ()) in
  let accu = ref 0.0 in
  let revenge = ref 0 in
  let alive = ref true in
  let rec loop w r =
    let this_instant = gettimeofday () in
    let delta_time = this_instant -. !last_instant in
    last_instant := this_instant;
    accu := !accu +. delta_time;
    unwrap (Sdl.render_clear r);
    Sdl.pump_events ();
    if Sdl.has_event Sdl.Event.quit || not !alive then () else (
      let keyboard_states = Sdl.get_keyboard_state () in
      if keyboard_states.{Sdl.Scancode.w} = 1 then (
        player.y <- player.y -. delta_time *. speed
      );
      if keyboard_states.{Sdl.Scancode.a} = 1 then (
        player.x <- player.x -. delta_time *. speed
      );
      if keyboard_states.{Sdl.Scancode.s} = 1 then (
        player.y <- player.y +. delta_time *. speed
      );
      if keyboard_states.{Sdl.Scancode.d} = 1 then (
        player.x <- player.x +. delta_time *. speed
      );
      let player_rect = Sdl.Rect.create ~x:(round player.x) ~y:(round player.y) ~w:16 ~h:16 in
      unwrap (Sdl.render_copy 
        ~src:(Sdl.Rect.create ~x:0 ~y:0 ~w:16 ~h:16)
        ~dst:player_rect
        r plane_textures);
      List.iter(fun enemy -> (
        enemy.y <- enemy.y +. delta_time *. speed;
        let enemy_rect = Sdl.Rect.create ~x:(round enemy.x) ~y:(round enemy.y) ~w:16 ~h:16 in
        if Sdl.has_intersection player_rect enemy_rect then (
          alive := false
        );
        unwrap (Sdl.render_copy 
        ~src:(Sdl.Rect.create ~x:16 ~y:0 ~w:16 ~h:16)
        ~dst:enemy_rect
        r plane_textures);
      )) !enemies;
      enemies := List.filter (fun enemy ->
        if enemy.y > 500.0 then (
          revenge := !revenge + 1;
          false
        ) else true
      ) !enemies;
      if !accu > (0.1 -. ((float !revenge) /. 1000.0 *. 0.1)) then (
        if !revenge > 0 then (
          enemies := { x = (Random.float 640.0); y = 0.0 }::
          { x = (Random.float 640.0); y = 0.0 }::!(enemies);
          revenge := !revenge - 1;
        );
        accu := 0.0;
      );
      Sdl.render_present r;
      loop w r
    ) in
  loop w r;
  print_endline ("Game over! Your score: " ^ ((string_of_int !revenge)));
  Sdl.quit ();
  Gc.compact ()
