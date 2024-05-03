(* The Sdlrender.destroy is missing from the OCamlSDL2 library, causing a Segmentation fault.
  I do the binding myself here. *)

external destroy_renderer : Sdlrender.t -> unit
  = "caml_SDL_DestroyRenderer"