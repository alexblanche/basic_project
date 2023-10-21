(* Timing functions and values *)

(* If this condition is false, the sleepf functions are not called
  (i.e. the emulator runs as fast as possible) *)
let slowdown_condition () =
  !key_pressed <> Tab;;

type timer_constants =
{
  general  : float;
  locate   : float;
  fline    : float;
  plot     : float;
  drawstat : float;
  text     : float;
}

let timer =
{
  general  = 0.001253133;
  locate   = 0.013;
  fline    = 0.045;
  plot     = 0.04;
  drawstat = 0.;
  text     = 0.08;
};;