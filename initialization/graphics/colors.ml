(* Colors used in graphic display *)

(* Base colors *)
let white = (255, 255, 255);;
let light_gray = (200, 200, 200);;
let dark_gray = (60, 60, 60);;
let gray = (120, 120, 148);;
let black = (0, 0, 0);;

(* Type for color container *)
type color_constants =
{
	mutable pixels     : int * int * int;
	mutable background : int * int * int;
	mutable writing    : int * int * int;
  mutable grid       : int * int * int;
}

(* Colors used for graphic display *)
let colors =
{
	pixels     = black;
	background = white;
	writing    = dark_gray;
  grid       = gray;
};;

(* Go from light mode to dark mode, or vice versa *)
let switch_color_mode () : unit =
  if colors.pixels = black then
    (colors.pixels    <- white;
    colors.background <- black;
    colors.writing    <- light_gray)
  else
    (colors.pixels    <- black;
    colors.background <- white;
    colors.writing    <- dark_gray);;