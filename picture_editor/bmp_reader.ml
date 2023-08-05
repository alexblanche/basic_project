(* Simplified 24 bits BMP image reader *)

#use "topfind"
#require "graphics"
open Graphics;;

(** Types **)

(* Types for word (2 bytes) and dword (4 bytes) *)
type word  = int;;
type dword = int;;

(* Types for color matrices *)
type image_mat = color array array;;

(** Reading functions **)

(* Reading a Word and a DWord.*)
(* Least significant bit first (Little Endian) *) 
let read_dword channel =
  let a = input_byte channel in
  let b = input_byte channel in
  let c = input_byte channel in
  let d = input_byte channel in
  a + 256*(b + 256*(c + 256*d));;

let read_word channel =
  let a = input_byte channel in
  let b = input_byte channel in
  a + (256*b);;

let byte_repr (n : int) : bool array =
  let t = Array.make 8 false in
  let a = ref n in
  for i = 0 to 7 do
    t.(i) <- !a mod 2 = 1;
    a := !a /2
  done;
  t;;
  
(** Reading the file **)

(* Skipping the File Header *)
let skip_file_header channel : unit =
  let _ = read_word channel in (* Type *)
  let _ = read_dword channel in (* Size *)
  let _ = read_word channel in (* Reserved 1 *)
  let _ = read_word channel in (* Reserved 2 *)
  let _ = read_dword channel in (* Offset *)
  ();;
  
(* Extracting the width and the height of the image from the Info Header, skipping the rest *)
let skip_info_header channel : int * int =
  let _ = read_dword channel in (* Size *)
  let width = read_dword channel in (* Width *)
  let height = read_dword channel in (* Height *)
  let _ = read_word channel in (* Number of color planes *)
  let _ = read_word channel in (* Number of bits per pixel *)
  let _ = read_dword channel in (* Compression method used *)
  let _ = read_dword channel in (* Size of the image *)
  let _ = read_dword channel in (* Horizontal resolution *)
  let _ = read_dword channel in (* Vertical resolution *)
  let _ = read_dword channel in (* Number of colors used *)
  let _ = read_dword channel in (* Number of important colors used *)
  (width, height);;
 
(* Reading the pixels of the image.
  Returns an image (pixel matrix) of height lines and width columns.
  Each cell contains a tuple (r,g,b). *)

(* Padding to reach the next multiple of 4 *)  
let padd w =
  let r = (3*w) mod 4 in
  if r = 0
    then 0
    else 4-r;;

let read_pixels (width : int) (height : int) (channel : in_channel) : image_mat =
  let p = padd width in
  let m = Array.make_matrix height width black in
  for j = 0 to height-1 do
    for i = 0 to width-1 do
      let b = input_byte channel in
      let g = input_byte channel in
      let r = input_byte channel in
      m.(height-j-1).(i) <- rgb r g b
    done;
    for i = 1 to p do
      let _ = input_byte channel in ()
    done;
  done;
  m;;

(* Reads a RGB BMP picture *)
let read_bmp (filename : string) : image_mat =
  let channel = open_in_bin filename in
  let _ = skip_file_header channel in
  let (width, height) = skip_info_header channel in
  let m = read_pixels width height channel in
  close_in channel;
  m;;

(* Reads a monochromatic BMP picture *)
(* In particular, the ones produced by Screen captures of FA-124 *)
let read_mono_bmp (filename : string) : bool array array =
  let channel = open_in_bin filename in
  let _ = skip_file_header channel in
  let (width, height) = skip_info_header channel in
  let m = Array.make_matrix height width false in
  let nbcol = width/8 in
  let a = ref 0 in
  try
    for i=0 to 7 do
      let _ = input_byte channel in ()
    done;
    for j = 0 to height-1 do
      for i = 0 to nbcol-1 do
        a := input_byte channel;
        for k = 7 downto 0 do
          m.(j).(8*i+k) <- (!a mod 2) = 1;
          a := !a/2
        done;
      done;
    done;
    close_in channel;
    m;
  with
    | End_of_file -> close_in channel; failwith "read_mono_bmp: Out of bounds"
  ;;

(** Display **)

(* Displays the image on the screen as a picture *)
let view_image (m : image_mat) : unit =
  let height = Array.length m in
  let width = Array.length m.(0) in
  let margin = 40 in
  
  open_graph "";
  let pict_m = make_image m in
  resize_window (2*margin + width) (2*margin + height);
  set_window_title "Image viewer";
  auto_synchronize false;
  draw_image pict_m margin margin;
  synchronize();
  let _ = read_key() in
  close_graph();;
