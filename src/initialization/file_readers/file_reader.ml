(* Conversion functions *)

let file_to_string (file_name : string) : string =
	let channel = open_in file_name in
	let l = in_channel_length channel in
	let s = String.init l (fun _ -> input_char channel) in
	close_in channel;
	s;;

let string_to_code_array (s : string) : int array =
	Array.init (String.length s) (fun i -> Char.code s.[i]);;

let file_to_code_array (file_name : string) : int array =
	string_to_code_array (file_to_string file_name);;

let file_to_code_char_array (file_name : string) : (int * char) array =
	let m = file_to_code_array file_name in
	Array.init (Array.length m) (fun i -> (m.(i), Char.chr m.(i)));;

let display_whole_tab (t : (int * char) array) : unit =
	let print_elt (i,c) =
		print_string "(";
		print_int i;
		print_string ", ";
		print_string (Char.escaped c);
		print_string "); "
	in
	Array.iteri (fun i (j,c) -> (if i mod 6 = 0 then print_newline ()); print_elt (j,c)) t;
	print_newline ();;