(* Conversion functions *)

open String;;

let file_to_string (file_name : string) : string =
	let f = open_in file_name in
	let l = in_channel_length f in
	let chaine = init l (fun _ -> input_char f) in
	close_in f;
	chaine;;

let string_to_code_array (s : string) : int array =
	Array.init (length s) (fun i -> Char.code s[i]);;

let file_to_code_array (file_name : string) : int array =
	string_to_code_array (file_to_string file_name);;