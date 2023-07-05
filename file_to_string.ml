(* Conversion functions *)

open String;;
exception End_of_loop;;

(* TODO: à partir de OCaml 4.0.*, les Strings sont non mutables ! *)
(* Il faut changer le programme... *)


(* let file_to_string nom_du_fichier =
	let f = open_in nom_du_fichier in
	let l = in_channel_length f in
	let chaine = make l '\000'
	and cpt = ref 0 in
	while !cpt < l do
		(* print_string "\npos: "; print_int (pos_in f); print_newline(); *)
		try
			begin
				while true do
					let c = input_char f in
					chaine.[!cpt] <- c;
					incr cpt;
				done;
				raise End_of_loop;
			end
		with _ (* End_of_file *) -> (incr cpt)(* let k = pos_in f in
											 if k < l then
											 	begin
											 		(* print_string "/026"; *)
											 		incr cpt;
											 		(* chaine := !chaine ^ (make 1 '\026'); *)
											 		chaine.[(!cpt)-1] <- '\026';
											 		(****)
											 		seek_in f (k+1)
											 	end;*)
	done;
	close_in f;
	(*!*)chaine;;
ne fonctionne pas vraiment � tous les coups, mais presque! *)

let file_to_string (file_name : string) : string =
	let f = open_in file_name in
	let l = in_channel_length f in
	(* let chaine = make l '\000' in
	for i = 0 to l-1 do
		chaine.[i] <- input_char f;
	done; *)
	let chaine = init l (fun _ -> input_char f) in
	close_in f;
	chaine;;

let string_to_code_array (s : string) : int array =
	Array.init (length s) (fun i -> Char.code s[i]);;

let file_to_code_array (file_name : string) : int array =
	string_to_code_array (file_to_string file_name);;

(* exception Not_equal;;

let string_equal (s1 : string) (s2 : string) : bool =
	let l1 = length s1 in
	let l2 = length s2 in
	if l1<> l2
		then false
	else
		begin
			try
				for i=0 to l1-1 do
					if s1.[i] <> s2.[i] then raise Not_equal
				done;
				true;
			with Not_equal -> false
		end
;; *)
(* Present in String since 4.03.0 *)