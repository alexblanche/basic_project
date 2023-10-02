(* Entity functions *)

(* Hash table containing all the entity functions *)
let entity_func_table =
  let (entity_func_list : (string * (entity list -> entity)) list) = [
    (* Internal use only *)
    ("Init",
      (let f nl =
        match nl with
          (* 1 argument: list, 2 arguments: matrix *)
          | [Value x] ->
            if is_int x then
              let zero = Complex (get_complex 0. 0.) in
              ListContent (Array.make (int_of_complex x) zero)
            else failwith "Function error: Init only accepts integers as arguments"
          | [ListContent [|Complex r; Complex c|]] ->
            if is_int r && is_int c then
              let zero = Complex (get_complex 0. 0.) in
              MatContent (Array.make_matrix (int_of_complex r) (int_of_complex c) zero)
            else failwith "Function error: Init only accepts integers as arguments"
          | _ -> failwith "Function error: Init only accepts a number or a list of size 2 as arguments"
      in f)
    );

    ("DIM",
      (let f nl =
        match nl with
          | [ListContent t] ->
            Value (complex_of_int ((Array.length t)/2))
          | [MatContent m] ->
            let row = Array.length m in
            if row = 0
              then ListContent [|Complex (complex_of_float 0.); Complex (complex_of_float 0.)|]
              else
                let col = Array.length m.(0) in
                ListContent [|Complex (complex_of_int row); Complex (complex_of_int col)|]
          | _ -> failwith "Function error: Dim has arity 1 and accepts lists and matrices"
      in f)
    );
    ]
  in
  let t = Hashtbl.create (5 + List.length entity_func_list) in
  List.iter (fun (fname, fdef) -> Hashtbl.add t fname fdef) entity_func_list;
  t;;

(* Application of the functions *)
let apply_entity_func (fname : string) (nl : entity list) : entity =
  try
    (Hashtbl.find entity_func_table fname) nl
  with
    | Not_found -> failwith ("apply_entity_func: Function "^fname^" undefined");;