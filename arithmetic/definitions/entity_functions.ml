(* Entity functions *)

(* Auxiliary functions *)

let max_t (t : complex array) : complex =
  let n = Array.length t in
  if n = 0
    then failwith "Function error: Max expects a non-empty list";
  let rec aux m i =
    if i = n then
      m
    else
      if t.(i) > m
        then aux t.(i) (i+1)
        else aux m (i+1)
  in
  aux t.(0) 1;;

let min_t (t : complex array) : complex =
  let n = Array.length t in
  if n = 0
    then failwith "Function error: Max expects a non-empty list";
  let rec aux m i =
    if i = n then
      m
    else
      if t.(i) < m
        then aux t.(i) (i+1)
        else aux m (i+1)
  in
  aux t.(0) 1;;


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
            Value (complex_of_int (Array.length t))
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

    ("AUGMENT",
      (let f nl =
        match nl with
          | [ListContent t1; ListContent t2] ->
            ListContent (Array.append t1 t2)
          | [MatContent m1; MatContent m2] ->
            let r1 = Array.length m1 in
            let r2 = Array.length m2 in
            if r1 = r2 && r1 <> 0 && r2 <> 0 then
              let c1 = Array.length m1.(0) in
              let c2 = Array.length m2.(0) in
              let zero = Complex (complex_of_float 0.) in
              let m = Array.make_matrix r1 (c1+c2) zero in
              (try
                for i = 0 to r1-1 do
                  for j = 0 to c1-1 do
                    m.(i).(j) <- m1.(i).(j)
                  done
                done;
                for i = 0 to r1-1 do
                  for j = 0 to c1-1 do
                    m.(i).(j+c1) <- m2.(i).(j)
                  done
                done;
                MatContent m
              with
                | _ -> failwith "Function error: Wrong dimensions for function Augment")
            else failwith "Function error: Wrong dimensions for function Augment"
          | _ -> failwith "Function error: Augment has arity 2 and accepts lists and matrices"
      in f)
    );

    ("TRN",
      (let f nl =
        match nl with
          | [MatContent m] ->
            let r = Array.length m in
            if r = 0 then MatContent [||]
            else
              let c = Array.length m.(0) in
              let zero = Complex (complex_of_float 0.) in
              let tm = Array.make_matrix c r zero in
              for i = 0 to r-1 do
                for j = 0 to c-1 do
                  tm.(j).(i) <- m.(i).(j)
                done
              done;
              MatContent tm
          | _ -> failwith "Function error: Trn has arity 1 and only accepts matrices"
      in f)
    );

    ("IDENTITY",
      (let f nl =
        match nl with
          | [Value z] ->
            if is_int z then
              let n = int_of_complex z in
              let zero = Complex (complex_of_float 0.) in
              let id = Array.make_matrix n n zero in
              (for i = 0 to n-1 do
                id.(i).(i) <- Complex (complex_of_float 1.)
              done;
              MatContent id)
            else failwith "Function error: Augment has arity 1 and only accepts integer arguments"
          | _ -> failwith "Function error: Augment has arity 1 and only accepts integer arguments"
      in f)
    );

    ("SUM",
      (let f nl =
        match nl with
          | [ListContent t] ->
            let s =
              Array.fold_left
                (fun e1 e2 ->
                  match e1,e2 with
                    | Complex z1, Complex z2 -> Complex (Complex.add z1 z2)
                    | _ -> failwith "Sum: wrong type")
                (Complex (complex_of_float 0.)) t
            in
            (match s with
              | Complex z -> Value z
              | _ -> failwith "Sum: wrong type")
          | _ -> failwith "Function error: Sum has arity 1 and only accepts lists"
      in f)
    );

    ("PROD",
      (let f nl =
        match nl with
        | [ListContent t] ->
          let p =
            Array.fold_left
              (fun e1 e2 ->
                match e1,e2 with
                  | Complex z1, Complex z2 -> Complex (Complex.mul z1 z2)
                  | _ -> failwith "Function error: Prod has arity 1 and only accepts lists")
              (Complex (complex_of_float 1.)) t
          in
          (match p with
            | Complex z -> Value z
            | _ -> failwith "Function error: Prod has arity 1 and only accepts lists")
          | _ -> failwith "Function error: Prod has arity 1 and only accepts lists"
      in f)
    );

    ("PERCENT",
      (let f nl =
        match nl with
          | [ListContent t] ->
            if Array.exists (function (Complex z) -> z.im <> 0. | _ -> failwith "Percent: wrong type") t
              then failwith "Function error: Percent has arity 1 and only accepts lists of real numbers"
            else
              let tre =
                Array.map
                (function
                  | (Complex z) -> z.re
                  | _ -> failwith "Function error: Percent has arity 1 and only accepts lists of real numbers")
                t
              in
              let sum = Array.fold_left (fun a b -> a +. b) 0. tre in
              ListContent
                (Array.init (Array.length t)
                  (fun i -> Complex (complex_of_float (tre.(i) /. sum))))
          | _ -> failwith "Function error: Percent has arity 1 and only accepts lists of real numbers"
      in f)
    );

    ("MAX",
      (let f nl =
        match nl with
          | [ListContent t] ->
            let ct =
              Array.map
              (function
                | (Complex z) -> z
                | _ -> failwith "Function error: Max has arity 1 or 2 and only accepts lists")
              t
            in
            Value (max_t ct)
          | [ListContent t1; ListContent t2] ->
            let n1 = Array.length t1 in
            if n1 = Array.length t2
              then failwith "Function error: Max expects two lists of the same size";
            ListContent
              (Array.init
                n1
                (fun i ->
                  match t1.(i), t2.(i) with
                    | Complex z1, Complex z2 ->
                      if z1 >= z2
                        then Complex z1
                        else Complex z2
                    | _ -> failwith "Max: wrong type"))
          | _ -> failwith "Function error: Max has arity 1 or 2 and only accepts lists"
      in f)
    );

    ("MIN",
      (let f nl =
        match nl with
          | [ListContent t] ->
            let ct =
              Array.map
              (function
                | (Complex z) -> z
                | _ -> failwith "Function error: Min has arity 1 or 2 and only accepts lists")
              t
            in
            Value (min_t ct)
          | [ListContent t1; ListContent t2] ->
            let n1 = Array.length t1 in
            if n1 = Array.length t2
              then failwith "Function error: Min expects two lists of the same size";
            ListContent
              (Array.init
                n1
                (fun i ->
                  match t1.(i), t2.(i) with
                    | Complex z1, Complex z2 ->
                      if z1 <= z2
                        then Complex z1
                        else Complex z2
                    | _ -> failwith "Min: wrong type"))
          | _ -> failwith "Function error: Min has arity 1 or 2 and only accepts lists"
      in f)
    );

    ("CUML",
      (let f nl =
        match nl with
          | [ListContent t] ->
            let st = Array.make (Array.length t) t.(0) in
            let _ =
              let i = ref 0 in
              Array.fold_left
                (fun zs e ->
                  match e with
                    | Complex ze ->
                      let zs' = Complex.add zs ze in
                      (st.(!i) <- Complex zs';
                      incr i;
                      zs')
                    | _ -> failwith "Cuml: wrong type")
                (complex_of_float 0.) t
            in
            ListContent st
          | _ -> failwith "Function error: Cuml has arity 1 and only accepts lists"
      in f)
    );

    ("DELTALIST",
      (let f nl =
        match nl with
          | [ListContent t] ->
            let n = Array.length t in
            ListContent (Array.init (n-1)
              (fun i ->
                match t.(i+1), t.(i) with
                  | Complex zi1, Complex zi -> Complex (Complex.sub zi1 zi)
                  | _ -> failwith "DeltaList: wrong type"))
          | _ -> failwith "Function error: DeltaList has arity 1 and only accepts lists"
      in f)
    );

    ("MEAN",
      (let f nl =
        match nl with
          | [ListContent t] ->
            let s =
              Array.fold_left
                (fun e1 e2 ->
                  match e1,e2 with
                    | Complex z1, Complex z2 -> Complex (Complex.add z1 z2)
                    | _ -> failwith "Mean: wrong type")
                (Complex (complex_of_float 0.)) t
            in
            (match s with
              | Complex z -> Value (scal (1./.(float_of_int (Array.length t))) z)
              | _ -> failwith "Mean: wrong type")
          | [ListContent t1; ListContent t2] ->
            let n1 = Array.length t1 in
            if n1 = Array.length t2
              then failwith "Function error: Mean expects two lists of the same size";
            let weighted_sum =
              let wsum = ref (complex_of_float 0.) in
              Array.iter2
                (fun e1 e2 ->
                  match e1,e2 with
                    | Complex z, Complex weight ->
                      wsum := Complex.add !wsum (Complex.mul z weight)
                    | _ -> failwith "Mean: wrong type")
                t1 t2;
              !wsum
            in
            let sum_w =
              Array.fold_left
                (fun s e ->
                  match e with
                    | Complex z ->
                      if z.im = 0.
                        then s +. z.re
                        else failwith "Mean: the weights in the second list should be real numbers"
                    | _ -> failwith "Mean: wrong type")
                0. t2
            in
            Value (scal (1./.sum_w) weighted_sum)
          | _ -> failwith "Function error: Mean has arity 1 or 2 and only accepts lists"
      in f)
    );

    ("LISTTOMAT",
      (let f nl =
        let col = List.length nl in
        let row =
          match nl with
            | (ListContent t)::_ -> Array.length t
            | _ -> failwith "ListToMat: at least one argument is expected"
        in
        let m = Array.make_matrix row col (Complex (complex_of_float 0.)) in
        let rec aux nl i =
          match nl with
            | (ListContent t)::nt ->
              let n = Array.length t in
              (if n <> row
                then failwith "ListToMat: the arguments should be lists of the same size"
              else
                for j = 0 to n-1 do
                  m.(j).(i) <- t.(j)
                done;
                aux nt (i+1))
            | [] -> ()
            | _ -> failwith "ListToMat: wrong type"
        in
        aux nl 0;
        MatContent m
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