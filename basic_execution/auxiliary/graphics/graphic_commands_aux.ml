(* Auxiliary functions to execute_graphic_commands.ml *)

(** Graphs **)

(** Graph Y (=, >=, <=, >, <) **)

let graphy (ren : Sdlrender.t) (p : parameters) (text_screen : bool ref)
  (command : string) (e : num_expr) : unit =

  refresh_update ren p !text_screen;
  text_screen := false;
  let t =
    match command with
    (* t mod 2 = 0 <=> EQ type
       t <= 2 (and <> 0) <=> "greater than" type
       t >= 3 <=> "less than" type *)
      | "GRAPHYEQ" -> 0
      | "GRAPHYG" -> 1
      | "GRAPHYGEQ" -> 2
      | "GRAPHYL" -> 3
      | "GRAPHYLEQ" -> 4
      | _ -> -1
  in
  let pxmin = access_real_var p.var xmin_index in
  (* let step = (pxmax -. pxmin) /. 126. in *)
  let step = access_real_var p.var xdot_index in
  (* Xmin -> X *)
  set_var p.var 23 (complex_of_float pxmin);
  let z = eval_num p e in
  let last_j = ref (rescale_y p z.re) in

  for i = 1 to 127 do
    let z = eval_num p e in
    let j = rescale_y p z.re in
    (if j >= 1 && j <= 63 then
      (* normal line if =, >= or <=, dot line if > or < *)
      let (jmin, jmax) =
        if !last_j < j
          then (max (!last_j + 1) 1, j)
        else if !last_j > j
          then (j, min (!last_j - 1) 63)
        else (j, j)
      in
      (fline ren p i jmax i jmin
        (if t mod 2 = 0
          then Some StyleNormal
          else Some StyleDot);
      (* Refreshing at each step in Casio Basic *)
      if t = 0 then
        refresh ren)
    else if !last_j >= 1 && !last_j <= 63 then
      let (jmin, jmax) =
        if j <= 0 then (1, max (!last_j - 1) 1) else (min (!last_j + 1) 63, 63)
      in
      (fline ren p i jmax i jmin
        (if t mod 2 = 0
          then Some StyleNormal
          else Some StyleDot);
      (* Refreshing at each step in Casio Basic *)
      if t = 0 then
        refresh ren)
    );
    last_j := j;
    if t<>0 then
      (if t<=2 (* > or >= *) then
        (let acc = ref [] in
        let parity = (i + 1) mod 2 in
        for k = max (j+1) 1 to 63 do
          if k mod 2 = parity then
            acc := (ploton_rect i (64-k)) :: !acc
        done;
        let rect_t = Array.of_list !acc in
        Array.iter (fun r -> write_in_matrix gscreen r) rect_t;
        Sdlrender.fill_rects ren rect_t;
        refresh ren
        )
      else (* < or <= *)
        (let acc = ref [] in
        let parity = (i + 1) mod 2 in
        for k = min (j-1) 63 downto 1 do
          if k mod 2 = parity then
            acc := (ploton_rect i (64-k)) :: !acc
        done;
        let rect_t = Array.of_list !acc in
        Array.iter (fun r -> write_in_matrix gscreen r) rect_t;
        Sdlrender.fill_rects ren rect_t;
        refresh ren
        )
      );
    (* X + step -> X *)
    set_real_var p.var 23 ((access_real_var p.var 23) +. step)
  done;;

(* GraphX=, >, <, >=, <= *)
let graphx (ren : Sdlrender.t) (p : parameters) (text_screen : bool ref)
  (command : string) (e : num_expr) : unit =

  refresh_update ren p !text_screen;
  text_screen := false;
  let t =
    match command with
    (* t mod 2 = 0 <=> EQ type
       t <= 2 (and <> 0) <=> "greater than" type
       t >= 3 <=> "less than" type *)
      | "GRAPHXEQ" -> 0
      | "GRAPHXG" -> 1
      | "GRAPHXGEQ" -> 2
      | "GRAPHXL" -> 3
      | "GRAPHXLEQ" -> 4
      | _ -> -1
  in
  let pymin = access_real_var p.var ymin_index in
  let pymax = access_real_var p.var ymax_index in
  let step = (pymax -. pymin) /. 62. in
  (* Ymin -> Y *)
  set_var p.var 24 (complex_of_float pymin);
  let z = eval_num p e in
  let last_i = ref (rescale_x p z.re) in

  for j = 63 downto 1 do
    let z = eval_num p e in
    let i = rescale_x p z.re in
    (if i >= 1 && i <= 127 then
      (* normal line if =, >= or <=, dot line if > or < *)
      let (imin, imax) =
        if !last_i < i
          then (max (!last_i + 1) 1, i)
        else if !last_i > i
          then (i, min (!last_i - 1) 127)
        else (i, i)
      in
      (fline ren p imin (64-j) imax (64-j)
        (if t mod 2 = 0
          then Some StyleNormal
          else Some StyleDot);
      (* Refreshing at each step in Casio Basic *)
      if t = 0 then
        refresh ren)
    else if !last_i >= 1 && !last_i <= 127 then
      let (imin, imax) =
        if i <= 0 then (1, max (!last_i - 1) 1) else (min (!last_i + 1) 127, 127)
      in
      (fline ren p imax (64-j) imin (64-j)
        (if t mod 2 = 0
          then Some StyleNormal
          else Some StyleDot);
      (* Refreshing at each step in Casio Basic *)
      if t = 0 then
        refresh ren)
    );
    last_i := i;
    if t<>0 then
      (if t<=2 (* > or >= *) then
        (let acc = ref [] in
        let parity = (j + 1) mod 2 in
        for k = max (i+1) 1 to 127 do
          if k mod 2 = parity then
            acc := (ploton_rect k j) :: !acc
        done;
        let rect_t = Array.of_list !acc in
        Array.iter (fun r -> write_in_matrix gscreen r) rect_t;
        Sdlrender.fill_rects ren rect_t;
        refresh ren
        )
      else (* < or <= *)
        (let acc = ref [] in
        let parity = (j + 1) mod 2 in
        for k = min (i-1) 127 downto 1 do
          if k mod 2 = parity then
            acc := (ploton_rect k j) :: !acc
        done;
        let rect_t = Array.of_list !acc in
        Array.iter (fun r -> write_in_matrix gscreen r) rect_t;
        Sdlrender.fill_rects ren rect_t;
        refresh ren
        )
      );
    (* Y + step -> Y *)
    set_real_var p.var 24 ((access_real_var p.var 24) +. step)
  done;;



(** ViewWindow function **)

(* ViewWindow treatment with all 6 parameters *)
let viewWindow (ren : Sdlrender.t) (p : parameters) 
  (xmin : float) (xmax : float) (xscl : float)
  (ymin : float) (ymax : float) (yscl : float) : unit =

  set_real_var p.var xmin_index xmin;
  set_real_var p.var xmax_index xmax;
  set_real_var p.var xscl_index xscl;
  set_real_var p.var ymin_index ymin;
  set_real_var p.var ymax_index ymax;
  set_real_var p.var yscl_index yscl;
  set_real_var p.var xdot_index ((xmax -. xmin) /. 126.);
  wipe gscreen;
  draw_window ren p;
  background_changed := true
  (* No refresh: the will be refreshed when the first object will be drawn *)
;;

(* Calls viewWindow with 6 parameters or less *)
let partial_vwin (ren : Sdlrender.t) (p : parameters) (l : float list) : unit =
  let xmax = access_real_var p.var xmax_index in
  let xscl = access_real_var p.var xscl_index in
  let ymin = access_real_var p.var ymin_index in
  let ymax = access_real_var p.var ymax_index in
  let yscl = access_real_var p.var yscl_index in
  match l with
    | [nxmin]                                    -> viewWindow ren p nxmin xmax xscl ymin ymax yscl
    | [nxmin; nxmax]                             -> viewWindow ren p nxmin nxmax xscl ymin ymax yscl
    | [nxmin; nxmax; nxscl]                      -> viewWindow ren p nxmin nxmax nxscl ymin ymax yscl
    | [nxmin; nxmax; nxscl; nymin]               -> viewWindow ren p nxmin nxmax nxscl nymin ymax yscl
    | [nxmin; nxmax; nxscl; nymin; nymax]        -> viewWindow ren p nxmin nxmax nxscl nymin nymax yscl
    | [nxmin; nxmax; nxscl; nymin; nymax; nyscl] -> viewWindow ren p nxmin nxmax nxscl nymin nymax nyscl
    | _ -> failwith "Graphic error: partial_vwin expects a list of between 1 and 6 float numbers as argument"
;;


(** Circle **)
let circle (ren : Sdlrender.t) (p : parameters)
  (x : float) (y : float) (r : float) (style : style option) : unit =
  
  let n = 15 in
  let two_pi_over_n = 2. *. Float.pi /. (float_of_int n) in
  for k = 0 to n-1 do
    let a1 = rescale_x p (x +. r *. cos (two_pi_over_n *. (float_of_int k))) in
    let a2 = rescale_x p (x +. r *. cos (two_pi_over_n *. (float_of_int (k+1)))) in
    let b1 = rescale_y p (y +. r *. sin (two_pi_over_n *. (float_of_int k))) in
    let b2 = rescale_y p (y +. r *. sin (two_pi_over_n *. (float_of_int (k+1)))) in
    if a1 >= 1 && a1 <= 127 && a2 >= 1 && a2 <= 127
      && b1 >= 1 && b1 <= 63 && b2 >= 1 && b2 <= 63 then
        (fline ren p a1 b1 a2 b2 style;
        refresh_update ren p false;
        if slowdown_condition () then
          Unix.sleepf (timer.fline /. 2.));
  done;;

