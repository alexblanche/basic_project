(* Basic enconding *)

[
  (* One-byte symbols *)
  ("\012", "DISP");   (* Black triangle "disp" *)
  ("\013", "EOL");    (* End of line *)
  ("\014", "ASSIGN"); (* -> *)
  ("\034", "QUOTE");  (* double-quote *)
  ("\040", "LPAR");   (* ( *)
  ("\041", "RPAR");   (* ) *)
  ("\048", "0");      (* Int: 48 = 0, 49 = 1, ... *)
  ("\058", "COLON");  (* : (equivalent to EOL) *)
  ("\061", "EQUAL");  (* = *)
  ("\065", "A");      (* Letters: 65 = A, 66 = B, ... *)
  ("\137", "PLUS");   (* plus sign *)
  ("\169", "TIMES");  (* multiplication sign *)

  (* Two-byte symbols *)
  ("\247\000", "IF"); (* If *)
  ("\247\001", "THEN"); (* Then *)
  ("\247\002", "ELSE"); (* Else *)
  ("\247\003", "IFEND"); (* IfEnd *)
];;