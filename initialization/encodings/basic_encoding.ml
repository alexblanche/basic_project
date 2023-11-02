(* Encoding of characters and commands and visual representation *)

(** Accessing the encodings **)
(* /!\ The characters are encoded with a string like "\001".
  Char.escaped (Char.chr 1) returns the string "\\001".
  To obtain the right string from the integer i (during lexing), use this function *)
let enco_of_int (i : int) : string = 
  String.init 1 (fun _ -> Char.chr i);;

(* For two-byte characters *)
let enco_of_two_int (i1 : int) (i2 : int) : string =
  String.init 2 (fun i -> Char.chr (if i=0 then i1 else i2));;


(** Encoding of characters and commands **)

(* Main characters encodings:
 32 = SPACE
 48 ... 57 = 0 ... 9
 65 ... 90 = A ... Z
 97 ... 122 = a ... z
 44 = ,
 46 = .
*)


(* Most commands *)
(* Left in comments: the bytes that do not occur in the commands or the symbols *)
let commands = [
  (* One-byte symbols *)
  (* \000 *)
  ("\001", "FEMTO");  (* Lower-case f (femto) (Optn > Esym) *)
  ("\002", "PICO");   (* Lower-case p (pico) (Optn > Esym) *)
  ("\003", "NANO");   (* nu (nano) (Optn > Esym) *)
  ("\004", "MICRO");  (* mu (micro) (Optn > Esym) *)
  ("\005", "MILLI");  (* Lower-case m (milli) (Optn > Esym) *)
  ("\006", "KILO");   (* Lower-case k (kilo) (Optn > Esym) *)
  ("\007", "MEGA");   (* Upper-case M (mega) (Optn > Esym) *)
  ("\008", "GIGA");   (* Upper-case G (giga) (Optn > Esym) *)
  ("\009", "TERA");   (* Upper-case T (tera) (Optn > Esym) *)
  ("\010", "PETA");   (* Upper-case P (peta) (Optn > Esym) *)
  ("\011", "EXA");    (* Upper-case E (exa) (Optn > Esym) *)
  ("\012", "DISP");   (* Black triangle "disp" *)
  ("\013", "EOL");    (* End of line *)
  ("\014", "ASSIGN"); (* -> *)
  ("\015", "TIMESTENPOWER"); (* E (x10^ sign) *)
  ("\016", "LEQ");    (* Less or equal <= *)
  ("\017", "DIFFERENT"); (* Different (not-equal) sign *)
  ("\018", "GEQ");    (* Greater or equal >= *)
  ("\019", "IMPL");   (* => *)
  ("\020", "FONE");   (* f1 (Catalog) *)
  ("\021", "FTWO");   (* f2 (Catalog) *)
  ("\022", "FTHREE"); (* f3 (Catalog) *)
  ("\023", "FFOUR");  (* f4 (Catalog) *)
  ("\024", "FFIVE");  (* f5 (Catalog) *)
  ("\025", "FSIX");   (* f6 (Catalog) *)
  (* \026 ... \031 *)
  ("\034", "QUOTE");  (* Double-quote *)
  ("\040", "LPAR");   (* ( *)
  ("\041", "RPAR");   (* ) *)
  (* Int: 48 = 0, 49 = 1, ..., 57 = 9 *)
  ("\058", "COLON");  (* : (equivalent to EOL) *)
  ("\060", "LESS");   (* < *)
  ("\061", "EQUAL");  (* = *)
  ("\062", "GREATER"); (* > *)
  ("\063", "QMARK");  (* ? (Question mark) *)
  (* Letters: 65 = A, 66 = B, ..., 90 = Z *)
  ("\091", "LSQBRACKET"); (* [ *)
  ("\093", "RSQBRACKET"); (* ] *)
  (* Letters: 97 = a, 66 = B, ..., 122 = z *)
  ("\123", "LBRACKET"); (* { *)
  ("\125", "RBRACKET"); (* } *)
  (* \127: first byte of two-byte symbols *)
  ("\128", "POLPAR"); (* Pol( (Optn > Angl) *)
  ("\129", "SIN");    (* sin *)
  ("\130", "COS");    (* cos *)
  ("\131", "TAN");    (* tan *)
  (* \132 *)
  ("\133", "LN");     (* ln *)
  ("\134", "SQRT");   (* Square root sign *)
  ("\135", "UMINUS"); (* Unary minus sign *)
  ("\136", "THICKP"); (* Thick P (Optn > Prob) *)
  ("\137", "PLUS");   (* Plus sign *)
  (* \138 *)
  ("\139", "POWER2"); (* ^2 *)
  ("\140", "COORDSIGN"); (* Small white square (Optn > Angl) *)
  ("\141", "INTEGRALPAR"); (* Integral sign + left parenthesis (Optn > Calc) *)
  ("\142", "MMOD"); (* Mod (Catalog) (NOT the MOD( function) *)
  ("\143", "SIGMAXSQUARE"); (* (sigma)x2 (Catalog) *)
  (* \144 *)
  ("\145", "ARCSIN"); (* sin^-1 *)
  ("\146", "ARCCOS"); (* cos^-1 *)
  ("\147", "ARCTAN"); (* tan^-1 *)
  (* \148 *)
  ("\149", "LOG");    (* log *)
  ("\150", "CURT");   (* Cubic root *)
  ("\151", "ABS");    (* Absolute value (Optn > Cplx) *)
  ("\152", "THICKC"); (* Thick C (Optn > Prob) *)
  ("\153", "MINUS");  (* Minus sign *)
  (* \154 *)
  ("\155", "POWERMINUS1"); (* -1 (^-1 sign) *)
  ("\156", "DEGREESIGN"); (* o Degree sign *)
  (* \157 *)
  ("\158", "MED"); (* Med (Catalog) *)
  ("\159", "SIGMAX"); (* (sigma)x (Catalog) *)
  ("\160", "RECPAR"); (* Rec( (Optn > Angl) *)
  ("\161", "SINH");   (* sinh (Optn > Hyp) *)
  ("\162", "COSH");   (* cosh (Optn > Hyp) *)
  ("\163", "TANH");   (* tanh (Optn > Hyp) *)
  (* \164 *)
  ("\165", "EPOWER"); (* e^ *)
  ("\166", "INT");    (* Int (Optn > Num) *)
  (* \167 *)
  ("\168", "POWER");  (* ^ *)
  ("\169", "TIMES");  (* Multiplication sign *)
  (* \170 *)
  ("\171", "EXCLAMATIONMARK"); (* ! (Optn > Prob) *)
  ("\172", "RADIANSIGN"); (* r Radian sign *)
  ("\173", "MINY"); (* minY (Catalog) *)
  ("\174", "MINX"); (* minX (Catalog) *)
  ("\175", "NSTAT"); (* n(STAT) (Catalog) *)
  (* \176 *)
  ("\177", "ARCSINH"); (* sinh^-1 (Optn > Hyp) *)
  ("\178", "ARCCOSH"); (* cosh^-1 (Optn > Hyp) *)
  ("\179", "ARCTANH"); (* tanh^-1 (Optn > Hyp) *)
  (* \180 *)
  ("\181", "TENPOWER"); (* 10 (10^ sign) *)
  ("\182", "FRAC");   (* Frac (Optn > Num) *)
  (* \183 *)
  ("\184", "NSQRT");  (* N-th square root sign *)
  ("\185", "DIVIDED"); (* Division sign *)
  (* \186 *)
  ("\187", "FRACSIGN"); (* Fraction sign *)
  ("\188", "GRADSIGN"); (* g Grad sign *)
  ("\189", "MAXY"); (* maxY (Catalog) *)
  ("\190", "MAXX"); (* maxX (Catalog) *)
  ("\191", "SIGMAYSQUARE"); (* (sigma)y2 (Catalog) *)
  ("\192", "ANS");    (* Ans *)
  ("\193", "RAN");    (* Ran# (Optn > Prob) *)
  ("\194", "XBAR"); (* x with overhead bar (Catalog) *)
  ("\196", "SMALLSIGMAX"); (* (small sigma)x (Catalog) *)
  ("\195", "YBAR"); (* y with overhead bar (Catalog) *)
  ("\197", "SX"); (* sx (Catalog) *)
  ("\198", "SMALLSIGMAY"); (* (small sigma)y (Catalog) *)
  ("\199", "SY"); (* sy (Catalog) *)
  ("\200", "AREG"); (* a(Reg) (Catalog) *)
  ("\201", "BREG"); (* b(Reg) (Catalog) *)
  ("\202", "RREG"); (* r(Reg) (Catalog) *)
  ("\203", "XHAT"); (* ^x (Catalog) *)
  ("\204", "YHAT"); (* ^y (Catalog) *)
  ("\205", "SMALLR"); (* r (complex radius) *)
  ("\206", "THETA");  (* Theta (complex angle) *)
  ("\207", "SIGMAY"); (* (sigma)y (Catalog) *)
  ("\208", "PI");     (* Pi sign *)
  ("\209", "CLS");    (* Cls (Sketch) *)
  (* \210 *)
  ("\211", "RND");    (* Rnd (Optn > Num) *)
  (* \212 ... \216 *)
  ("\217", "NORM");   (* Norm (Set up > Disp) *)
  ("\218", "DEG");    (* Deg (Set up > Angl) *)
  ("\219", "RAD");    (* Rad (Set up > Angl) *)
  ("\220", "GRA");    (* Gra (Set up > Angl) *)
  ("\221", "ENG");    (* Eng (Set up > Disp > Eng) *)
  ("\222", "INTG");   (* Intg (Optn > Num) *)
  ("\223", "SIGMAXY"); (* (sigma)xy (Catalog) *)
  ("\224", "PLOT");   (* Plot (Sketch > Plot) *)
  ("\225", "LINE");   (* Line (Sketch > Line) *)
  ("\226", "LBL");    (* Lbl *)
  ("\227", "FIX");    (* Fix (Set up > Disp) *)
  ("\228", "SCI");    (* Sci (Set up > Disp) *)
  (* \229 ... \231: first byte of two-byte symbols *)
  ("\232", "DSZ");    (* Dsz *)
  ("\233", "ISZ");    (* Isz *)
  ("\234", "FACTOR"); (* Factor (Zoom) *)
  ("\235", "VIEWWINDOW"); (* ViewWindow (V-Window) *)
  ("\236", "GOTO");   (* Goto *)
  ("\237", "PROG");   (* Prog *)
  ("\238", "GRAPHYEQ"); (* Graph Y= (Sketch > Grph) *)
  ("\239", "GRAPHS"); (* Graph [Integral sign] (Sketch > Grph) *)
  ("\240", "GRAPHYG"); (* Graph Y> (Sketch > Grph) *)
  ("\241", "GRAPHYL"); (* Graph Y< (Sketch > Grph) *)
  ("\242", "GRAPHYGEQ"); (* Graph Y>= (Sketch > Grph) *)
  ("\243", "GRAPHYLEQ"); (* Graph Y<= (Sketch > Grph) *)
  ("\244", "GRAPHREQ"); (* Graph r= (Sketch > Grph) *)
  ("\245", "GRAPHXYEQ"); (* Graph(X,Y)=( (Sketch > Grph) *)
  (* \246 \248 \250 *)
  (* \247, 249: first byte of two-byte symbols *)
  ("\251", "PPAR");   (* P( (Optn > Prob) *)
  ("\252", "QPAR");   (* Q( (Optn > Prob) *)
  ("\253", "RPAR");   (* R( (Optn > Prob) *)
  ("\254", "TPAR");   (* t( (Optn > Prob) *)
  (* \255 *)
  
  (* Two-byte symbols *)
  ("\127\000", "XMIN"); (* Xmin (Catalog) *)
  ("\127\001", "XMAX"); (* Xmax (Catalog) *)
  ("\127\002", "XSCL"); (* Xscl (Catalog) *)
  ("\127\004", "YMIN"); (* Ymin (Catalog) *)
  ("\127\005", "YMAX"); (* Ymax (Catalog) *)
  ("\127\006", "YSCL"); (* Yscl (Catalog) *)
  ("\127\008", "TTHETAMIN"); (* T(theta)min (Catalog) *)
  ("\127\009", "TTHETAMAX"); (* T(theta)max (Catalog) *)
  ("\127\010", "TTHETAPTCH"); (* T(theta)ptch (Catalog) *)
  ("\127\011", "XFCT"); (* Xfct (Catalog) *)
  ("\127\012", "YFCT"); (* Yfct (Catalog) *)
  ("\127\013", "DSTART"); (* D Start (Catalog) *)
  ("\127\014", "DEND"); (* D End (Catalog) *)
  ("\127\015", "DPITCH"); (* D pitch (Catalog) *)
  ("\127\016", "RIGHTXMIN"); (* RightXmin (Catalog) *)
  ("\127\017", "RIGHTXMAX");  (* RightXmax (Catalog) *)
  ("\127\018", "RIGHTXSCL");  (* RightXscl (Catalog) *)
  ("\127\020", "RIGHTYMIN"); (* RightYmin (Catalog) *)
  ("\127\021", "RIGHTYMAX");  (* RightYmax (Catalog) *)
  ("\127\022", "RIGHTYSCL");  (* RightYscl (Catalog) *)
  ("\127\024", "RIGHTTTHETAMIN"); (* RightT(theta)min (Catalog) *)
  ("\127\025", "RIGHTTTHETAMAX"); (* RightT(theta)max (Catalog) *)
  ("\127\026", "RIGHTTTHETAPTCH"); (* RightT(theta)ptch (Catalog) *)
  ("\127\029", "CREG"); (* c(Reg) (Catalog) *)
  ("\127\030", "DREG"); (* d(Reg) (Catalog) *)
  ("\127\031", "EREG"); (* e(Reg) (Catalog) *)
  ("\127\032", "MAX"); (* Max (Optn > List) *)
  ("\127\033", "DET"); (* Det (Optn > Mat) *)
  ("\127\034", "ARG"); (* Arg (Optn > Cplx) *)
  ("\127\035", "CONJG"); (* Conjg (Optn > Cplx) *)
  ("\127\036", "REP"); (* ReP real part (Optn > Cplx) *)
  ("\127\037", "IMP"); (* ImP imaginary part (Optn > Cplx) *)
  ("\127\038", "DOVERDX"); (* d/dx( (Optn > Calc) *)
  ("\127\039", "DSQOVERDXSQ"); (* d2/dx2( (Optn > Calc) *)
  ("\127\040", "SOLVE"); (* Solve( (Optn > Calc) *)
  ("\127\041", "SIGMAPAR"); (* Sigma( (Optn > Calc) *)
  ("\127\042", "FMIN"); (* FMin( (Optn > Calc) *)
  ("\127\043", "FMAX"); (* FMax( (Optn > Calc) *)
  ("\127\044", "SEQ"); (* Seq( (Optn > List) *)
  ("\127\045", "MIN"); (* Min( (Optn > List) *)
  ("\127\046", "MEAN"); (* Mean( (Optn > List) *)
  ("\127\047", "MEDIAN"); (* Median( (Optn > List) *)
  ("\127\048", "SOLVEN"); (* SolveN( (Optn > Calc) *)
  ("\127\058", "MOD"); (* MOD( (Optn > Num) *)
  ("\127\059", "MODEXP"); (* MOD_Exp( (Optn > Num) *)
  ("\127\060", "GCD"); (* GCD( (Optn > Num) *)
  ("\127\061", "LCM"); (* LCM( (Optn > Num) *)
  ("\127\062", "STDDEV"); (* StdDev( (Catalog) *)
  ("\127\063", "VARIANCE"); (* Variance( (Catalog) *)
  ("\127\064", "MAT"); (* Mat *)
  ("\127\065", "TRN"); (* Trn (Optn > Mat) *)
  ("\127\066", "STARROW"); (* *Row *)
  ("\127\067", "STARROWPLUS"); (* *Row+ *)
  ("\127\068", "ROWPLUS"); (* Row+ *)
  ("\127\069", "SWAP"); (* Swap *)
  ("\127\070", "DIM"); (* Dim (Optn > List) *)
  ("\127\071", "FILL"); (* Fill( (Optn > List) *)
  ("\127\072", "IDENTITY"); (* Identity (Optn > Mat) *)
  ("\127\073", "AUGMENT"); (* Augment( (Optn > List) *)
  ("\127\074", "LISTTOMAT"); (* List->Mat (Optn > List) *)
  ("\127\075", "MATTOLIST"); (* Mat -> List (Optn > Mat) *)
  ("\127\076", "SUM"); (* Sum (Optn > List) *)
  ("\127\077", "PROD"); (* Prod (Optn > List) *)
  ("\127\078", "PERCENT"); (* Percent (Optn > List) *)
  ("\127\079", "CUML"); (* Cuml (Optn > List) *)
  ("\127\080", "CPLXI"); (* i (complex number) *)
  ("\127\081", "LIST"); (* List *)
  ("\127\082", "DELTALIST"); (* Delta List (Optn > List) *)
  ("\127\084", "CHEVRON"); (* Chevron sign (Shift-A) *)
  ("\127\085", "REF"); (* Ref (Optn > Mat) *)
  ("\127\086", "RREF"); (* Rref (Optn > Mat) *)
  ("\127\087", "BLACKARROW"); (* Black arrow (Optn > Conv) *)
  ("\127\096", "SIMCOEF"); (* Sim Coef (Catalog) *)
  ("\127\097", "PLYCOEF"); (* Ply Coef (Catalog) *)
  ("\127\098", "SIMRESULT"); (* Sim Result (Catalog) *)
  ("\127\099", "PLYRESULT"); (* Ply Result (Catalog) *)
  ("\127\100", "NTVM"); (* n(TVM) (Catalog) *)
  ("\127\101", "IPERCENT"); (* I(percent) (Optn > Calc) *)
  ("\127\102", "PV"); (* PV (Catalog) *)
  ("\127\103", "PMT"); (* PMT (Catalog) *)
  ("\127\104", "FV"); (* FV (Catalog) *)
  ("\127\106", "LISTONE");   (* List1 (Catalog) *)
  ("\127\107", "LISTTWO");   (* List2 (Catalog) *)
  ("\127\108", "LISTTHREE"); (* List3 (Catalog) *)
  ("\127\109", "LISTFOUR");  (* List4 (Catalog) *)
  ("\127\110", "LISTFIVE");  (* List5 (Catalog) *)
  ("\127\111", "LISTSIX");   (* List6 (Catalog) *)
  ("\127\118", "QONE"); (* Q1 (Catalog) *)
  ("\127\119", "QTHREE"); (* Q3 (Catalog) *)
  ("\127\120", "XONE"); (* x1 (Catalog) *)
  ("\127\121", "YONE"); (* y1 (Catalog) *)
  ("\127\122", "XTWO"); (* x2 (Catalog) *)
  ("\127\123", "YTWO"); (* y2 (Catalog) *)
  ("\127\124", "XTHREE"); (* x3 (Catalog) *)
  ("\127\125", "YTHREE"); (* y3 (Catalog) *)
  ("\127\133", "LOGAB"); (* logab( (Optn > Calc) *)
  ("\127\134", "RNDFIX"); (* RndFix( (Optn > Num) *)
  ("\127\135", "RANINT"); (* RanInt#( (Optn > Prob) *)
  ("\127\136", "RANLIST"); (* RanList#( (Optn > Prob) *)
  ("\127\137", "RANBIN"); (* RanBin#( (Optn > Prob) *)
  ("\127\138", "RANNORM"); (* RanNorm#( (Optn > Prob) *)
  ("\127\140", "SIGMAAN"); (* (Sigma)an (Catalog) *)
  ("\127\141", "SIGMABN"); (* (Sigma)bn (Catalog) *)
  ("\127\142", "SIGMACN"); (* (Sigma)cn (Catalog) *)
  ("\127\143", "GETKEY"); (* Getkey *)
  ("\127\144", "FRESULT"); (* F Result (Catalog) *)
  ("\127\145", "FSTART"); (* F Start (Catalog) *)
  ("\127\146", "FEND"); (* F End (Catalog) *)
  ("\127\147", "FPITCH"); (* F pitch (Catalog) *)
  ("\127\148", "RRESULT"); (* R Result (Catalog) *)
  ("\127\149", "RSTART"); (* R Start (Catalog) *)
  ("\127\150", "REND"); (* R End (Catalog) *)
  ("\127\151", "HSTART"); (* H Start (Catalog) *)
  ("\127\152", "HPITCH"); (* H pitch (Catalog) *)
  ("\127\156", "TOSIMP"); (* >Simp (Optn > Calc) *)
  ("\127\160", "AN"); (* an (Vars > Recr > Form) *)
  ("\127\161", "ANPLUSONE"); (* an+1 (Vars > Recr > Form) *)
  ("\127\162", "ANPLUSTWO"); (* an+2 (Vars > Recr > Form) *)
  ("\127\163", "NRECUR"); (* n(RECUR) (Catalog) *)
  ("\127\164", "AZERO"); (* a0 (Vars > Recr > Rang) *)
  ("\127\165", "AONE"); (* a1 (Vars > Recr > Rang) *)
  ("\127\166", "ATWO"); (* a2 (Vars > Recr > Rang) *)
  ("\127\167", "BN"); (* bn (Vars > Recr > Form) *)
  ("\127\168", "BNPLUSONE"); (* bn+1 (Vars > Recr > Form) *)
  ("\127\169", "BNPLUSTWO"); (* bn+2 (Vars > Recr > Form) *)
  ("\127\170", "BZERO"); (* b0 (Vars > Recr > Rang) *)
  ("\127\171", "BONE"); (* b1 (Vars > Recr > Rang) *)
  ("\127\172", "BTWO"); (* b2 (Vars > Recr > Rang) *)
  ("\127\173", "ANSTART"); (* anStart (Vars > Recr > Rang) *)
  ("\127\174", "BNSTART"); (* bnStart (Vars > Recr > Rang) *)
  ("\127\176", "AND"); (* And (Optn > Logic) *)
  ("\127\177", "OR"); (* Or (Optn > Logic) *)
  ("\127\179", "NOT"); (* Not (Optn > Logic) *)
  ("\127\180", "XOR"); (* Xor (Optn > Logic) *)
  ("\127\181", "SIGMAANPLUSONE"); (* (Sigma)an+1 (Catalog) *)
  ("\127\182", "SIGMABNPLUSONE"); (* (Sigma)bn+1 (Catalog) *)
  ("\127\183", "SIGMACNPLUSONE"); (* (Sigma)cn+1 (Catalog) *)
  ("\127\185", "SIGMAANPLUSTWO"); (* (Sigma)an+2 (Catalog) *)
  ("\127\186", "SIGMABNPLUSTWO"); (* (Sigma)bn+2 (Catalog) *)
  ("\127\187", "SIGMACNPLUSTWO"); (* (Sigma)cn+2 (Catalog) *)
  ("\127\188", "INTDIV"); (* Int division (Optn > Calc) *)
  ("\127\189", "RMDR"); (* Rmdr (Optn > Calc) *)
  ("\127\190", "FA"); (* Fa (Catalog) *)
  ("\127\192", "SMALLNONE"); (* n1 (Catalog) *)
  ("\127\193", "SMALLNTWO"); (* n2 (Catalog) *)
  ("\127\194", "XBARONE"); (* x(overhead bar)1 (Catalog) *)
  ("\127\195", "XBARTWO"); (* x(overhead bar)2 (Catalog) *)
  ("\127\196", "SXONE"); (* sx1 (Catalog) *)
  ("\127\197", "SXTWO"); (* sx2 (Catalog) *)
  ("\127\198", "SP"); (* sp (Catalog) *)
  ("\127\199", "PHAT"); (* ^p (Catalog) *)
  ("\127\200", "PHATONE"); (* ^p1 (Catalog) *)
  ("\127\201", "PHATTWO"); (* ^p2 (Catalog) *)
  ("\127\202", "LEFT"); (* Left (Catalog) *)
  ("\127\203", "RIGHT"); (* Right (Catalog) *)
  ("\127\204", "POVERY"); (* P/Y (Catalog) *)
  ("\127\205", "COVERY"); (* C/Y (Catalog) *)
  ("\127\206", "FB"); (* Fb (Catalog) *)
  ("\127\208", "LETTERF"); (* F (Catalog) *)
  ("\127\209", "SMALLZ"); (* z (Catalog) *)
  ("\127\210", "SMALLP"); (* Small p (Catalog) *)
  ("\127\211", "SMALLT"); (* Small t (Catalog) *)
  ("\127\212", "SE"); (* se (Catalog) *)
  ("\127\213", "XSQUARE"); (* x^2 (Catalog) *)
  ("\127\214", "RSQUARE"); (* r^2 (Catalog) *)
  ("\127\215", "ADF"); (* Adf (Catalog) *)
  ("\127\216", "EDF"); (* Edf (Catalog) *)
  ("\127\217", "DF"); (* df (Catalog) *)
  ("\127\218", "SSA"); (* SSa (Catalog) *)
  ("\127\219", "MSA"); (* MSa (Catalog) *)
  ("\127\220", "SSE"); (* SSe (Catalog) *)
  ("\127\221", "MSE"); (* MSe (Catalog) *)
  ("\127\222", "FAB"); (* Fab (Catalog) *)
  ("\127\224", "BDF"); (* Bdf (Catalog) *)
  ("\127\225", "ABDF"); (* ABdf (Catalog) *)
  ("\127\226", "PA"); (* pa (Catalog) *)
  ("\127\227", "PB"); (* pb (Catalog) *)
  ("\127\228", "PAB"); (* pab (Catalog) *)
  ("\127\233", "CELLSUM"); (* CellSum( (Catalog) *)
  ("\127\234", "CELLPROD"); (* CellProd( (Catalog) *)
  ("\127\235", "CELLMIN"); (* CellMin( (Catalog) *)
  ("\127\236", "CELLMAX"); (* CellMax( (Catalog) *)
  ("\127\237", "CELLMEAN"); (* CellMean( (Catalog) *)
  ("\127\238", "CELLMEDIAN"); (* CellMedian( (Catalog) *)
  ("\127\239", "CELLIF"); (* CellIf( (Catalog) *)
  ("\127\240", "YGRAPH"); (* Y(GRAPH) (Catalog) *)
  ("\127\241", "RGRAPH"); (* r(GRAPH) (Catalog) *)
  ("\127\242", "XT"); (* Xt (Catalog) *)
  ("\127\243", "YT"); (* Yt (Catalog) *)
  ("\127\244", "XGRAPH"); (* X(GRAPH) (Catalog) *)
  ("\127\251", "SSB"); (* SSb (Catalog) *)
  ("\127\252", "SSAB"); (* SSab (Catalog) *)
  ("\127\253", "MSB"); (* MSb (Catalog) *)
  ("\127\254", "MSAB"); (* MSab (Catalog) *)
  ("\231\001", "UNITNS"); (* [ns] (Catalog) *)
  ("\231\002", "UNITMICROS"); (* [(micro)s] (Catalog) *)
  ("\231\003", "UNITMS"); (* [ms] (Catalog) *)
  ("\231\004", "UNITS"); (* [s] (Catalog) *)
  ("\231\005", "UNITMIN"); (* [min] (Catalog) *)
  ("\231\006", "UNITH"); (* [h] (Catalog) *)
  ("\231\007", "UNITDAY"); (* [day] (Catalog) *)
  ("\231\008", "UNITWEEK"); (* [week] (Catalog) *)
  ("\231\009", "UNITYEAR"); (* [yr] (Catalog) *)
  ("\231\010", "UNITSYR"); (* [s-yr] (Catalog) *)
  ("\231\011", "UNITTYR"); (* [t-yr] (Catalog) *)
  ("\231\012", "UNITCELSIUS"); (* [°C] (Catalog) *)
  ("\231\013", "UNITK"); (* [K] (Catalog) *)
  ("\231\014", "UNITFAHRENHEIT"); (* [°F] (Catalog) *)
  ("\231\015", "UNITRANKINE"); (* [°R] (Catalog) *)
  ("\231\016", "UNITU"); (* [u] (Catalog) *)
  ("\231\017", "UNITG"); (* [g] (Catalog) *)
  ("\231\018", "UNITKG"); (* [kg] (Catalog) *)
  ("\231\019", "UNITLB"); (* [lb] (Catalog) *)
  ("\231\020", "UNITOZ"); (* [oz] (Catalog) *)
  ("\231\021", "UNITSLUG"); (* [slug] (Catalog) *)
  ("\231\022", "UNITTONSHORT"); (* [ton(short)] (Catalog) *)
  ("\231\023", "UNITTONLONG"); (* [ton(long)] (Catalog) *)
  ("\231\025", "UNITMTON"); (* [mton] (Catalog) *)
  ("\231\026", "UNITLATM"); (* [l-atm] (Catalog) *)
  ("\231\027", "UNITFTLBF"); (* [ft.lbf] (Catalog) *)
  ("\231\028", "UNITCALIT"); (* [calIT] (Catalog) *)
  ("\231\029", "UNITCALTH"); (* [calth] (Catalog) *)
  ("\231\030", "UNITBTU"); (* [Btu] (Catalog) *)
  ("\231\031", "UNITKWH"); (* [kW.h] (Catalog) *)
  ("\231\032", "UNITKGFM"); (* [kgf.m] (Catalog) *)
  ("\231\033", "UNITPA"); (* [Pa] (Catalog) *)
  ("\231\034", "UNITKPA"); (* [kPa] (Catalog) *)
  ("\231\035", "UNITBAR"); (* [bar] (Catalog) *)
  ("\231\036", "UNITMMHTWOO"); (* [mmH2O] (Catalog) *)
  ("\231\037", "UNITMMHG"); (* [mmHg] (Catalog) *)
  ("\231\038", "UNITINHTWOO"); (* [inH2O] (Catalog) *)
  ("\231\039", "UNITINHG"); (* [inHg] (Catalog) *)
  ("\231\040", "UNITLBFPINSQUARE"); (* [lbf/in2] (Catalog) *)
  ("\231\041", "UNITKGFPCMSQUARE"); (* [kgf/cm2] (Catalog) *)
  ("\231\042", "UNITATM"); (* [atm] (Catalog) *)
  ("\231\043", "UNITDYNE"); (* [dyne] (Catalog) *)
  ("\231\044", "UNITN"); (* [N] (Catalog) *)
  ("\231\045", "UNITKGF"); (* [kgf] (Catalog) *)
  ("\231\046", "UNITLBF"); (* [lbf] (Catalog) *)
  ("\231\047", "UNITTONF"); (* [tonf] (Catalog) *)
  ("\231\176", "UNITFM"); (* [fm] (Catalog) *)
  ("\231\178", "UNITMM"); (* [mm] (Catalog) *)
  ("\231\179", "UNITCM"); (* [cm] (Catalog) *)
  ("\231\180", "UNITM"); (* [m] (Catalog) *)
  ("\231\181", "UNITKM"); (* [km] (Catalog) *)
  ("\231\182", "UNITMIL"); (* [Mil] (Catalog) *)
  ("\231\183", "UNITIN"); (* [in] (Catalog) *)
  ("\231\184", "UNITFT"); (* [ft] (Catalog) *)
  ("\231\185", "UNITYD"); (* [yd] (Catalog) *)
  ("\231\186", "UNITFATH"); (* [fath] (Catalog) *)
  ("\231\187", "UNITRD"); (* [rd] (Catalog) *)
  ("\231\189", "UNITMILE"); (* [mile] (Catalog) *)
  ("\231\191", "UNITNMILE"); (* [n mile] (Catalog) *)
  ("\231\192", "UNITACRE"); (* [acre] (Catalog) *)
  ("\231\194", "UNITHA"); (* [ha] (Catalog) *)
  ("\231\195", "UNITCMSQUARE"); (* [cm2] (Catalog) *)
  ("\231\196", "UNITMSQUARE"); (* [m2] (Catalog) *)
  ("\231\197", "UNITKMSQUARE"); (* [km2] (Catalog) *)
  ("\231\199", "UNITINSQUARE"); (* [in2] (Catalog) *)
  ("\231\200", "UNITFTSQUARE"); (* [ft2] (Catalog) *)
  ("\231\201", "UNITYDSQUARE"); (* [yd2] (Catalog) *)
  ("\231\202", "UNITMILESQUARE"); (* [mile2] (Catalog) *)
  ("\231\203", "UNITMPS"); (* [m/s] (Catalog) *)
  ("\231\204", "UNITKMPH"); (* [km/h] (Catalog) *)
  ("\231\205", "UNITFTPS"); (* [ft/s] (Catalog) *)
  ("\231\206", "UNITMPH"); (* [mile/h] (Catalog) *)
  ("\231\207", "UNITKNOT"); (* [knot] (Catalog) *)
  ("\231\208", "UNITML"); (* [mL] (Catalog) *)
  ("\231\209", "UNITL"); (* [L] (Catalog) *)
  ("\231\210", "UNITTSP"); (* [tsp] (Catalog) *)
  ("\231\211", "UNITCMCUBE"); (* [cm3] (Catalog) *)
  ("\231\212", "UNITMCUBE"); (* [m3] (Catalog) *)
  ("\231\213", "UNITTBSP"); (* [tbsp] (Catalog) *)
  ("\231\215", "UNITINCUBE"); (* [in3] (Catalog) *)
  ("\231\216", "UNITFTCUBE"); (* [ft3] (Catalog) *)
  ("\231\217", "UNITFLOZUK"); (* [fl_oz(UK)] (Catalog) *)
  ("\231\218", "UNITFLOZUS"); (* [fl_oz(US)] (Catalog) *)
  ("\231\219", "UNITCUP"); (* [cup] (Catalog) *)
  ("\231\220", "UNITPT"); (* [pt] (Catalog) *)
  ("\231\221", "UNITQT"); (* [qt] (Catalog) *)
  ("\231\222", "UNITGALUS"); (* [gal(US)] (Catalog) *)
  ("\231\223", "UNITGALUK"); (* [gal(UK)] (Catalog) *)
  ("\231\226", "UNITMICROM"); (* [(micro)m] (Catalog) *)
  ("\231\227", "UNITMG"); (* [mg] (Catalog) *)
  ("\231\228", "UNITANGSTROM"); (* [ao] (Catalog) *)
  ("\231\232", "UNITAU"); (* [AU] (Catalog) *)
  ("\231\233", "UNITLY"); (* [l.y.] (Catalog) *)
  ("\231\234", "UNITPC"); (* [pc] (Catalog) *)
  ("\231\235", "UNITFTLBFPS"); (* [ft.lbf/s] (Catalog) *)
  ("\231\236", "UNITCALTHPS"); (* [calth/s] (Catalog) *)
  ("\231\237", "UNITHP"); (* [hp] (Catalog) *)
  ("\231\238", "UNITBTUPM"); (* [Btu/min] (Catalog) *)
  ("\231\239", "UNITW"); (* [W] (Catalog) *)
  ("\231\240", "UNITEV"); (* [eV] (Catalog) *)
  ("\231\241", "UNITERG"); (* [erg] (Catalog) *)
  ("\231\242", "UNITJ"); (* [J] (Catalog) *)
  ("\231\243", "UNITCALFIFTEEN"); (* [cal15] (Catalog) *)
  ("\231\244", "UNITKCALFIFTEEN"); (* [kcal15] (Catalog) *)
  ("\231\245", "UNITKCALTH"); (* [kcalTH] (Catalog) *)
  ("\231\251", "UNITKCALIT"); (* [kcalIT] (Catalog) *)
  ("\247\000", "IF"); (* If *)
  ("\247\001", "THEN"); (* Then *)
  ("\247\002", "ELSE"); (* Else *)
  ("\247\003", "IFEND"); (* IfEnd *)
  ("\247\004", "FOR"); (* For *)
  ("\247\005", "TO"); (* To *)
  ("\247\006", "STEP"); (* Step *)
  ("\247\007", "NEXT"); (* Next *)
  ("\247\008", "WHILE"); (* While *)
  ("\247\009", "WHILEEND"); (* WhileEnd *)
  ("\247\010", "DO"); (* Do *)
  ("\247\011", "LPWHILE"); (* LpWhile *)
  ("\247\012", "RETURN"); (* Return *)
  ("\247\013", "BREAK"); (* Break *)
  ("\247\014", "STOP"); (* Stop *)
  ("\247\016", "LOCATE"); (* Locate *)
  ("\247\017", "SEND"); (* Send( (Catalog) *)
  ("\247\018", "RECEIVE"); (* Receive( (Catalog) *)
  ("\247\019", "OPENCOMPORTTHIRTYEIGHTK"); (* OpenComport38k (Catalog) *)
  ("\247\020", "CLOSECOMPORTTHIRTYEIGHTK"); (* CloseComport38k (Catalog) *)
  ("\247\021", "SENDTHIRTYEIGHTK"); (* Send38k (Catalog) *)
  ("\247\022", "RECEIVETHIRTYEIGHTK"); (* Receive38k (Catalog) *)
  ("\247\024", "CLRTEXT"); (* ClrText *)
  ("\247\025", "CLRGRAPH"); (* ClrGraph *)
  ("\247\026", "CLRLIST"); (* ClrList *)
  ("\247\027", "LINEARREGAPLUSBX"); (* LinearRef(a+bx) (Catalog) *)
  ("\247\028", "SLNORMAL"); (* S-L-Normal (Set up > S/L) *)
  ("\247\029", "SLTHICK"); (* S-L-Thick (Set up > S/L) *)
  ("\247\030", "SLBROKEN"); (* S-L-Broken (Set up > S/L) *)
  ("\247\031", "SLDOT"); (* S-L-Dot (Set up > S/L) *)
  ("\247\032", "DRAWGRAPH"); (* DrawGraph *)
  ("\247\033", "PLOTPHASE"); (* PlotPhase (Catalog) *)
  ("\247\034", "DRAWDYNA"); (* DrawDyna *)
  ("\247\035", "DRAWSTAT"); (* DrawStat *)
  ("\247\036", "DRAWFTGCON"); (* DrawFTG-Con (Catalog) *)
  ("\247\037", "DRAWFTGPLT"); (* DrawFTG-Plt (Catalog) *)
  ("\247\038", "DRAWRCON"); (* DrawR-Con (Catalog) *)
  ("\247\039", "DRAWRPLT"); (* DrawR-Plt (Catalog) *)
  ("\247\040", "DRAWRSIGMACON"); (* DrawR(Sigma)-Con (Catalog) *)
  ("\247\041", "DRAWRSIGMAPLT"); (* DrawR(Sigma)-Plt (Catalog) *)
  ("\247\042", "DRAWWEB"); (* DrawWeb (Catalog) *)
  ("\247\043", "NORMALG"); (* NormalG (Menu > Graph > Styl) *)
  ("\247\044", "THICKG"); (* ThickG (Menu > Graph > Styl) *)
  ("\247\045", "BROKENTHICKG"); (* BrokenThickG (Menu > Graph > Styl) *)
  ("\247\046", "DISPFTBL"); (* DispF-Tbl (Catalog) *)
  ("\247\047", "DISPRTBL"); (* DispR-Tbl (Catalog) *)
  ("\247\048", "SIMPLIFYAUTO"); (* SimplifyAuto (Catalog) *)
  ("\247\049", "SIMPLIFYMAN"); (* SimplifyMan (Catalog) *)
  ("\247\050", "NPPLOT"); (* NPPlot (Catalog) *)
  ("\247\051", "SINUSOIDAL"); (* Sinusoidal (Catalog) *)
  ("\247\052", "SINREG"); (* SinReg (Catalog) *)
  ("\247\053", "LOGISTIC"); (* Logistic (Catalog) *)
  ("\247\054", "LOGISTICREG"); (* LogisticReg (Catalog) *)
  ("\247\058", "PIE"); (* Pie (Catalog) *)
  ("\247\060", "BAR"); (* Bar (DrawStat setup) *)
  ("\247\063", "DOTG"); (* DotG (Menu > Graph > Styl) *)
  ("\247\064", "ONEVARIABLE"); (* 1-Variable (Catalog) *)
  ("\247\065", "TWOVARIABLE"); (* 2-Variable (Catalog) *)
  ("\247\066", "LINEARREGAXPLUSB"); (* LinearRef(ax+b) (Catalog) *)
  ("\247\067", "MEDMEDLINE"); (* Med-MedLine (Catalog) *)
  ("\247\068", "QUADREG"); (* QuadReg (Catalog) *)
  ("\247\069", "CUBICREG"); (* CubicReg (Catalog) *)
  ("\247\070", "QUARTREG"); (* QuartReg (Catalog) *)
  ("\247\071", "LOGREG"); (* LogReg (Catalog) *)
  ("\247\072", "EXPREGAEPOWBX"); (* ExpReg(a.e^bx) (Catalog) *)
  ("\247\073", "POWERREG"); (* PowerReg (Catalog) *)
  ("\247\074", "SGPH1"); (* S-Gph1 (DrawStat setup) *)
  ("\247\075", "SGPH2"); (* S-Gph2 (DrawStat setup) *)
  ("\247\076", "SGPH3"); (* S-Gph3 (DrawStat setup) *)
  ("\247\077", "SQUARE"); (* Square (DrawStat setup) *)
  ("\247\078", "CROSS"); (* Cross (DrawStat setup) *)
  ("\247\079", "DOT"); (* Dot (DrawStat setup) *)
  ("\247\080", "SCATTER"); (* Scatter (DrawStat setup) *)
  ("\247\081", "XYLINE"); (* xyLine (DrawStat setup) *)
  ("\247\082", "HIST"); (* Hist (DrawStat setup) *)
  ("\247\083", "MEDBOX"); (* MedBox (DrawStat setup) *)
  ("\247\085", "NDIST"); (* N-Dist (DrawStat setup) *)
  ("\247\086", "BROKEN"); (* Broken (DrawStat setup) *)
  ("\247\087", "LINEAR"); (* Linear (Catalog) *)
  ("\247\088", "MEDMED"); (* Med-Med (Catalog) *)
  ("\247\089", "QUAD"); (* Quad (Catalog) *)
  ("\247\090", "CUBIC"); (* Cubic (Catalog) *)
  ("\247\091", "QUART"); (* Quart (Catalog) *)
  ("\247\092", "LLOG"); (* Log (Catalog) (NOT the log function) *)
  ("\247\093", "EXPAEPOWBX"); (* Exp(a.e^bx) (Catalog) *)
  ("\247\094", "POWERKEYWORD"); (* Power (keyword, NOT the power operator) (Catalog) *)
  ("\247\095", "EXPREGABPOWX"); (* ExpReg(a.b^x) (Catalog) *)
  ("\247\096", "SWINDAUTO"); (* S-WindAuto (Set up > S-Win) *)
  ("\247\097", "SWINDMAN"); (* S-WindMan (Set up > S-Win) *)
  ("\247\098", "GRAPHXEQ"); (* Graph X= (Sketch > Grph) *)
  ("\247\099", "YEQUALTYPE"); (* Y=Type (Menu > Graph > Type) *)
  ("\247\100", "REQUALTYPE"); (* r=Type (Menu > Graph > Type) *)
  ("\247\101", "PARAMTYPE"); (* ParamType (Menu > Graph > Type) *)
  ("\247\103", "XEQUALTYPE"); (* X=Type (Menu > Graph > Type) *)
  ("\247\104", "XGTYPE"); (* X>Type (Menu > Graph > Type) *)
  ("\247\105", "XLTYPE"); (* X<Type (Menu > Graph > Type) *)
  ("\247\106", "YGTYPE"); (* Y>Type (Menu > Graph > Type) *)
  ("\247\107", "YLTYPE"); (* Y<Type (Menu > Graph > Type) *)
  ("\247\108", "YGEQTYPE"); (* Y>=Type (Menu > Graph > Type) *)
  ("\247\109", "YLEQTYPE"); (* Y<=Type (Menu > Graph > Type) *)
  ("\247\110", "XGEQTYPE"); (* X>=Type (Menu > Graph > Type) *)
  ("\247\111", "XLEQTYPE"); (* X<=Type (Menu > Graph > Type) *)
  ("\247\112", "GCONNECT"); (* G-Connect (Set up > Draw) *)
  ("\247\113", "GPLOT"); (* G-Plot (Set up > Draw) *)
  ("\247\118", "RESIDNONE"); (* Resid-None (Catalog) *)
  ("\247\119", "RESIDLIST"); (* Resid-List (Catalog) *)
  ("\247\120", "BGNONE"); (* BG-None (Set up > Back) *)
  ("\247\121", "BGPICT"); (* BG-Pict (Set up > Back) *)
  ("\247\122", "GRIDOFF"); (* GridOff (Set up > Grid) *)
  ("\247\125", "GRIDON"); (* GridOn (Set up > Grid) *)
  ("\247\126", "EXPABPOWX"); (* Exp(a.b^x) (Catalog) *)
  ("\247\128", "DVAR"); (* D Var (Menu > Dyna) *)
  ("\247\135", "QONEQTHREETYPESTD"); (* Q1Q3TypeStd (Catalog) *)
  ("\247\136", "VARRANGE"); (* VarRange (Catalog) *)
  ("\247\137", "QONEQTHREETYPEONDATA"); (* Q1Q3TypeOnData (Catalog) *)
  ("\247\140", "SKETCHNORMAL"); (* SketchNormal (Sketch > Styl) *)
  ("\247\141", "SKETCHTHICK"); (* SketchThick (Sketch > Styl) *)
  ("\247\142", "SKETCHBROKEN"); (* SketchBroken (Sketch > Styl) *)
  ("\247\143", "SKETCHDOT"); (* SketchDot (Sketch > Styl) *)
  ("\247\144", "ANTYPE"); (* anType (Catalog) *)
  ("\247\145", "ANPLUSONETYPE"); (* an+1Type (Catalog) *)
  ("\247\146", "ANPLUSTWOTYPE"); (* an+2Type (Catalog) *)
  ("\247\147", "STOPICT"); (* StoPict *)
  ("\247\148", "RCLPICT"); (* RclPict *)
  ("\247\149", "STOGMEM"); (* StoGMEM (Menu > Graph > Gmem) *)
  ("\247\150", "RCLGMEM"); (* RclGMEM (Menu > Graph > Gmem) *)
  ("\247\151", "STOVWIN"); (* StoV-Win (V-Win) *)
  ("\247\152", "RCLVWIN"); (* RclV-Win (V-Win) *)
  ("\247\153", "PERCENTSIGN"); (* % (percent sign) *)
  ("\247\154", "DATA"); (* Data *)
  ("\247\158", "MENU"); (* Menu *)
  ("\247\159", "RCLCAPT"); (* RclCapt (Optn > Capt) *)
  ("\247\160", "TANGENT"); (* Tangent (Sketch) *)
  ("\247\161", "NORMAL"); (* Normal (Sketch) *)
  ("\247\162", "INVERSE"); (* Inverse (Sketch) *)
  ("\247\163", "VERTICAL"); (* Vertical (Sketch) *)
  ("\247\164", "HORIZONTAL"); (* Horizontal (Sketch) *)
  ("\247\165", "TEXT"); (* Text (Sketch) *)
  ("\247\166", "CIRCLE"); (* Circle (Sketch) *)
  ("\247\167", "FLINE"); (* F-Line (Sketch > Line) *)
  ("\247\168", "PLOTON"); (* PlotOn (Sketch > Plot) *)
  ("\247\169", "PLOTOFF"); (* PlotOff (Sketch > Plot) *)
  ("\247\170", "PLOTCHG"); (* PlotChg (Sketch > Plot) *)
  ("\247\171", "PXLON"); (* PxlOn (Sketch > Pixl) *)
  ("\247\172", "PXLOFF"); (* PxlOff (Sketch > Pixl) *)
  ("\247\173", "PXLCHG"); (* PxlChg (Sketch > Pixl) *)
  ("\247\175", "PXLTEST"); (* PxlTest( (Sketch > Plot) *)
  ("\247\176", "SORTA"); (* SortA( *)
  ("\247\177", "SORTD"); (* SortD( *)
  ("\247\178", "VARLISTONE");   (* VarList1 (Catalog) *)
  ("\247\179", "VARLISTTWO");   (* VarList2 (Catalog) *)
  ("\247\180", "VARLISTTHREE"); (* VarList3 (Catalog) *)
  ("\247\181", "VARLISTFOUR");  (* VarList4 (Catalog) *)
  ("\247\182", "VARLISTFIVE");  (* VarList5 (Catalog) *)
  ("\247\183", "VARLISTSIX");   (* VarList6 (Catalog) *)
  ("\247\184", "FILEONE");   (* File1 (Catalog) *)
  ("\247\185", "FILETWO");   (* File2 (Catalog) *)
  ("\247\186", "FILETHREE"); (* File3 (Catalog) *)
  ("\247\187", "FILEFOUR");  (* File4 (Catalog) *)
  ("\247\188", "FILEFIVE");  (* File5 (Catalog) *)
  ("\247\189", "FILESIX");   (* File6 (Catalog) *)
  ("\247\190", "YDRAWSPEEDNORM"); (* Y=DrawSpeedNorm (Set up > Y-Spd) *)
  ("\247\191", "YDRAWSPEEDHIGH"); (* Y=DrawSpeedHigh (Set up > Y-Spd) *)
  ("\247\192", "FUNCON"); (* FuncOn (Set up > Func) *)
  ("\247\193", "SIMULON"); (* SimulOn (Set up > Siml) *)
  ("\247\194", "AXESON"); (* AxesOn (Set up > Axes) *)
  ("\247\195", "COORDON"); (* CoordOn (Set up > Coor) *)
  ("\247\196", "LABELON"); (* LabelOn (Set up > Labl) *)
  ("\247\197", "DERIVON"); (* DerivOn (Set up > Derv) *)
  ("\247\198", "LOCUSON"); (* LocusOn (Set up > Locs) *)
  ("\247\199", "SIGMADISPON"); (* (Sigma)dispOn (Catalog) *)
  ("\247\200", "GSELON"); (* G SelOn (Menu > Graph > Sel) *)
  ("\247\201", "TSELON"); (* T SelOn (Menu > Tabl) *)
  ("\247\202", "DSELON"); (* D SelOn (Menu > Dyna) *)
  ("\247\204", "DRAWON"); (* DrawOn *)
  ("\247\205", "ABOVERC"); (* ab/c (Set up > Frac) *)
  ("\247\206", "DOVERC"); (* d/c (Set up > Frac) *)
  ("\247\208", "FUNCOFF"); (* FuncOff (Set up > Func) *)
  ("\247\209", "SIMULOFF"); (* SimulOff (Set up > Siml) *)
  ("\247\210", "AXESOFF"); (* AxesOff (Set up > Axes) *)
  ("\247\211", "COORDOFF"); (* CoordOff (Set up > Coor) *)
  ("\247\212", "LABELOFF"); (* LabelOff (Set up > Labl) *)
  ("\247\213", "DERIVOFF"); (* DerivOff (Set up > Derv) *)
  ("\247\214", "LOCUSOFF"); (* LocusOff (Set up > Locs) *)
  ("\247\215", "SIGMADISPOFF"); (* (Sigma)dispOff (Catalog) *)
  ("\247\216", "GSELOFF"); (* G SelOff (Menu > Graph > Sel) *)
  ("\247\217", "TSELOFF"); (* T SelOff (Menu > Tabl) *)
  ("\247\218", "DSELOFF"); (* D SelOff (Menu > Dyna) *)
  ("\247\220", "DRAWOFF"); (* DrawOff *)
  ("\249\005", "TODMS");   (* >DMS (Optn > Angl) *)
  ("\249\006", "TOAPLUSBI"); (* >a+bi (Optn > Cplx) *)
  ("\249\007", "TORTHETA"); (* >r<Theta (Optn > Cplx) *)
  ("\249\008", "REAL"); (* Real (Set up > Cplx) *)
  ("\249\009", "APLUSBI"); (* a+bi (Set up > Cplx) *)
  ("\249\010", "RTHETA"); (* r<theta (Set up > Cplx) *)
  ("\249\011", "ENGON"); (* EngOn (Set up > Disp > Eng) *)
  ("\249\012", "ENGOFF"); (* EngOff (Set up > Disp > Eng) *)
  ("\249\016", "SELAZERO"); (* Sel a0 (Catalog) *)
  ("\249\017", "SELAONE"); (* Sel a1 (Catalog) *)
  ("\249\018", "CN"); (* cn (Vars > Recr > Form) *)
  ("\249\019", "CNPLUSONE"); (* cn+1 (Vars > Recr > Form) *)
  ("\249\020", "CNPLUSTWO"); (* cn+2 (Vars > Recr > Form) *)
  ("\249\021", "CZERO"); (* c0 (Vars > Recr > Rang) *)
  ("\249\022", "CONE"); (* c1 (Vars > Recr > Rang) *)
  ("\249\023", "CTWO"); (* c2 (Vars > Recr > Rang) *)
  ("\249\024", "CNSTART"); (* cnstart (Vars > Recr > Rang) *)
  ("\249\025", "INEQTYPEAND"); (* IneqTypeAnd (Catalog) *)
  ("\249\027", "FN"); (* fn (Optn > Fmem) *)
  ("\249\028", "FILE"); (* File (Set up > List) *)
  ("\249\029", "VARLIST"); (* VarList (Catalog) *)
  ("\249\030", "CLRMAT"); (* ClrMat *)
  ("\249\032", "ZOOMAUTO"); (* ZoomAuto (Zoom) *)
  ("\249\033", "XDOT"); (* Xdot (Zoom) *)
  ("\249\034", "RIGHTXDOT"); (* RightXdot (Catalog) *)
  ("\249\036", "DRAWDISTNORM"); (* DrawDistNorm (Catalog) *)
  ("\249\037", "DRAWDISTT"); (* DrawDistT (Catalog) *)
  ("\249\038", "DRAWDISTCHI"); (* DrawDistChi (Catalog) *)
  ("\249\039", "DRAWDISTF"); (* DrawDistF (Catalog) *)
  ("\249\040", "NONE"); (* None *)
  ("\249\041", "STICKLENGTH"); (* StickLength *)
  ("\249\042", "STICKHORIZ"); (* StickHoriz *)
  ("\249\043", "INEQTYPEOR"); (* IneqTypeOr (Catalog) *)
  ("\249\044", "GRAPHXG"); (* Graph X> (Sketch > Grph) *)
  ("\249\045", "GRAPHXL"); (* Graph X< (Sketch > Grph) *)
  ("\249\046", "GRAPHXGEQ"); (* Graph X>= (Sketch > Grph) *)
  ("\249\047", "GRAPHXLEQ"); (* Graph X<= (Sketch > Grph) *)
  ("\249\048", "STRJOIN"); (* StrJoin( *)
  ("\249\049", "STRLEN"); (* StrLen( *)
  ("\249\050", "STRCMP"); (* StrCmp( *)
  ("\249\051", "STRSRC"); (* StrSrc( *)
  ("\249\052", "STRLEFT"); (* StrLeft( *)
  ("\249\053", "STRRIGHT"); (* StrRight( *)
  ("\249\054", "STRMID"); (* StrMid( *)
  ("\249\055", "EXPSTR"); (* Exp>Str( (Exp to Str) *)
  ("\249\056", "EXPPAR"); (* Exp( *)
  ("\249\057", "STRUPR"); (* StrUpr( *)
  ("\249\058", "STRLWR"); (* StrLwr( *)
  ("\249\059", "STRINV"); (* StrInv( *)
  ("\249\060", "STRSHIFT"); (* StrShift( *)
  ("\249\061", "STRROTATE"); (* StrRotate( *)
  ("\249\063", "STR"); (* Str *)
  ("\249\128", "NORMPD"); (* NormPD( (Catalog) *)
  ("\249\129", "NORMCD"); (* NormCD( (Catalog) *)
  ("\249\130", "INVNORMCD"); (* InvNormCD( (Catalog) *)
  ("\249\131", "TPD"); (* tPD( (Catalog) *)
  ("\249\132", "TCD"); (* tCD( (Catalog) *)
  ("\249\133", "INVTCD"); (* InvTCD( (Catalog) *)
  ("\249\134", "CHIPD"); (* ChiPD( (Catalog) *)
  ("\249\135", "CHICD"); (* ChiCD( (Catalog) *)
  ("\249\136", "INVCHICD"); (* InvChiCD( (Catalog) *)
  ("\249\137", "FPD"); (* FPD( (Catalog) *)
  ("\249\138", "FCD"); (* FCD( (Catalog) *)
  ("\249\139", "INVFCD"); (* InvFCD( (Catalog) *)
  ("\249\140", "BINOMIALPD"); (* BinomialPD( (Catalog) *)
  ("\249\141", "BINOMIALCD"); (* BinomialCD( (Catalog) *)
  ("\249\142", "INVBINOMIALCD"); (* InvBinomialCD( (Catalog) *)
  ("\249\143", "POISSONPD"); (* PoissonPD( (Catalog) *)
  ("\249\144", "POISSONCD"); (* PoissonCD( (Catalog) *)
  ("\249\145", "INVPOISSONCD"); (* InvPoissonCD( (Catalog) *)
  ("\249\146", "GEOPD"); (* GeoPD( (Catalog) *)
  ("\249\147", "GEOCD"); (* GeoCD( (Catalog) *)
  ("\249\148", "INVGEOCD"); (* InvGeoCD( (Catalog) *)
  ("\249\149", "HYPERGEOPD"); (* HypergeoPD( (Catalog) *)
  ("\249\150", "HYPERGEOCD"); (* HypergeoCD( (Catalog) *)
  ("\249\151", "INVHYPERGEOCD"); (* InvHypergeoCD( (Catalog) *)
  ("\249\160", "SMPLSI"); (* Smpl_SI( (Catalog) *)
  ("\249\161", "SMPLSFV"); (* Smpl_SFV( (Catalog) *)
  ("\249\162", "CMPDN"); (* Cmpd_n( (Catalog) *)
  ("\249\163", "CMPDIPERCENT"); (* Cmpd_I(percent) (Catalog) *)
  ("\249\164", "CMPDPV"); (* Cmpd_PV( (Catalog) *)
  ("\249\165", "CMPDPMT"); (* Cmpd_PMT( (Catalog) *)
  ("\249\166", "CMPDFV"); (* Cmpd_FV( (Catalog) *)
  ("\249\167", "CASHNPV"); (* Cash_NPV( (Catalog) *)
  ("\249\168", "CASHIRR"); (* Cash_IRR( (Catalog) *)
  ("\249\169", "CASHPBP"); (* Cash_PBP( (Catalog) *)
  ("\249\170", "CASHNFV"); (* Cash_NFV( (Catalog) *)
  ("\249\171", "AMTBAL"); (* Amt_BAL( (Catalog) *)
  ("\249\172", "AMTINT"); (* Amt_INT( (Catalog) *)
  ("\249\173", "AMTPRN"); (* Amt_PRN( (Catalog) *)
  ("\249\174", "AMTSINT"); (* Amt_(Sigma)INT( (Catalog) *)
  ("\249\175", "AMTSPRN"); (* Amt_(Sigma)PRN( (Catalog) *)
  ("\249\176", "CNVTEFF"); (* Cnvt_EFF( (Catalog) *)
  ("\249\177", "CNVTAPR"); (* Cnvt_APR( (Catalog) *)
  ("\249\178", "COST"); (* Cost( (Catalog) *)
  ("\249\179", "SELL"); (* Sell( (Catalog) *)
  ("\249\180", "MARGIN"); (* Margin( (Catalog) *)
  ("\249\181", "PMTEND"); (* PmtEnd (Catalog) *)
  ("\249\182", "PMTBGN"); (* PmtBgn (Catalog) *)
  ("\249\183", "BONDPRC"); (* Bond_PRC( (Catalog) *)
  ("\249\184", "BONDYLD"); (* Bond_YLD( (Catalog) *)
  ("\249\185", "DATEMODETHREESIXTYFIVE"); (* DateMode365 (Catalog) *)
  ("\249\186", "DATEMODETHREESIXTY"); (* DateMode360 (Catalog) *)
  ("\249\187", "PERIODANNUAL"); (* PeriodAnnual (Catalog) *)
  ("\249\188", "PERIODSEMI"); (* PeriodSemi (Catalog) *)
  ("\249\189", "DAYSPRD"); (* Days_Prd( (Catalog) *)
  ("\249\224", "ONESAMPLEZTEST"); (* OneSampleZTest (Catalog) *)
  ("\249\225", "TWOSAMPLEZTEST"); (* TwoSampleZTest (Catalog) *)
  ("\249\226", "ONEPROPZTEST"); (* OnePropZTest (Catalog) *)
  ("\249\227", "TWOPROPZTEST"); (* TwoPropZTest (Catalog) *)
  ("\249\232", "ONESAMPLETTEST"); (* OneSampleTTest (Catalog) *)
  ("\249\233", "TWOSAMPLETTEST"); (* TwoSampleTTest (Catalog) *)
  ("\249\234", "LINREGTTEST"); (* LinRegTTest (Catalog) *)
  ("\249\235", "CHIGOFTEST"); (* ChiGOFTest (Catalog) *)
  ("\249\236", "CHITEST"); (* ChiTest (Catalog) *)
  ("\249\237", "TWOSAMPLEFTEST"); (* TwoSampleFTest (Catalog) *)
  ("\249\238", "ONEWAYANOVA"); (* OneWayANOVA (Catalog) *)
  ("\249\239", "TWOWAYANOVA"); (* TwoWayANOVA (Catalog) *)
  ("\249\240", "XONEINVN"); (* x1InvN (Catalog) *)
  ("\249\241", "XTWOINVN"); (* x2InvN (Catalog) *)
  ("\249\242", "XINV"); (* xInv (Catalog) *)
  ("\249\251", "ZLOW"); (* zLow (Catalog) *)
  ("\249\252", "ZUP"); (* zUp (Catalog) *)
  ("\249\253", "TLOW"); (* tLow (Catalog) *)
  ("\249\254", "TUP"); (* tUp (Catalog) *)
];;

let symbols = [ (* From the menu "Char" *)
  (* MATH: 115 symbols *)
  "\043"; "\045"; "\169"; "\185"; "\094"; "\042"; "\047"; "\061"; "\017";
  "\060"; "\062"; "\016"; "\018"; "\135";
  "\229\190"; "\229\191"; "\230\176"; "\230\177"; "\229\163"; "\229\164";
  "\127\080"; "\229\176"; "\015"; "\230\079"; "\127\083"; "\229\066"; "\134";
  "\229\081"; "\229\079"; "\230\187"; "\230\183"; "\230\184"; "\230\185";
  "\229\192"; "\229\193"; "\229\194"; "\229\195"; "\229\196"; "\229\197";
  "\229\198"; "\229\199"; "\229\200"; "\229\201"; "\229\202"; "\229\203"; 
  "\229\204"; "\229\205"; "\229\206"; "\229\207"; "\229\208"; "\229\209"; 
  "\229\210"; "\229\211"; "\229\212"; "\229\213"; "\229\214"; "\229\215"; 
  "\229\216"; "\229\217"; "\229\218"; "\229\219"; "\229\220"; "\229\221"; 
  "\229\222"; "\229\223"; "\194"; "\195"; "\203"; "\204"; "\127\199"; "\127\084";
  "\140"; "\156"; "\172"; "\188"; "\230\189"; "\230\190"; "\230\191"; "\230\192";
  "\230\193"; "\230\194"; "\230\195"; "\230\196"; "\230\197"; "\230\198";
  "\230\199"; "\230\200"; "\230\201"; "\230\202"; "\230\203"; "\230\214";
  "\230\204"; "\230\205"; "\230\206"; "\230\207"; "\230\208"; "\230\209";
  "\230\210"; "\230\211"; "\230\212"; "\230\213"; "\230\178"; "\230\179";
  "\230\180"; "\230\181"; "\230\188"; "\230\182"; "\230\215"; "\230\216";
  "\230\217"; "\230\218"; "\230\219"; "\230\220"; "\230\221"; "\230\222";

  (* SYBL: 83 symbols *)
  "\033"; "\034"; "\035"; "\036"; "\037"; "\038"; "\039"; "\040"; "\041";
  "\044"; "\046"; "\058"; "\059"; "\063"; "\064"; "\091"; "\092"; "\093";
  "\095"; "\096"; "\123"; "\124"; "\125"; "\126"; "\019"; "\181"; "\187";
  "\229\148"; "\229\149"; "\229\150"; "\229\151"; "\229\152"; "\229\144"; "\229\145"; "\229\146";
  "\229\147"; "\229\153"; "\229\154"; "\229\155"; "\229\156"; "\229\157"; "\229\158"; "\229\161";
  "\229\159"; "\229\162"; "\229\160"; "\229\165"; "\229\166"; "\229\167"; "\229\181"; "\229\182";
  "\229\184"; "\229\185"; "\229\186"; "\229\187"; "\229\188"; "\230\144"; "\230\145"; "\230\146"; 
  "\230\147"; "\230\148"; "\230\149"; "\230\150"; "\230\151"; "\230\152"; "\230\153"; "\230\154";
  "\230\155"; "\230\156"; "\230\157"; "\230\158"; "\230\159"; "\230\160"; "\230\161"; "\230\162";
  "\230\163"; "\230\164"; "\230\165"; "\230\166"; "\230\167"; "\230\168"; "\230\169"; "\230\170";
  
  (* Upper-case special: 110 symbols *)
  "\229\064"; "\229\065"; "\229\066"; "\229\067"; "\229\068"; "\229\069"; "\229\070"; "\229\071";
  "\229\072"; "\229\073"; "\229\074"; "\229\075"; "\229\076"; "\229\077"; "\229\078"; "\229\079";
  "\229\080"; "\229\081"; "\229\083"; "\229\084"; "\229\085"; "\229\086"; "\229\087"; "\229\088";
  "\229\001"; "\229\002"; "\229\003"; "\229\004"; "\229\005"; "\229\006"; "\229\007"; "\229\008";
  "\229\009"; "\229\010"; "\229\011"; "\229\012"; "\229\013"; "\229\014"; "\229\015"; "\229\016"; 
  "\229\017"; "\229\018"; "\229\019"; "\229\020"; "\229\021"; "\229\022"; "\229\023"; "\229\024"; 
  "\229\025"; "\229\026"; "\229\027"; "\229\028"; "\229\029"; "\229\030"; "\229\032"; "\229\033"; 
  "\229\034"; "\229\035"; "\229\036"; "\229\037"; "\229\038"; "\229\039"; "\229\040"; "\229\041"; 
  "\229\042"; "\229\043"; "\229\044"; "\229\045"; "\229\046"; "\229\047"; "\229\048"; "\229\049"; 
  "\229\050"; "\229\051"; "\229\052"; "\229\053"; "\229\096"; "\229\097"; "\229\098"; "\229\099";
  "\229\100"; "\229\101"; "\229\102"; "\229\103"; "\229\104"; "\229\105"; "\229\106"; "\229\107";
  "\229\108"; "\229\109"; "\229\110"; "\229\111"; "\229\112"; "\229\113"; "\229\114"; "\229\115";
  "\229\116"; "\229\117"; "\229\118"; "\229\119"; "\229\120"; "\229\121"; "\229\122"; "\229\123";
  "\229\124"; "\229\125"; "\229\126"; "\229\128"; "\229\129"; "\229\130";

  (* Lower-case special: 112 symbols *)
  "\230\064"; "\230\065"; "\230\066"; "\230\067"; "\230\068"; "\230\069"; "\230\070"; "\230\071";
  "\230\072"; "\230\073"; "\230\074"; "\230\075"; "\230\076"; "\230\077"; "\230\078"; "\230\079";
  "\230\080"; "\230\081"; "\230\082"; "\230\083"; "\230\084"; "\230\085"; "\230\086"; "\230\087";
  "\230\088"; "\230\001"; "\230\002"; "\230\003"; "\230\004"; "\230\005"; "\230\006"; "\230\007";
  "\230\008"; "\230\009"; "\230\010"; "\230\011"; "\230\012"; "\230\013"; "\230\014"; "\230\015";
  "\230\016"; "\230\017"; "\230\018"; "\230\019"; "\230\020"; "\230\021"; "\230\022"; "\230\023";
  "\230\024"; "\230\025"; "\230\026"; "\230\027"; "\230\028"; "\230\029"; "\230\030"; "\230\031";
  "\230\032"; "\230\033"; "\230\034"; "\230\035"; "\230\036"; "\230\037"; "\230\038"; "\230\039";
  "\230\040"; "\230\041"; "\230\042"; "\230\043"; "\230\044"; "\230\045"; "\230\046"; "\230\047";
  "\230\048"; "\230\049"; "\230\050"; "\230\051"; "\230\052"; "\230\053"; "\230\096"; "\230\097";
  "\230\098"; "\230\099"; "\230\100"; "\230\101"; "\230\102"; "\230\103"; "\230\104"; "\230\105";
  "\230\106"; "\230\107"; "\230\108"; "\230\109"; "\230\110"; "\230\111"; "\230\112"; "\230\113";
  "\230\114"; "\230\115"; "\230\116"; "\230\117"; "\230\118"; "\230\119"; "\230\120"; "\230\121";
  "\230\122"; "\230\123"; "\230\124"; "\230\125"; "\230\126"; "\230\128"; "\230\129"; "\230\130";

  (* Indices: 52 symbols *)
  "\231\065"; "\231\066"; "\231\067"; "\231\068"; "\231\069"; "\231\070"; "\231\071"; "\231\072";
  "\231\073"; "\231\074"; "\231\075"; "\231\076"; "\231\077"; "\231\078"; "\231\079"; "\231\080";
  "\231\081"; "\231\082"; "\231\083"; "\231\084"; "\231\085"; "\231\086"; "\231\087"; "\231\088";
  "\231\089"; "\231\090"; "\231\097"; "\231\098"; "\231\099"; "\231\100"; "\231\101"; "\231\102";
  "\231\103"; "\231\104"; "\231\105"; "\231\106"; "\231\107"; "\231\108"; "\231\109"; "\231\110";
  "\231\111"; "\231\112"; "\231\113"; "\231\114"; "\231\115"; "\231\116"; "\231\117"; "\231\118";
  "\231\119"; "\231\120"; "\231\121"; "\231\122"
];;

(* One-character commands *)
(* Subset of commands. Some are also present in symbols. *)
let onecharcomm = ["\001"; "\002";"\003";"\004";"\005";"\006";"\007";"\008";"\009";"\010";"\011";
  "\014"; "\015"; "\016"; "\017"; "\018"; "\019"; "\032"; "\034"; "\040"; "\041"; "\058"; "\060";
  "\061"; "\062"; "\063"; "\091"; "\093"; "\123"; "\125"; "\135"; "\136"; "\137"; "\139"; "\140";
  "\152"; "\153"; "\155"; "\156"; "\168"; "\169"; "\171"; "\172"; "\181"; "\185"; "\187"; "\188";
  "\205"; "\206"; "\208"];;

(* Symbols that have a visual representation in graphic mode (the others print a space) *)
let symbols_graphic = [
  (* MATH *)
  "\043"; "\045"; "\169"; "\185"; "\094"; "\042"; "\047"; "\061"; "\017";
  "\060"; "\062"; "\016"; "\018"; "\135";
  "\229\190"; "\229\191";
  "\127\080"; "\229\176"; "\015"; "\230\079"; "\127\083"; "\134";
  "\229\081"; "\230\187";
  "\229\192"; "\229\193"; "\229\194"; "\229\195"; "\229\196"; "\229\197";
  "\229\198"; "\229\199"; "\229\200"; "\229\201"; "\229\202"; "\229\203";
  "\229\204"; "\229\205"; "\229\206"; "\229\207"; "\229\208"; "\229\209";
  "\229\210"; "\229\211"; "\229\212"; "\229\213"; "\229\214"; "\229\215";
  "\229\216"; "\229\217"; "\229\218"; "\229\219"; "\229\220"; "\229\221"; 
  "\229\222"; "\229\223"; "\194"; "\195"; "\203"; "\204"; "\127\199"; "\127\084";
  "\140"; "\156"; "\172"; "\188";

  (* SYBL *)
  "\033"; "\035"; "\036"; "\037"; "\038"; "\039"; "\040"; "\041";
  "\044"; "\046"; "\058"; "\059"; "\063"; "\064"; "\091"; "\092"; "\093";
  "\095"; "\096"; "\123"; "\124"; "\125"; "\126"; "\019"; "\181"; "\187";
  "\230\144"; "\230\145"; "\230\146"; 
  "\230\147"; "\230\154";
  "\230\155"; "\230\156"; "\230\157"; "\230\158"; "\230\165"; "\230\166";
  
  (* Upper-case special *)
  "\229\067"; "\229\081";

  (* Lower-case special *)
  "\230\071";
  "\230\075"; "\230\079";
  "\230\081";

  (* Indices *)
  "\231\065"; "\231\066"; "\231\067"; "\231\068"; "\231\069"; "\231\070"; "\231\071"; "\231\072";
  "\231\073"; "\231\074"; "\231\075"; "\231\076"; "\231\077"; "\231\078"; "\231\079"; "\231\080";
  "\231\081"; "\231\082"; "\231\083"; "\231\084"; "\231\085"; "\231\086"; "\231\087"; "\231\088";
  "\231\089"; "\231\090"; "\231\097"; "\231\098"; "\231\099"; "\231\100"; "\231\101"; "\231\102";
  "\231\103"; "\231\104"; "\231\105"; "\231\106"; "\231\107"; "\231\108"; "\231\109"; "\231\110";
  "\231\111"; "\231\112"; "\231\113"; "\231\114"; "\231\115"; "\231\116"; "\231\117"; "\231\118";
  "\231\119"; "\231\120"; "\231\121"; "\231\122"
];;


(** Visual representation **)

(* #use "picture_editor/bmp_reader.ml" *)

(* Each character's visual representation is a 7*5 monochromatic image in text mode.
  It is encoded as an array of 35 booleans, line by line, starting from the upper one. *)

(* Returns an empty representation array *)
let new_vr () = Array.make 35 false;;

(* Returns the representation array of the symbol at line j, column i of the calculator screen
  in the image vischar *)
(* i between 0 and 20, j between 0 and 6 *)
let read_symbol (vischar : bool array array) (i : int) (j : int) =
  let t = new_vr () in
  for y = 0 to 6 do
    for x = 0 to 4 do
      t.(5*y+x) <- vischar.(64-1-j*8-y).(x+1+i*6)
    done;
  done;
  t;;

(* For testing purposes *)
let print_repr (t : bool array) =
  for j = 0 to 6 do
    for i = 0 to 4 do
      if t.(5*j+i)
        then print_char '#'
        else print_char ' '
    done;
    print_newline ();
  done;
  print_newline ();;

(* Creation of the hash table that stores the visual representation of each symbol
  in text mode (Standard print, Locate) *)
let visual () =
  let vischar1 = read_mono_bmp "data/char1.bmp" in
  let vischar2 = read_mono_bmp "data/char2.bmp" in
  let vischar3 = read_mono_bmp "data/char3.bmp" in
  let vischar4 = read_mono_bmp "data/char4.bmp" in
  let vischar5 = read_mono_bmp "data/char5.bmp" in
  let vischar6 = read_mono_bmp "data/char6.bmp" in
  let vischar7 = read_mono_bmp "data/char7.bmp" in
  let t = Hashtbl.create 580 in
  (* Reminder: main characters encodings:
    32 = SPACE
    48 ... 57 = 0 ... 9
    65 ... 90 = A ... Z
    97 ... 122 = a ... z
    44 = ,
    46 = .
  *)
  (* Space *)
  Hashtbl.add t " " (new_vr ());
  (* Digits *)
  for i = 48 to 57 do
    Hashtbl.add t (Char.escaped (Char.chr i)) (read_symbol vischar2 (i-48) 3);
  done;
  (* Upper-case letters *)
  for i = 65 to 90 do
    Hashtbl.add t (Char.escaped (Char.chr i)) (read_symbol vischar1 ((i-65) mod 21) (i/(65+21)));
  done;
  (* Lower-case letters *)
  for i = 97 to 122 do
    Hashtbl.add t (Char.escaped (Char.chr i)) (read_symbol vischar1 ((i-97) mod 21) (2+(i/(97+21))));
  done;
  (* Comma *)
  Hashtbl.add t "," (read_symbol vischar1 0 4);
  (* Point *)
  Hashtbl.add t "." (read_symbol vischar1 1 4);
  (* Disp *)
  Hashtbl.add t "\012" (read_symbol vischar1 2 4);
  (* EOL *)
  Hashtbl.add t "\013" (read_symbol vischar1 0 5);
  let l = ref symbols in
  (* Reads n representations in the image vischar
    for the n first elements of list l *)
  let read_all n vischar =
    for i = 0 to n-1 do
      Hashtbl.add t (List.hd !l) (read_symbol vischar (i mod 19) (i/19));
      l := List.tl !l
    done;
  in
  (* MATH: 115 symbols *)
  read_all 114 vischar3;
  (* Last one: "\230\222" *)
  Hashtbl.add t (List.hd !l) (read_symbol vischar3 19 5);
  l := List.tl !l;
  (* SYBL: 83 symbols *)
  read_all 83 vischar4;
  (* Upper-case special: 110 symbols *)
  read_all 110 vischar5;
  (* Lower-case special: 112 symbols *)
  read_all 112 vischar6;
  (* Indices: 52 symbols *)
  read_all 52 vischar7;
  (* One-character commands *)
  (* Some are already present in the list symbols; they are not re-added *)
  let add_elt i s =
    try
      let _ = Hashtbl.find t s in
      (* print_endline ("Already exists: "^(string_of_int (Char.code s.[0]))); *)
      ()
    with
      | Not_found -> Hashtbl.add t s (read_symbol vischar2 (i mod 21) (i/21))
  in
  List.iteri add_elt onecharcomm;
  t;;

(** Visual representation in graphic mode **)

(* The size of the representation varies depending on the symbol.
  All symbols are 5 pixels tall and between 1 and 5 pixels wide. *)
(* Each representation is encoded as an array of 5*w booleans
  if the character is w pixels wide; line by line, starting from the upper one.
  The length can be retrieved with (List.length enc)/5. *)

(** /!\ ONE EXCEPTION: the backslash (\092) has height 6.
    For it we store an array of size 6*3.
    It needs to be properly handled by the interpreter. **)

let new_gvr (w : int) : bool array =
  Array.make (5*w) false;;

(* Detects if the vertical line of 5 pixels on line j (between 0 and 9) and
  abscissa i (between 0 and 126) of image gphvischar is blank *)
let is_white (gphvischar : bool array array) (j : int) (i : int) : bool =
  not (List.exists (fun y -> gphvischar.(62-6*j-y).(i)) [0;1;2;3;4]);;

(* Returns the length of the representation of the character starting at
  abscissa i (between 0 and 126) of line j (between 0 and 8) of image gphvischar *)
(* k (between 1 and 3) is the number of the image gphvischar (ex: 2 for gphvischar2) *)
let repr_length (gphvischar : bool array array) (k : int) (j : int) (i : int) : int =
  (* Characters who have white vertical lines in their representations are hard-coded. *)
  try
    List.assoc (k,j,i)
      [ ((1,2,5)(* 1 *),          3);
        ((1,4,4)(* . *),          3);
        ((1,6,1)(* uminus *),     3);
        ((1,6,31)(* ^-1 *),       4);
        ((1,6,54)(* small 10 *),  5);
        ((2,0,56)(* uminus *),    3);
        ((2,1,21)(* ^-1 *),       4);
        ((2,1,82)(* _-1 *),       4);
        ((2,3,37)(* . *),         3);
        ((2,3,94)(* small 10 *),  5)]
  with
    | Not_found ->
      (* Abscissa i is now assumed to contain at least one black pixel *)
      let next_white = List.find (is_white gphvischar j) [i; i+1; i+2; i+3; i+4; i+5] in
      next_white - i;;

(* Returns the representation array of the symbol at line j, abscissa i of image gphvischar,
  if it has length l.
  j between 0 and 8, i between 0 and 126, l between 1 and 5 *)
let read_graphic_symbol (gphvischar : bool array array) (j : int) (i : int)  (l : int) =
  let t = new_gvr l in
  for y = 0 to 4 do
    for x = 0 to l-1 do
      t.(l*y+x) <- gphvischar.(62-6*j-y).(i+x)
    done;
  done;
  t;;

(* For testing purposes *)
let print_g_repr (t : bool array) =
  let l = (Array.length t)/5 in
  for j = 0 to 4 do
    for i = 0 to l-1 do
      if t.(l*j+i)
        then print_char '#'
        else print_char ' '
    done;
    print_newline ();
  done;
  print_newline ();;

(* Creation of the hash table that stores the visual representation of each symbol
  in graphic mode (Graphic "Text" function) *)
let visualgraph () =
  let gphvischar1 = read_mono_bmp "data/gphchar1.bmp" in
  let gphvischar2 = read_mono_bmp "data/gphchar2.bmp" in
  let t = Hashtbl.create 280 in
  (* Space *)
  Hashtbl.add t " " (new_gvr 3);
  (* Double-quote *)
  Hashtbl.add t "\034" (new_gvr 3);
  (* Digits *)
  let x = ref 1 in
  for i = 48 to 57 do
    let rlen = repr_length gphvischar1 1 2 !x in
    Hashtbl.add t (Char.escaped (Char.chr i)) (read_graphic_symbol gphvischar1 2 !x rlen);
    x := !x + rlen + 1;
  done;
  (* Upper-case letters *)
  x := 1;
  for i = 65 to 90 do
    let rlen = repr_length gphvischar1 1 0 !x in
    Hashtbl.add t (Char.escaped (Char.chr i)) (read_graphic_symbol gphvischar1 0 !x rlen);
    x := !x + rlen + 1;
  done;
  (* Lower-case letters *)
  x := 1;
  for i = 97 to 122 do
    let rlen = repr_length gphvischar1 1 1 !x in
    Hashtbl.add t (Char.escaped (Char.chr i)) (read_graphic_symbol gphvischar1 1 !x rlen);
    x := !x + rlen + 1;
  done;
  (* Comma *)
  Hashtbl.add t "," (read_graphic_symbol gphvischar1 4 1 2);
  (* Point *)
  Hashtbl.add t "." (read_graphic_symbol gphvischar1 4 4 3);
  (* Disp *)
  Hashtbl.add t "\012" (read_graphic_symbol gphvischar1 4 8 3);
  (* One-character commands *)
  (* Some are already present in the list symbols; they are not re-added *)
  let x = ref 1 in
  let c = ref 0 in
  let add_gelt s =
    try
      let _ = Hashtbl.find t s in
      ()
    with
      | Not_found ->
        (let rlen = repr_length gphvischar1 1 (5+(!c/28)) !x in
        Hashtbl.add t s (read_graphic_symbol gphvischar1 (5+(!c/28)) !x rlen);
        x := !x + rlen + 1;
        incr c;
        if !c=28 then
          x := 1)
  in
  List.iter add_gelt onecharcomm;
  let l = ref symbols_graphic in
  (* Reads n representations in line j of the image gphvischar
    for the n first elements of list l *)
  let read_all_graph j n =
    let x = ref 1 in
    for i = 0 to n-1 do
      let rlen = repr_length gphvischar2 2 j !x in
      (try
        let _ = Hashtbl.find t (List.hd !l) in
        ()
      with
        | Not_found ->
          Hashtbl.add t (List.hd !l) (read_graphic_symbol gphvischar2 j !x rlen));
      x := !x + rlen + 1;
      l := List.tl !l
    done;
  in
  (* 9 lines *)
  List.iteri read_all_graph [29;31;6;30;7;2;4;26;26];
  (* Final step: the one exception, the backslash (\092),
    which is the only symbol that has a representation of
    height 6 (seriously?). *)
  let vbackslash = Hashtbl.find t "\092" in
  Hashtbl.remove t "\092";
  Hashtbl.add t "\092" (Array.append vbackslash [|false;false;true|]);
  t;;