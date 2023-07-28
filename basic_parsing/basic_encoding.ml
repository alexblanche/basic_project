(* Encoding of characters and commands and visual representation *)

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
  ("\001", "FEMTO");    (* Lower-case f (femto) (Optn > Esym) *)
  ("\002", "PICO");    (* Lower-case p (pico) (Optn > Esym) *)
  ("\003", "NANO");    (* nu (nano) (Optn > Esym) *)
  ("\004", "MICRO");    (* mu (micro) (Optn > Esym) *)
  ("\005", "MILLI");    (* Lower-case m (milli) (Optn > Esym) *)
  ("\006", "KILO");    (* Lower-case k (kilo) (Optn > Esym) *)
  ("\007", "MEGA");    (* Upper-case M (mega) (Optn > Esym) *)
  ("\008", "GIGA");    (* Upper-case G (giga) (Optn > Esym) *)
  ("\009", "TERA");    (* Upper-case T (tera) (Optn > Esym) *)
  ("\010", "PETA");    (* Upper-case P (peta) (Optn > Esym) *)
  ("\011", "EXA");    (* Upper-case E (exa) (Optn > Esym) *)
  ("\012", "DISP");   (* Black triangle "disp" *)
  ("\013", "EOL");    (* End of line *)
  ("\014", "ASSIGN"); (* -> *)
  ("\015", "TIMESTENPOWER"); (* E (x10^ sign) *)
  ("\016", "LEQ"); (* Less or equal <= *)
  ("\017", "DIFFERENT"); (* Different (not-equal) sign *)
  ("\018", "GEQ"); (* Greater or equal >= *)
  ("\019", "IMPL"); (* => *)
  (* \020 ... \031 *)
  ("\034", "QUOTE");  (* Double-quote *)
  ("\040", "LPAR");   (* ( *)
  ("\041", "RPAR");   (* ) *)
  ("\048", "0");      (* Int: 48 = 0, 49 = 1, ..., 57 = 9 *)
  ("\058", "COLON");  (* : (equivalent to EOL) *)
  ("\060", "LESS");  (* < *)
  ("\061", "EQUAL");  (* = *)
  ("\062", "GREATER");  (* > *)
  ("\063", "QMARK");  (* ? (Question mark) *)
  ("\065", "A");      (* Letters: 65 = A, 66 = B, ..., 90 = Z *)
  ("\091", "LSQBRACKET"); (* [ *)
  ("\093", "RSQBRACKET"); (* ] *)
  ("\123", "LBRACKET");   (* { *)
  ("\125", "RBRACKET");   (* } *)
  (* \127: first byte of two-byte symbols *)
  ("\128", "POLPAR");   (* Pol( (Optn > Angl) *)
  ("\129", "SIN");   (* sin *)
  ("\130", "COS");   (* cos *)
  ("\131", "TAN");   (* tan *)
  (* \132 *)
  ("\133", "LN");   (* ln *)
  ("\134", "SQRT");   (* Square root sign *)
  ("\135", "UMINUS");   (* Unary minus sign *)
  ("\136", "THICKP");  (* Thick P (Optn > Prob) *)
  ("\137", "PLUS");   (* Plus sign *)
  (* \138 *)
  ("\139", "POWER2");   (* ^2 *)
  ("\140", "COORDSIGN");   (* Small white square (Optn > Angl) *)
  ("\141", "INTEGRALPAR");   (* Integral sign + left parenthesis (Optn > Calc) *)
  (* \142 \143 \144 *)
  ("\145", "ARCSIN");   (* sin^-1 *)
  ("\146", "ARCCOS");   (* cos^-1 *)
  ("\147", "ARCTAN");   (* tan^-1 *)
  (* \148 *)
  ("\149", "LOG");   (* log *)
  ("\150", "CURT");   (* Cubic root *)
  ("\151", "ABS");   (* Absolute value (Optn > Cplx) *)
  ("\152", "THICKC");  (* Thick C (Optn > Prob) *)
  ("\153", "MINUS");   (* Minus sign *)
  (* \154 *)
  ("\155", "POWERMINUS1");   (* -1 (^-1 sign) *)
  ("\156", "DEGREESIGN");   (* o Degree sign *)
  (* \157 \158 \159 *)
  ("\160", "RECPAR");   (* Rec( (Optn > Angl) *)
  ("\161", "SINH");   (* sinh (Optn > Hyp) *)
  ("\162", "COSH");   (* cosh (Optn > Hyp) *)
  ("\163", "TANH");   (* tanh (Optn > Hyp) *)
  (* \164 *)
  ("\165", "EPOWER");   (* e^ *)
  ("\166", "INT");   (* Int (Optn > Num) *)
  (* \167 *)
  ("\168", "POWER");   (* ^ *)
  ("\169", "TIMES");  (* Multiplication sign *)
  (* \170 *)
  ("\171", "EXCLAMATIONMARK");  (* ! (Optn > Prob) *)
  ("\172", "RADIANSIGN");   (* r Radian sign *)
  (* \173 \174 \175 \176 *)
  ("\177", "ARCSINH");  (* sinh^-1 (Optn > Hyp) *)
  ("\178", "ARCCOSH");  (* cosh^-1 (Optn > Hyp) *)
  ("\179", "ARCTANH");  (* tanh^-1 (Optn > Hyp) *)
  (* \180 *)
  ("\181", "TENPOWER");   (* 10 (10^ sign) *)
  ("\182", "FRAC");   (* Frac (Optn > Num) *)
  (* \183 *)
  ("\184", "NSQRT");   (* N-th square root sign *)
  ("\185", "DIVIDED");   (* Division sign *)
  (* \186 *)
  ("\187", "FRACSIGN");   (* Fraction sign *)
  ("\188", "GRADSIGN");   (* g Grad sign *)
  (* \189 \190 \191 *)
  ("\192", "ANS");  (* Ans *)
  ("\193", "RAN");  (* Ran# (Optn > Prob) *)
  (* \196 ... \202 *)
  ("\205", "SMALLR");  (* r (complex radius) *)
  ("\206", "THETA");  (* Theta (complex angle) *)
  (* \207 *)
  ("\208", "PI");  (* Pi sign *)
  ("\209", "CLS");  (* Cls (Sketch) *)
  (* \210 *)
  ("\211", "RND");   (* Rnd (Optn > Num) *)
  (* \212 ... \216 *)
  ("\217", "NORM"); (* Norm (Set up > Disp) *)
  ("\218", "DEG"); (* Deg (Set up > Angl) *)
  ("\219", "RAD"); (* Rad (Set up > Angl) *)
  ("\220", "GRA"); (* Gra (Set up > Angl) *)
  ("\221", "ENG"); (* Eng (Set up > Disp > Eng) *)
  ("\222", "INTG");   (* Intg (Optn > Num) *)
  (* \223 *)
  ("\224", "PLOT"); (* Plot (Sketch > Plot) *)
  ("\225", "LINE"); (* Line (Sketch > Line) *)
  ("\226", "LBL"); (* Lbl *)
  ("\227", "FIX"); (* Fix (Set up > Disp) *)
  ("\228", "SCI"); (* Sci (Set up > Disp) *)
  (* \229 ... \231: first byte of two-byte symbols *)
  ("\232", "DSZ"); (* Dsz *)
  ("\233", "ISZ"); (* Isz *)
  ("\234", "FACTOR"); (* Factor (Zoom) *)
  ("\235", "VIEWWINDOW"); (* ViewWindow (V-Window) *)
  ("\236", "GOTO"); (* Goto *)
  ("\237", "PROG"); (* Prog *)
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
  ("\251", "PPAR"); (* P( (Optn > Prob) *)
  ("\252", "QPAR"); (* Q( (Optn > Prob) *)
  ("\253", "RPAR"); (* R( (Optn > Prob) *)
  ("\254", "tPAR"); (* t( (Optn > Prob) *)
  (* \255 *)
  
  (* Two-byte symbols *)
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
  ("\127\058", "MOD");   (* MOD( (Optn > Num) *)
  ("\127\059", "MODEXP");   (* MOD_Exp( (Optn > Num) *)
  ("\127\060", "GCD");   (* GCD( (Optn > Num) *)
  ("\127\061", "LCM");   (* LCM( (Optn > Num) *)
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
  ("\127\133", "LOGAB"); (* logab( (Optn > Calc) *)
  ("\127\134", "RNDFIX");   (* RndFix( (Optn > Num) *)
  ("\127\135", "RANINT"); (* RanInt#( (Optn > Prob) *)
  ("\127\136", "RANLIST"); (* RanList#( (Optn > Prob) *)
  ("\127\137", "RANBIN"); (* RanBin#( (Optn > Prob) *)
  ("\127\138", "RANNORM"); (* RanNorm#( (Optn > Prob) *)
  ("\127\143", "GETKEY"); (* Getkey *)
  ("\127\156", "TOSIMP"); (* >Simp (Optn > Calc) *)
  ("\127\176", "AND"); (* And (Optn > Logic) *)
  ("\127\177", "OR"); (* Or (Optn > Logic) *)
  ("\127\179", "NOT"); (* Not (Optn > Logic) *)
  ("\127\180", "XOR"); (* Xor (Optn > Logic) *)
  ("\127\188", "INTDIV"); (* Int division (Optn > Calc) *)
  ("\127\189", "RMDR"); (* Rmdr (Optn > Calc) *)
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
  ("\247\024", "CLRTEXT"); (* ClrText *)
  ("\247\025", "CLRGRAPH"); (* ClrGraph *)
  ("\247\026", "CLRLIST"); (* ClrList *)
  ("\247\028", "SLNORMAL"); (* S-L-Normal (Set up > S/L) *)
  ("\247\029", "SLTHICK"); (* S-L-Thick (Set up > S/L) *)
  ("\247\030", "SLBROKEN"); (* S-L-Broken (Set up > S/L) *)
  ("\247\031", "SLDOT"); (* S-L-Dot (Set up > S/L) *)
  ("\247\032", "DRAWGRAPH"); (* DrawGraph *)
  ("\247\034", "DRAWDYNA"); (* DrawDyna *)
  ("\247\035", "DRAWSTAT"); (* DrawStat *)
  ("\247\043", "NORMALG"); (* NormalG (Menu > Graph > Styl) *)
  ("\247\044", "THICKG"); (* ThickG (Menu > Graph > Styl) *)
  ("\247\045", "BROKENTHICKG"); (* BrokenThickG (Menu > Graph > Styl) *)
  ("\247\060", "BAR"); (* Bar (DrawStat setup) *)
  ("\247\063", "DOTG"); (* DotG (Menu > Graph > Styl) *)
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
  ("\247\120", "BGNONE"); (* BG-None (Set up > Back) *)
  ("\247\121", "BGPICT"); (* BG-Pict (Set up > Back) *)
  ("\247\122", "GRIDOFF"); (* GridOff (Set up > Grid) *)
  ("\247\125", "GRIDON"); (* GridOn (Set up > Grid) *)
  ("\247\128", "DVAR"); (* D Var (Menu > Dyna) *)
  ("\247\140", "SKETCHNORMAL"); (* SketchNormal (Sketch > Styl) *)
  ("\247\141", "SKETCHTHICK"); (* SketchThick (Sketch > Styl) *)
  ("\247\142", "SKETCHBROKEN"); (* SketchBroken (Sketch > Styl) *)
  ("\247\143", "SKETCHDOT"); (* SketchDot (Sketch > Styl) *)
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
  ("\247\190", "YDRAWSPEEDNORM"); (* Y=DrawSpeedNorm (Set up > Y-Spd) *)
  ("\247\191", "YDRAWSPEEDHIGH"); (* Y=DrawSpeedHigh (Set up > Y-Spd) *)
  ("\247\192", "FUNCON"); (* FuncOn (Set up > Func) *)
  ("\247\193", "SIMULON"); (* SimulOn (Set up > Siml) *)
  ("\247\194", "AXESON"); (* AxesOn (Set up > Axes) *)
  ("\247\195", "COORDON"); (* CoordOn (Set up > Coor) *)
  ("\247\196", "LABELON"); (* LabelOn (Set up > Labl) *)
  ("\247\197", "DERIVON"); (* DerivOn (Set up > Derv) *)
  ("\247\198", "LOCUSON"); (* LocusOn (Set up > Locs) *)
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
  ("\249\027", "FN"); (* fn (Optn > Fmem) *)
  ("\249\028", "FILE"); (* File (Set up > List) *)
  ("\249\030", "CLRMAT"); (* ClrMat *)
  ("\249\032", "ZOOMAUTO"); (* ZoomAuto (Zoom) *)
  ("\249\040", "NONE"); (* None *)
  ("\249\041", "STICKLENGTH"); (* StickLength *)
  ("\249\042", "STICKHORIZ"); (* StickHoriz *)
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
  "\156"; "\172"; "\188"; "\230\189"; "\230\190"; "\230\191"; "\230\192";
  "\230\193"; "\230\194"; "\230\195"; "\230\196"; "\230\197"; "\230\198";
  "\230\199"; "\230\200"; "\230\201"; "\230\202"; "\230\203"; "\230\214";
  "\230\203"; "\230\204"; "\230\205"; "\230\206"; "\230\207"; "\230\208";
  "\230\209"; "\230\210"; "\230\211"; "\230\212"; "\230\213"; "\230\178";
  "\230\179"; "\230\180"; "\230\181"; "\230\188"; "\230\182"; "\230\215"; 
  "\230\216"; "\230\217"; "\230\218"; "\230\219"; "\230\220"; "\230\221";
  "\230\222";

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


(** Visual representation **)

#use "picture_editor/bmp_reader.ml";;

(* Main characters encodings:
 32 = SPACE
 48 ... 57 = 0 ... 9
 65 ... 90 = A ... Z
 97 ... 122 = a ... z
 44 = ,
 46 = .
  *)

(* Each character's visual representation is a 7*5 monochromatic image.
  It is encoded as a sequence of 35 booleans, line by line, starting from the upper one *)

(* Creation of the hash table that stores the visual representation of each symbol *)
  let visual () =
  (* let vischar1 = read_mono_bmp "data/char1.bmp" in *)
  let vischar2 = read_mono_bmp "data/char2.bmp" in
  (* let vischar3 = read_mono_bmp "data/char3.bmp" in
  let vischar4 = read_mono_bmp "data/char4.bmp" in
  let vischar5 = read_mono_bmp "data/char5.bmp" in
  let vischar6 = read_mono_bmp "data/char6.bmp" in
  let vischar7 = read_mono_bmp "data/char7.bmp" in *)
  let new_vr () = Array.make 35 false in
  let t = Hashtbl.create 500 in
  (* Space *)
  Hashtbl.add t "\032" (new_vr ()); 
  (* Digits *)
  for i = 48 to 57 do
    let t = new_vr () in
    for y = 0 to 6 do
      for x = 0 to 4 do
        t.(5*y+x) <- vischar2.(64-1-3*8-y).(x+1+(i-48)*6)
      done;
    done;
    (* Test: visual check *)
    print_int (i-48);
    print_newline ();
    for i = 0 to 34 do
      print_int (if t.(i) then 1 else 0);
      if (i mod 5) = 4 then print_newline()
    done;
  done;
  t;;