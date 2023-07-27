(* Basic enconding *)

(* Most commands *)
let commands = [
  (* One-byte symbols *)
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
  ("\032", "SPACE");  (* Space ' ' *)
  ("\034", "QUOTE");  (* Double-quote *)
  ("\040", "LPAR");   (* ( *)
  ("\041", "RPAR");   (* ) *)
  ("\044", "COMMA");   (* , *)
  ("\046", "POINT");   (* . *)
  ("\048", "0");      (* Int: 48 = 0, 49 = 1, ... *)
  ("\058", "COLON");  (* : (equivalent to EOL) *)
  ("\060", "LESS");  (* < *)
  ("\061", "EQUAL");  (* = *)
  ("\062", "GREATER");  (* > *)
  ("\063", "QMARK");  (* ? (Question mark) *)
  ("\065", "A");      (* Letters: 65 = A, 66 = B, ... *)
  ("\091", "LSQBRACKET"); (* [ *)
  ("\093", "RSQBRACKET"); (* ] *)
  ("\123", "LBRACKET");   (* { *)
  ("\125", "RBRACKET");   (* } *)
  ("\128", "POLPAR");   (* Pol( (Optn > Angl) *)
  ("\129", "SIN");   (* sin *)
  ("\130", "COS");   (* cos *)
  ("\131", "TAN");   (* tan *)
  ("\133", "LN");   (* ln *)
  ("\134", "SQRT");   (* Square root sign *)
  ("\135", "UMINUS");   (* Unary minus sign *)
  ("\136", "THICKP");  (* Thick P (Optn > Prob) *)
  ("\137", "PLUS");   (* Plus sign *)
  ("\139", "POWER2");   (* ^2 *)
  ("\140", "COORDSIGN");   (* Small white square (Optn > Angl) *)
  ("\141", "INTEGRALPAR");   (* Integral sign + left parenthesis (Optn > Calc) *)
  ("\145", "ARCSIN");   (* sin^-1 *)
  ("\146", "ARCCOS");   (* cos^-1 *)
  ("\147", "ARCTAN");   (* tan^-1 *)
  ("\149", "LOG");   (* log *)
  ("\150", "CURT");   (* Cubic root *)
  ("\151", "ABS");   (* Absolute value (Optn > Cplx) *)
  ("\152", "THICKC");  (* Thick C (Optn > Prob) *)
  ("\153", "MINUS");   (* Minus sign *)
  ("\155", "POWERMINUS1");   (* -1 (^-1 sign) *)
  ("\156", "DEGREESIGN");   (* o Degree sign *)
  ("\160", "RECPAR");   (* Rec( (Optn > Angl) *)
  ("\161", "SINH");   (* sinh (Optn > Hyp) *)
  ("\162", "COSH");   (* cosh (Optn > Hyp) *)
  ("\163", "TANH");   (* tanh (Optn > Hyp) *)
  ("\165", "EPOWER");   (* e^ *)
  ("\166", "INT");   (* Int (Optn > Num) *)
  ("\168", "POWER");   (* ^ *)
  ("\169", "TIMES");  (* Multiplication sign *)
  ("\171", "EXCLAMATIONMARK");  (* ! (Optn > Prob) *)
  ("\172", "RADIANSIGN");   (* r Radian sign *)
  ("\177", "ARCSINH");  (* sinh^-1 (Optn > Hyp) *)
  ("\178", "ARCCOSH");  (* cosh^-1 (Optn > Hyp) *)
  ("\179", "ARCTANH");  (* tanh^-1 (Optn > Hyp) *)
  ("\181", "TENPOWER");   (* 10 (10^ sign) *)
  ("\182", "FRAC");   (* Frac (Optn > Num) *)
  ("\184", "NSQRT");   (* N-th square root sign *)
  ("\185", "DIVIDED");   (* Division sign *)
  ("\187", "FRACSIGN");   (* Fraction sign *)
  ("\188", "GRADSIGN");   (* g Grad sign *)
  ("\192", "ANS");  (* Ans *)
  ("\193", "RAN");  (* Ran# (Optn > Prob) *)
  ("\205", "SMALLR");  (* r (complex radius) *)
  ("\206", "THETA");  (* Theta (complex angle) *)
  ("\208", "PI");  (* Pi sign *)
  ("\209", "CLS");  (* Cls (Sketch) *)
  ("\211", "RND");   (* Rnd (Optn > Num) *)
  ("\217", "NORM"); (* Norm (Set up > Disp) *)
  ("\218", "DEG"); (* Deg (Set up > Angl) *)
  ("\219", "RAD"); (* Rad (Set up > Angl) *)
  ("\220", "GRA"); (* Gra (Set up > Angl) *)
  ("\221", "ENG"); (* Eng (Set up > Disp > Eng) *)
  ("\222", "INTG");   (* Intg (Optn > Num) *)
  ("\224", "PLOT"); (* Plot (Sketch > Plot) *)
  ("\225", "LINE"); (* Line (Sketch > Line) *)
  ("\226", "LBL"); (* Lbl *)
  ("\227", "FIX"); (* Fix (Set up > Disp) *)
  ("\228", "SCI"); (* Sci (Set up > Disp) *)
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
  ("\251", "PPAR"); (* P( (Optn > Prob) *)
  ("\252", "QPAR"); (* Q( (Optn > Prob) *)
  ("\253", "RPAR"); (* R( (Optn > Prob) *)
  ("\254", "tPAR"); (* t( (Optn > Prob) *)
  

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
  
  (* MATH *)
  ""
];;