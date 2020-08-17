( Defer a few things )
( ------------------ )

FORWARD ;
FORWARD (
FORWARD [CHAR]
FORWARD DOLIT

( Basic offsets )
( ------------- )

: CELL-BITS   CELL 8 * ;
: CELL+   CELL + ;
: CELL-   CELL - ;
: CELLS   CELL * ;
: CELL/   CELL / ;
: 1+   1 + ;
: 1-   1 - ;
: 2+   2 + ;
: 2-   2 - ;
: 2*   2 * ;
: 2/   2 / ;
: 4+   4 + ;
: 4-   4 - ;
: 4*   4 * ;
: 4/   4 / ;

( Relative Slots )
( -------------- )

4 CONSTANT SLOT
: SLOT+   SLOT + ;
: SLOT-   SLOT - ;
: SLOTS   SLOT * ;
: SLOT/   SLOT / ;

( Heap Handling )
( ------------- )

VARIABLE CP
: HERE ( -- a ) CP @ ;
: ALIGNED   3 + 3 INVERT AND ;
: ALIGN   HERE ALIGNED CP ! ;
: ALLOT ( n -- ) CP +! ;
: , ( n -- ) HERE ! CELL ALLOT ;
: C, ( n -- ) HERE C! 1 ALLOT ;
: L, ( n -- ) HERE L! SLOT ALLOT ;
: COMPILE, ( a -- ) HERE - SLOT/ L, ;

( Dictionary Fields )
( ----------------- )
( NAME, [COUNT, FLAGS, OFFSET, INLINE-SLOTS], LINK, ADVANCE, CODE, PARAMS )

: >COUNT ( a -- n ) SLOT- C@ ;
: >FLAGS ( a -- a ) SLOT- 1+ ;
: >FLAGS@ ( a -- n ) >FLAGS C@ ;
: >OFFSET ( a -- n ) SLOT- 2+ C@ ;
: >INLINE-SLOTS ( a -- a ) SLOT- 3 + ;
: >NAME ( a -- b u ) DUP >R 3 SLOTS - R> >COUNT DUP >R ALIGNED - R> ;
: >PARAMS ( xt -- a ) SLOT+ ;
: <COMPILER ( xt -- ) DUP >OFFSET SLOTS - SLOT- ;
: COMPILER> ( xt -- ) DUP >OFFSET SLOTS + SLOT+ ;

( Dictionary and Vocabularies )
( --------------------------- )

VARIABLE CONTEXT
VARIABLE FIRST
VARIABLE LAST


( FLAGS )
( ----- )

1 CONSTANT IMMEDIATE-FLAG
2 CONSTANT REDIRECT-FLAG
4 CONSTANT COMPILE-ONLY-FLAG
16 CONSTANT HIDDEN-LIST-FLAG
32 CONSTANT NEWLINE-FLAG
64 CONSTANT INDENT-AFTER-FLAG
128 CONSTANT UNINDENT-BEFORE-FLAG


( Compiler runs )
( ------------- )

: COMPILER ( -- ) R@ 2 SLOTS - COMPILER> COMPILE, ;
: ENSLOT ( n -- ) LAST >INLINE-SLOTS C! ;
: VARISLOT ( -- ) -1 ENSLOT ;
: +TAB ( -- ) LAST >FLAGS@ INDENT-AFTER-FLAG OR LAST >FLAGS L! ;
: -TAB ( -- ) LAST >FLAGS@ UNINDENT-BEFORE-FLAG OR LAST >FLAGS L! ;
: LIST-NEWLINE ( -- ) LAST >FLAGS@ NEWLINE-FLAG OR LAST >FLAGS L! ;
: HIDDEN-LIST ( -- ) LAST >FLAGS@ HIDDEN-LIST-FLAG OR LAST >FLAGS L! ;

( Control Flow )
( ------------ )

: <MARK ( -- a ) HERE ;
: <RESOLVE ( a -- ) COMPILE, ;
: >MARK ( -- A ) HERE 0 L, ;
: >RESOLVE ( A -- ) HERE OVER - SLOT/ SWAP L! ;

: FOR COMPILER ( -- a ) <MARK RUNSCODE> >R ; IMMEDIATE +TAB
: BEGIN COMPILER ( -- a ) <MARK RUNSCODE> NOP ; IMMEDIATE +TAB
: NEXT COMPILER ( a -- ) <RESOLVE RUNSCODE> DONEXT ; IMMEDIATE 1 ENSLOT -TAB
: UNTIL COMPILER ( a -- ) <RESOLVE RUNSCODE> 0BRANCH ; IMMEDIATE 1 ENSLOT -TAB
: AGAIN COMPILER ( a -- ) <RESOLVE RUNSCODE> BRANCH ; IMMEDIATE 1 ENSLOT -TAB
: IF COMPILER ( -- A ) >MARK RUNSCODE> 0BRANCH ; IMMEDIATE 1 ENSLOT +TAB
: AHEAD COMPILER ( -- A ) >MARK RUNSCODE> BRANCH ; IMMEDIATE 1 ENSLOT +TAB
: REPEAT COMPILER ( A a -- ) <RESOLVE >RESOLVE RUNSCODE> BRANCH ; IMMEDIATE 1 ENSLOT -TAB
: THEN COMPILER ( A -- ) >RESOLVE RUNSCODE> NOP ; IMMEDIATE -TAB
: AFT COMPILER ( a -- a A ) DROP >MARK <MARK SWAP RUNSCODE> BRANCH ; IMMEDIATE 1 ENSLOT +TAB
: ELSE COMPILER ( A -- A ) >MARK SWAP >RESOLVE RUNSCODE> BRANCH ; IMMEDIATE 1 ENSLOT -TAB +TAB
: WHEN COMPILER ( a A -- a A a ) >MARK OVER RUNSCODE> 0BRANCH ; IMMEDIATE 1 ENSLOT -TAB +TAB
: WHILE COMPILER ( a -- A a ) >MARK SWAP RUNSCODE> 0BRANCH ; IMMEDIATE 1 ENSLOT -TAB +TAB

( Safe Storing / Loading slots )
( ---------------------------- )

: SLOT@ ( a -- n ) DUP L@ DUP 0= IF 2DROP 0 EXIT THEN SLOTS + ;
: SLOT! ( n a -- ) OVER 0= IF L! EXIT THEN SWAP OVER - SLOT/ SWAP L! ;

( Common Functions )
( ---------------- )

: ?DUP ( w -- w w | 0 ) DUP IF DUP THEN ;
: DNEGATE ( d -- -d ) INVERT >R INVERT 1 UM+ R> + ;
: D+ ( d d -- d ) >R SWAP >R UM+ R> R> + + ;

( Basic Ops )
( ------------- )

: 2! ( d a -- ) SWAP OVER ! CELL+ ! ;
: 2@ ( a -- d ) DUP CELL+ @ SWAP @ ;
: COUNT ( b -- b +n ) DUP SLOT+ SWAP L@ ;
: WITHIN ( u ul uh -- t ) ( ul <= u < uh ) OVER - >R - R> U< ;
: 3DUP ( a b c -- a b c a b c ) >R 2DUP R> DUP >R -ROT R> ;

( Basic I/O )
( --------- )
32 CONSTANT BL
: KEY ( -- c ) BEGIN YIELD ?KEY ?DUP UNTIL ;
: PACE ( -- ) 11 EMIT ;
: SPACE ( -- ) BL EMIT ;
: CHARS ( +n c -- ) SWAP 0 MAX FOR AFT DUP EMIT THEN NEXT DROP ;
: SPACES ( +n -- ) BL CHARS ;
: TYPE ( b u -- ) FOR AFT DUP C@ EMIT 1 + THEN NEXT DROP ;
: CR ( -- ) 13 EMIT 10 EMIT ;

( Extra Math )
( ---------- )

: UM/MOD ( ud u -- ur uq )
   2DUP U<
   IF NEGATE CELL-BITS 1-
      FOR >R DUP UM+ >R >R DUP UM+ R> + DUP
         R> R@ SWAP >R UM+ R> OR
         IF >R DROP 1 + R> ELSE DROP THEN R>
      NEXT DROP SWAP EXIT
   THEN DROP 2DROP -1 DUP ;

: M/MOD ( d n -- r q ) ( floored division )
   DUP 0< DUP >R
   IF NEGATE >R DNEGATE R>
   THEN >R DUP 0< IF R@ + THEN R> UM/MOD R>
   IF SWAP NEGATE SWAP THEN ;

: UM* ( u u -- ud )
   0 SWAP ( u1 0 u2 ) CELL-BITS 1-
   FOR DUP UM+ >R >R DUP UM+ R> + R>
      IF >R OVER UM+ R> + THEN
   NEXT ROT DROP ;

: M* ( n n -- d ) 2DUP XOR 0< >R ABS SWAP ABS UM* R> IF DNEGATE THEN ;
: */MOD ( n n n -- r q ) >R M* R> M/MOD ;
: */ ( n n n -- q ) */MOD SWAP DROP ;

( Memory Alignment )
( ---------------- )

95 CONSTANT '_'
VARIABLE SP0
VARIABLE RP0
: >CHAR ( c -- c ) 127 AND DUP 127 BL WITHIN IF DROP '_' THEN ;
: >UPPER ( c -- c ) >CHAR '_' AND ;
: DEPTH ( -- n ) SP@ SP0 @ - CELL/ ;
: PICK ( +n -- w ) 1 + CELLS SP@ + @ ;

( String Handling )
( --------------- )

: NAME= ( b u b u -- f )
   >R SWAP DUP R> <> IF DROP 2DROP 0 EXIT THEN
   FOR AFT
      2DUP C@ >UPPER SWAP C@ >UPPER <> IF R> DROP 2DROP 0 EXIT THEN
      1+ SWAP 1+
   THEN NEXT
   2DROP -1
;

( Memory Access )
( ------------- )

: @EXECUTE ( a -- ) @ ?DUP IF EXECUTE THEN ;
: CMOVE ( b b u -- ) FOR AFT >R DUP C@ R@ C! 1 + R> 1 + THEN NEXT 2DROP ;
: CMOVE> ( b b u -- )
   DUP >R + SWAP R@ + SWAP R> FOR 1 - R> 1 - AFT >R DUP C@ R@ C! THEN NEXT 2DROP ;
: FILL ( b u c -- ) SWAP FOR SWAP AFT 2DUP C! 1 + THEN NEXT 2DROP ;
: -TRAILING ( b u -- b u )
   FOR AFT BL OVER R@ + C@ <
      IF R> 1 + EXIT THEN
   THEN NEXT 0 ;
: PACK$ ( b u a -- a ) ( null fill )
   ALIGNED DUP >R OVER
   DUP 0 2 UM/MOD DROP
   - OVER + 0 SWAP ! 2DUP C! 1 + SWAP CMOVE R> ;

( Numeric Output )
( -------------- )

VARIABLE BASE
VARIABLE HLD
: PAD ( -- a ) HERE 80 + ;
: DIGIT ( u -- c ) 9 OVER < 7 AND + 48 + ;
: EXTRACT ( n base -- n c ) 0 SWAP UM/MOD SWAP DIGIT ;
: <# ( -- ) PAD HLD ! ;
: HOLD ( c -- ) HLD @ 1 - DUP HLD ! C! ;
: # ( u -- u ) BASE @ EXTRACT HOLD ;
: #S ( u -- 0 ) BEGIN # DUP WHILE REPEAT ;
: SIGN ( n -- ) 0< IF 45 HOLD THEN ;
: #> ( w -- b u ) DROP HLD @ PAD OVER - ;
: str ( n -- b u ) DUP >R ABS <# #S R> SIGN #> ;
: HEX ( -- ) 16 BASE ! ;
: DECIMAL ( -- ) 10 BASE ! ;

: .R ( n +n -- ) >R str R> OVER - SPACES TYPE ;
: U.R ( u +n -- ) >R <# #S #> R> OVER - SPACES TYPE ;
: U. ( u -- ) <# #S #> TYPE SPACE ;
: . ( w -- ) BASE @ 10 XOR IF U. EXIT THEN str TYPE SPACE ;
: ? ( a -- ) @ . ;

( Numeric Output )
( -------------- )

: DIGIT? ( c base -- u t )
   >R 48 - 9 OVER < IF 7 - DUP 10 < OR THEN DUP R> U< ;

: NUMBER? ( a u -- number T | a u F )
   BASE @ >R
   2DUP >R >R 0 R> R> ( a n 0 b n )
   OVER C@ 36 =
   IF HEX SWAP 1 + SWAP 1 - THEN ( a n 0 b' n' )
   OVER C@ 45 = >R ( a n 0 b n )
   SWAP R@ - SWAP R@ + ( a n 0 b" n" )
   ?DUP
   IF
      1- ( a n 0 b n )
      FOR
         ( a n num b )
         DUP >R C@ BASE @ DIGIT?
      WHILE
         SWAP BASE @ * +
         R> 1+
      NEXT
         DROP ( b ) >R 2DROP ( a n ) R> R@ ( number ?sign )
         IF
            NEGATE
         THEN 1
      ELSE
         R> R> 2DROP ( b index ) 2DROP ( digit n ) 0
      THEN
   THEN
   R> DROP ( ?sign )
   R> BASE !
;

( Input Buffer )
( ------------ )

VARIABLE >IN
VARIABLE #TIB
128 ALLOT
: TIB ( -- a ) #TIB CELL+ ;

( Terminal )
( -------- )

: ^H ( b b b -- b b b ) ( backspace )
   >R OVER R> SWAP OVER XOR
   IF 1- 8 EMIT 32 EMIT 8 EMIT THEN ;
: TAP ( bot eot cur c -- bot eot cur )
   DUP EMIT OVER C! 1 + ;
: kTAP ( bot eot cur c -- bot eot cur )
   DUP 13 = OVER 10 = OR IF DROP SWAP DROP DUP EXIT THEN
   DUP 8 = SWAP 127 = OR IF ^H EXIT THEN
   BL TAP ;
: ACCEPT ( b u -- b u )
   OVER + OVER ( bot eot cur )
   BEGIN 2DUP XOR
   WHILE KEY DUP BL - 95 U<
     IF TAP ELSE kTAP THEN
   REPEAT DROP OVER - ;
VARIABLE SPAN
: EXPECT ( b u -- ) ACCEPT SPAN ! DROP ;
: QUERY ( -- ) TIB 80 ACCEPT #TIB ! DROP 0 >IN ! ;

( Exceptions )
( ---------- )

VARIABLE HANDLER
: CATCH ( ca -- err#/0 ) SP@ >R HANDLER @ >R RP@ HANDLER ! EXECUTE
                         R> HANDLER ! R> DROP 0 ;
: THROW ( err# -- err# ) HANDLER @ RP! R> HANDLER ! R> SWAP >R SP! DROP R> ;

( Parsing )
( ------- )

VARIABLE tmp
: _PARSE ( b u c -- b u delta ; <string> )
   tmp ! OVER >R DUP ( b u u )
   IF 1 - tmp @ BL =
      IF ( b u' \ 'skip' )
         FOR BL OVER C@ - 0< 0= WHILE 1 +
         NEXT ( b ) R> DROP 0 DUP EXIT ( all delim )
            THEN R>
      THEN OVER SWAP ( b' b' u' \ 'scan' )
      FOR tmp @ OVER C@ - tmp @ BL =
         IF 0< THEN WHILE 1 +
      NEXT DUP >R ELSE R> DROP DUP 1 + >R
                  THEN OVER - R> R> - EXIT
   THEN ( b u ) OVER R> - ;
: PARSE ( c -- b u ; <string> )
   >R TIB >IN @ + #TIB @ >IN @ - R> _PARSE >IN +! ;

: .( ( -- ) [CHAR] ) PARSE TYPE ; IMMEDIATE
: \ ( -- ) #TIB @ >IN ! ; IMMEDIATE
: CHAR ( -- c ) BL PARSE DROP C@ ;

: TOKEN ( -- b u ) BL PARSE ;

( Dictionary Build and Find )
( ------------------------- )
( NAME, LINK, ADVANCE, [count, flags, offset, enslot], CODE, PARAMS )

: >LINK& ( xt -- a ) 3 SLOTS - ;
: >LINK ( xt -- xt' ) >LINK& SLOT@ ;
: >LINK! ( xt' xt -- ) >LINK& SLOT! ;
: >ADVANCE& ( xt -- a ) 2 SLOTS - ;
: >ADVANCE ( xt -- xt' ) >ADVANCE& SLOT@ ;
: >ADVANCE! ( xt' xt -- ) >ADVANCE& SLOT! ;

: COMPILE ( -- ) R> DUP SLOT+ >R SLOT@ COMPILE, ;
: ['] ( -- ) R> DUP SLOT+ >R SLOT@ ;

: HEADER ( b u -- )
   ALIGN
   DUP >R HERE SWAP CMOVE R> DUP ALLOT ( name )
   ALIGN
   CONTEXT SLOT@ COMPILE, ( link )
   0 L, ( forward )
   L, ( count & flags )
   HERE LAST SLOT@ >ADVANCE!
   HERE LAST SLOT! ( Hook it in )
;

: .ID ( a -- ) >NAME TYPE SPACE ;

: RAW-FIND ( b u s-xt -- xt | b u 0 )
   BEGIN
     3DUP >NAME NAME= IF >R 2DROP R> EXIT THEN
     >LINK
     DUP 0=
   UNTIL
   DROP 0
;

: FIND ( b u -- xt | b u 0 ) CONTEXT SLOT@ RAW-FIND ;

: WORD-SPAN ( xt -- a n )
   DUP >NAME DROP SWAP >ADVANCE DUP IF
      >NAME DROP
   ELSE
      DROP HERE
   THEN
   OVER -
;

( Compilation Words )
( ----------------- )

: ' ( -- xt ) TOKEN FIND DUP IF EXIT THEN 100 THROW ;
: [COMPILE] ( -- ; <string> ) ' COMPILE, ; IMMEDIATE
: LITERAL ( w -- ) COMPILE DOLIT L, ; IMMEDIATE
: $, ( a n -- ) DUP L, FOR AFT DUP C@ C, 1+ THEN NEXT DROP ALIGN ;
: QUOTE$, ( -- ) [CHAR] " PARSE $, ;
: RECURSE ( -- ) ( LAST @ NAME> COMPILE, ) ; IMMEDIATE  ( TODO )

: SKIP$   R> R> DUP L@ ALIGNED + SLOT+ >R >R ;
: SKIP1   R> R> SLOT+ >R >R ;

: RUNS> COMPILER ( -- )
   HERE LAST SLOT@ - SLOT/ L, OP_DORUN L,
   RUNSCODE> EXIT ; IMMEDIATE 2 ENSLOT
: LISTS> COMPILER ( -- )
   OP_DOCOL L,
   RUNSCODE> EXIT ; IMMEDIATE 1 ENSLOT
: >LISTS ( xt -- xt )
   BEGIN
      SLOT+ DUP SLOT@ ['] ; =
      IF
         DROP 0 EXIT
      THEN
      DUP SLOT@ ['] LISTS> =
   UNTIL
   SLOT+
;

: DOLIT COMPILER ( -- )
   L,
   RUNSCODE> DOLIT
   LISTS> L@ . SKIP1 ; IMMEDIATE 1 ENSLOT HIDDEN-LIST
: [CHAR] COMPILER ( -- )
   CHAR L,
   RUNS> L@ SKIP1
   LISTS> L@ EMIT SPACE ; IMMEDIATE 1 ENSLOT
: LIST$   COUNT TYPE [CHAR] " EMIT SPACE ;
: $" COMPILER ( -- ; <string> )
   QUOTE$,
   RUNS> SKIP$
   LISTS> LIST$ ; IMMEDIATE VARISLOT
: ." COMPILER ( -- ; <string> )
   QUOTE$,
   RUNS> COUNT TYPE SKIP$
   LISTS> LIST$ ; IMMEDIATE VARISLOT
: ABORT" COMPILER ( -- ; <string> ) QUOTE$,
   RUNS> SKIP$ COUNT TYPE 1 THROW
   LISTS> LIST$ ; IMMEDIATE VARISLOT

: ( COMPILER ( -- )
   [CHAR] ) PARSE $,
   RUNS> DROP SKIP$
   LISTS> COUNT TYPE [CHAR] ) EMIT SPACE ; IMMEDIATE VARISLOT

( Interpreter )
( ----------- )

: $INTERPRET ( a u -- )
   FIND ?DUP IF
      DUP >FLAGS@ COMPILE-ONLY-FLAG AND IF ABORT" compile ONLY" THEN EXECUTE EXIT
   THEN
   NUMBER? IF EXIT THEN
   ."   Undefined word: " TYPE CR
   101 THROW
;

: $COMPILE ( a u -- )
   FIND ?DUP IF
      DUP >FLAGS@ IMMEDIATE-FLAG AND IF EXECUTE ELSE COMPILE, THEN EXIT
   THEN
   NUMBER? IF LITERAL EXIT THEN
   ."   Undefined word: " TYPE CR
   102 THROW
;

: PRESET ( -- ) SP0 @ SP! ;

VARIABLE 'EVAL
: .STACK   >R >R >R DUP . R> DUP . R> DUP . R> DUP . ;
: .OK   ['] $INTERPRET 'EVAL @ = IF CR .STACK ." ok> " ELSE ."  compiling" CR THEN ;

: ?STACK    DEPTH 0< IF ABORT" underflow" THEN ;

: [   ['] $INTERPRET 'EVAL ! ; IMMEDIATE
: ]   ['] $COMPILE 'EVAL ! ;
: IMMEDIATE    LAST SLOT@ >FLAGS DUP L@ IMMEDIATE-FLAG OR SWAP L! ;

: OVERT   LAST SLOT@ CONTEXT SLOT! ;

: EVAL
   BEGIN
      TOKEN DUP
   WHILE
      'EVAL @ EXECUTE ?STACK
   REPEAT
   2DROP
;

: QUIT
   RP0 @ RP!
   BEGIN
      [
      BEGIN
         .OK
         QUERY ['] EVAL CATCH
      UNTIL
      >IN @ #TIB @ <
      IF
         CR TIB #TIB @ TYPE
         CR >IN @ 94 CHARS
         CR ."  ?" CR
      THEN
      PRESET
   AGAIN
;

( Startup )
( ------- )

: BYE   CR 0 TERMINATE ;
: INIT-CORE
   CP !
   R> RP@ RP0 ! >R
   0 0 0 0 SP@ SP0 !
   DECIMAL
   ['] $INTERPRET 'EVAL !
;
: BOOT   INIT-CORE ." StenoForth 1.0" QUIT ;

( Utility Words )
( ------------- )

: NORMAL   -1 COLOR ;
: GOLD   214 COLOR ;
: BLUE   27 COLOR ;
: PAGE    12 EMIT ;
: CLS   PAGE ;
: CLEAR   PAGE ;

: SEE@ ( a -- )
   SLOT@
   DUP >FLAGS@ REDIRECT-FLAG AND
   IF
      <COMPILER
   THEN
;

: RAW-#SLOTS ( a -- n ) SEE@ >INLINE-SLOTS C@ ;
: VARSLOTS? ( a -- f ) RAW-#SLOTS 255 = ;
: #SLOTS ( a -- n )
   DUP L@ 0= IF
     DROP 0 EXIT
   THEN
   DUP VARSLOTS?
   IF
      SLOT+ L@ ALIGNED SLOT/ 1+
   ELSE
      RAW-#SLOTS
   THEN
;


( Traversal Words )
( --------------- )

: WORD> ( a -- a ) DUP #SLOTS SLOTS + SLOT+ ;

: SWEEP-ALL-WORDS
   ( xt -- )
   FIRST SLOT@
   BEGIN
      DUP >ADVANCE >R
      OVER >R
      SWAP EXECUTE
      R> R>
      DUP 0=
   UNTIL
   2DROP
;

VARIABLE LINK-ACTION

: DO-LINK LINK-ACTION @ EXECUTE ;

: SWEEP-WORD-REFS
   ( xt -- )
   DUP WORD-SPAN + >R
   >PARAMS
   BEGIN
      DUP R@ U<
   WHILE
      DUP WORD> >R DO-LINK R>
   REPEAT
   R> 2DROP
;

( Utility Words )
( ------------- )

: ALL-WORDS
  CR ['] .ID SWEEP-ALL-WORDS
;

: WORDS
   CR
   CONTEXT SLOT@
   BEGIN
      DUP .ID
      >LINK
      DUP 0=
   UNTIL DROP
;

: DUMP ( a n -- )
   CR
   OVER . ."  :: "
   FOR AFT
      DUP L@ . SLOT+
      DUP 8 SLOTS MOD 0=
      IF
         CR DUP . ."  :: "
      THEN
   THEN NEXT
   DROP
;

VARIABLE INDENT

3 CONSTANT TAB-SIZE
: +INDENT TAB-SIZE INDENT +! ;
: -INDENT TAB-SIZE NEGATE INDENT +! ;

: CR-INDENT CR INDENT @ SPACES ;

( SEE )
( --- )

: SEE-PROPERTIES ( xt -- )
   BLUE
   DUP >FLAGS@
   DUP IMMEDIATE-FLAG AND IF ." IMMEDIATE " THEN
   DUP UNINDENT-BEFORE-FLAG AND IF ." -TAB " THEN
   INDENT-AFTER-FLAG AND IF ." +TAB " THEN
   >INLINE-SLOTS C@
   DUP 255 =
   IF
      DROP ." VARISLOT" NORMAL EXIT
   THEN
   DUP 0=
   IF
      DROP NORMAL EXIT
   THEN
   . ."  ENSLOT"
   NORMAL
;

: SEE1
   DUP L@ 0= IF DROP EXIT THEN
   DUP SEE@
   DUP >FLAGS@ UNINDENT-BEFORE-FLAG AND
   IF
       -INDENT CR-INDENT
   ELSE
      DUP >FLAGS@ INDENT-AFTER-FLAG AND
      IF
         CR-INDENT
      THEN
   THEN
   DUP >FLAGS@
   IF
      BLUE
   THEN
   DUP >FLAGS@ HIDDEN-LIST-FLAG AND 0= IF
      DUP .ID
   THEN
   DUP >LISTS IF
      OVER SLOT+ OVER >LISTS EXECUTE
   THEN
   DUP >FLAGS@ INDENT-AFTER-FLAG AND
   IF
      +INDENT
      CR-INDENT
   ELSE
      DUP >FLAGS@ UNINDENT-BEFORE-FLAG NEWLINE-FLAG OR AND
      IF
         CR-INDENT
      THEN
   THEN
   2DROP
   NORMAL
;

: SEE.
   CR
   DUP L@ OP_DOVAR =
   IF
     BLUE ." VARIABLE " GOLD .ID NORMAL EXIT
   THEN
   DUP L@ OP_DOCON =
   IF
     DUP SLOT+ L@ . BLUE ." CONSTANT " GOLD .ID NORMAL EXIT
   THEN
   DUP L@ OP_DOCOL <>
   IF
     DUP L@ . BLUE ." OPCODE " GOLD DUP .ID SEE-PROPERTIES EXIT
   THEN
   BLUE ." : " GOLD DUP .ID NORMAL
   3 INDENT !
   CR-INDENT
   ['] SEE1 LINK-ACTION ! DUP SWEEP-WORD-REFS
   SEE-PROPERTIES
;

: SEE   ' SEE. ;

: CATALOG
   CONTEXT SLOT@
   BEGIN
      DUP SEE.
      >LINK
      DUP 0=
   UNTIL DROP
;


( Compilation Words )
( ----------------- )

: CREATE   TOKEN HEADER OP_DOVAR L, OVERT ;
: VARIABLE   TOKEN HEADER OP_DOVAR L, 0 L, 0 L, OVERT ;
: CONSTANT   TOKEN HEADER OP_DOCON L, L, 0 L, OVERT ;
: :   TOKEN HEADER OP_DOCOL L, ] ;
: ;   COMPILER OVERT [ RUNSCODE> EXIT ; IMMEDIATE -TAB


( Revision  Words )
( --------------- )

VARIABLE REPLACING
VARIABLE REPLACING-COMPILER
VARIABLE REPLACE-WITH
VARIABLE REPLACE-WITH-COMPILER
VARIABLE FOUNDATION
VARIABLE PEAK

: SLOT+! ( n a -- ) SWAP SLOT/ SWAP L+! ;

: ADJUSTMENT ( -- n ) FOUNDATION @ PEAK @ - ;

: ADJUST1
   ( a -- )
   DUP SLOT@ 0= IF
      DROP EXIT
   THEN
   DUP SLOT@ REPLACING @ = IF
      REPLACE-WITH @ OVER SLOT+!
   THEN
   DUP SLOT@ REPLACING-COMPILER @ = IF
      REPLACE-WITH-COMPILER @ OVER SLOT+!
   THEN
   DUP FOUNDATION @ U< IF
      DUP SLOT@ PEAK @ U< 0= IF
        ADJUSTMENT SWAP SLOT+! EXIT
      THEN
   THEN
   DUP PEAK @ U< 0= IF
      DUP SLOT@ FOUNDATION @ U< IF
        ADJUSTMENT NEGATE SWAP SLOT+! EXIT
      THEN
   THEN
   DROP
;

: FIND-OLD' ( -- xt )
   TOKEN CONTEXT SLOT@ >LINK RAW-FIND DUP IF EXIT THEN 104 THROW ;

: SETUP-REDEFINE
   ( check that last = context )
   LAST SLOT@ CONTEXT SLOT@ <> IF
      105 THROW
   THEN
   ( setup the things we're replacing )
   FIND-OLD' REPLACING !
   REPLACING @ COMPILER> REPLACING-COMPILER !
   ( setup the span of what's moved )
   REPLACING @ WORD-SPAN OVER + PEAK ! FOUNDATION !
   ( setup the adjustment to replace old with new )
   CONTEXT SLOT@ REPLACING @ - REPLACE-WITH !
   CONTEXT SLOT@ COMPILER> REPLACING @ COMPILER> - REPLACE-WITH-COMPILER !
;

: DISCONNECT-ADVANCE
   ( xt -- )
   DUP >ADVANCE REPLACING @ = IF
      DUP >ADVANCE >ADVANCE SWAP >ADVANCE!
   ELSE
      DROP
   THEN
;

: DISCONNECT-LINK
   ( xt -- )
   DUP >LINK REPLACING @ = IF
      LAST SLOT@ SWAP >LINK!
   ELSE
      DROP
   THEN
;

: ADJUST-LINK&ADVANCE
   ( xt -- )
   DUP >LINK& ADJUST1
   >ADVANCE& ADJUST1
;

: REPLACEMENT-RELINK
   ( word-after-replaced )
   REPLACING @ >LINK
   ( word-after-last )
   CONTEXT SLOT@ >LINK
   ( disconnect link )
   ['] DISCONNECT-LINK SWEEP-ALL-WORDS
   ( connect to prior word as last )
   CONTEXT SLOT!
   ( connect replacement word to word after replaced )
   LAST SLOT@ >LINK!
   ( make last match context )
   CONTEXT SLOT@ LAST SLOT!

   ( disconnect advance )
   ['] DISCONNECT-ADVANCE SWEEP-ALL-WORDS
   ( adjust advance and link )
   ['] ADJUST-LINK&ADVANCE SWEEP-ALL-WORDS
;

: ADJUST-COLON-WORD
   ( xt -- )
   DUP L@ OP_DOCOL =
   IF
      ['] ADJUST1 LINK-ACTION !
      SWEEP-WORD-REFS
   ELSE
      DROP
   THEN
;

: ADJUST-COLON-WORDS
   ['] ADJUST-COLON-WORD SWEEP-ALL-WORDS
;

: TRIM-OLD
   LAST ADJUST1
   CONTEXT ADJUST1
   PEAK @ FOUNDATION @ HERE PEAK @ - CMOVE
   ADJUSTMENT ALLOT
;

: REDEFINE
   SETUP-REDEFINE
   ADJUST-COLON-WORDS
   REPLACEMENT-RELINK
   TRIM-OLD
;

: junky1 emit emit ." junky" emit ;
: junky emit emit ." junky" emit ;
: foo ." hello" ;
: bar cr foo foo ;
: junk ." junk" cr ;
: baz ." there" cr ;
