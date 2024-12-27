; Netronic TinyBasic For The ELF II Recreation
; By Richard Peters, Richard11092000@cox.net
; Special THANKS to TOM PITTMAN for Writing Program,
; To LEE A. HART for Posting Necessary Parts
; And To Dave Ruske for creating COMACELF group
; Which Made This Recreation Possible
; Assembled With QELFEXE V2.0 Multiformat Assembler
; Current Code Running In Tinybasi.zip Emulator
; Requires Giant Board And ROM Monitor
; To Use LOAD And SAVE to Tape
; Designed to run In RAM Starting at 0000
; I Have Done What I Could To Make This Source
; Moveable And To Follow Itself, But I Still Could
; Could Have Missed Something. I Also Tried To Figure
; Out Some Of The IL Code and Add Comments To It.
; If There Is Something To Add Or Change, Let Me Know.
; Code Has Only Been Verified With What I Have
; Hope The Above Changes Soon.
; Last Update 01/29/2004 09:40PM
;
; INTERNAL MACRO DEFINITIONS
;
; CALL   = SEP R4 + DW   SUB LOCATION
; RETURN = SEP R5
; SEP R7 = SEP R7 + DB   LOW LOCATION OF BYTE
;

FETCH  MACRO address
        SEP R7
	db (address)&255
        ENDM
        
CALL    MACRO address
        SEP R4
	dw address
        ENDM

RETURN  MACRO
        SEP R5
        ENDM

SERIAL_B MACRO address
		B4 address
		ENDM

SERIAL_BN MACRO address
		BN4 address
		ENDM
		
KB_B MACRO address
		B3 address
		ENDM

KB_BN MACRO address
		BN3 address
		ENDM

KB_INP MACRO address
		INP   7
		ENDM

LDI0 MACRO
        GHI RD 
        ENDM


R0         EQU     0         ;REGISTER DEFINITION
R1         EQU     1         ;REGISTER DEFINITION
R2         EQU     2         ;REGISTER DEFINITION
R3         EQU     3         ;REGISTER DEFINITION
R4         EQU     4         ;REGISTER DEFINITION
R5         EQU     5         ;REGISTER DEFINITION
R6         EQU     6         ;REGISTER DEFINITION
R7         EQU     7         ;REGISTER DEFINITION
R8         EQU     8         ;REGISTER DEFINITION
R9         EQU     9         ;REGISTER DEFINITION
RA         EQU     10        ;REGISTER DEFINITION
RB         EQU     11        ;REGISTER DEFINITION
RC         EQU     12        ;REGISTER DEFINITION
RD         EQU     13        ;REGISTER DEFINITION
RE         EQU     14        ;REGISTER DEFINITION
RF         EQU     15        ;REGISTER DEFINITION
;
; The Following Register And EQU Assignments Are Not Used
; In Every Part Of Program
;
; REGISTER ASSIGNMENTS:
;
;      0 ; PC (VIA RESET) AT ENTRY
;      1 ; INTERRUPT PROGRAM COUNTER
;      2 ; STACK POINTER
;      3 ; NORMAL PROGRAM COUNTER
;      4 ; BASIC: SCRT "CALL" PC
;      5 ; BASIC: SCRT "RETURN" PC
;      6 ; BASIC: SCRT RETURN ADDR.
;      7 ; BASIC: PC FOR "FECH"
XX         EQU     8         ;BASIC: WORK REGISTER
PC         EQU     9         ;IL PROGRAM COUNTER
AC         EQU     10        ;BASIC: 16-BIT ACCUMULATOR
BP         EQU     11        ;BASIC POINTER
;     12                     SERIAL AND TAPE ROUTINES
PZ         EQU     13        ;BASE: PAGE 0 POINTER
;     14 ;      RE.0=BAUD RATE CONSTANT
;            IF RE.0=0 USES 1861 AND KEYBOARD P7,EF3
;               RE.1=  USED FOR INPUT,OUTPUT
X          EQU     15        ;BASIC: SCRATCH REGISTER
;
; DISPLAY BUFFER EQU
;
BUFF       EQU     0DB0h     ;ONLY CHANGE PAGE, UNLESS YOU
BUFE       EQU     BUFF+344  ;WANT TO CHANGE INTERUPT ROUTINE
BUFX       EQU     BUFE+56   ;ALSO LIMITED TO 1DB0 BY PLOT
;
MONITOR    EQU     0F000h    ;Monitor address
;Putting C8 in first byte allows Monitor To Run instead of Tiny
PAGE       LBR     COLDV
           LBR     MONITOR
           SEP     R0
           IDL
;
;  DATA AREA, COULD BE EQUATES
;
TVXY       DB      00Fh       ;DISPLAY CURSOR LOCATION
           DB      000h
           DB      000h       ;BIT LOCATION OF CURSOR
           DB      000h
MASK       DB      0E0h
TIME_      DB      09Ah
           DB      027h
           DB      03Ah
           DB      000h
           DB      000h
           DB      000h
BS         DB      008h
CAN        DB      01Bh
PAD        DB      000h
TAPEMODE   DB      000h
SPARE      DB      019h
XEQ        DB      019h
LEND       DB      034h
AEPTR      DB      080h
TTYCC      DB      000h
NXA        DW      00773h
AIL        DW      STRT       ;START OF IL
BASIC      DW      00F40h     ;LOWEST ADD. FOR PROGRAM
STACK      DW      03FF7h     ;HIGHEST ADD. FOR PROGRAM
MEND       DW      0109Bh     ;PROGRAM END + STACK RESERVE
TOPS       DW      03FF7h     ;TOP OF GOSUB STACK
LINO       DW      000AAh     ;CURRENT BASIC LINE NUMBER
WORK       DW      01083h
           DW      00034h
SP         DW      00033h
LINE       DW      00000h     ;INPUT LINE BUFFER
;
           ORG     080h
AESTK      DW      00000h     ;RANDOM NUMBER GEN.
           DW      00000h     ;VAR. A
           DW      00000h     ;VAR. B
           DW      00000h     ;VAR. C
           DW      00000h     ;VAR. D
           DW      00000h     ;VAR. E
           DW      00000h     ;VAR. F
           DW      00000h     ;VAR. G
           DW      00000h     ;VAR. H
           DW      00000h     ;VAR. I
           DW      00000h     ;VAR. J
           DW      00000h     ;VAR. K
           DW      00000h     ;VAR. L
           DW      00000h     ;VAR. M
           DW      00000h     ;VAR. N
           DW      00000h     ;VAR. O
           DW      00000h     ;VAR. P
           DW      00000h     ;VAR. Q
           DW      00000h     ;VAR. R
           DW      00000h     ;VAR. S
           DW      00000h     ;VAR. T
           DW      00000h     ;VAR. U
           DW      00000h     ;VAR. V
           DW      00000h     ;VAR. W
           DW      00000h     ;VAR. X
           DW      00000h     ;VAR. Y
           DW      00000h     ;VAR. Z
Z165       PLO     R7         ;I/O ROUTINES
           LBDF    PEND       ;GOTO WARM START
           GHI     RD
Z149       KB_B      Z148     ;CHECK FOR KEYBOARD OR SERIAL
           SERIAL_B      Z149 ;INPUT
Z150       KB_B      Z148
           SERIAL_BN    Z150  ;FINED TIMING OF SERIAL INPUT
           SEQ
Z153       PLO     RE
           LDI     8
Z151       SMI     1
           BNZ     Z151
           GLO     RE
           ADI     2
           BNQ     Z152
           SERIAL_B      Z153
           REQ
Z152       SERIAL_BN    Z153       ;MUST GOTO #C4
           NOP                     ;|
           NOP                     ;|
           SMI     1               ;|
           SERIAL_BN    Z154       ;|
           BNZ     Z152+1          ;BECAUSE OF THIS
           INC     RE
Z154       GLO     RE
           SMI     6
Z148       PHI     RE
           LDI     00Ch
           CALL    OUTPUTR    ;OUTPUT 0C  CLEARSCREEN
           LBR     CLEAR
BRKTST     ADI     0          ;BREAK TEST
           GHI     RE
           BNZ     Z156
           KB_BN     Z157
           LSKP
Z156       SERIAL_B      Z157
           SMI     0
SERIAL_DELAY       GHI     RE
           ANI     0FEh
Z158       PLO     RE
           LSZ
           DEC     RE
           GLO     RE
           BNZ     Z158
Z157       RETURN
COLDV      NOP                ;COLD START
           BR      COLD
           LBR     WARM       ;WARM START ENTRY
KEYV       LBR     INPUTR     ;BRANCH TO CHARATER INPUT
TYPEV      LBR     OUTPUTR    ;BRANCH TO CHARATER OUPUT
BREAKV     LBR     BRKTST     ;BRANCH TO BREAK TEST
; DEFAULTS LOADED TO DIRECT PAGE
           DB      008h       ;BACKSPACE CODE
           DB      01Bh       ;LINE CANCEL CODE
           DB      000h       ;PAD CHARATER
           DB      000h       ;TAPE MODE ENABLE FLAG 80=ENABLED
           DB      019h       ;SPARE STACK SIZE
ILPEEK     BR      PEEK       ;BRANCH TO PEEK
           DB      000h
           DB      000h
ILPOKE     DB      058h       ;POKE
           DB      0D5h
           DW      STRT       ;ADDRESS OF IL PROGRAM START
CONST      DW      00F40h     ;DEFAULT START OF PROGRAM SPACE
           DB      07Fh       ;END MEM STOP
           DB      000h
; END DEFAULTS
           LDA     R8         ;DOUBLE PEEK ENTRY
           SKP
PEEK       GHI     RD         ;PEEK ENTRY
           PHI     RA
           LDA     R8
           RETURN
ILINPOUT   LBR     IO
CALL_S     SEP     R3
CALL_      PHI     RF         ;CALL ROUTINE
           SEX     R2
           GLO     R6
           STXD
           GHI     R6
           STXD
           GLO     R3
           PLO     R6
           GHI     R3
           PHI     R6
           LDA     R6
           PHI     R3
           LDA     R6
           PLO     R3
           GHI     RF
           BR      CALL_S
RETURN_S   SEP     R3
RETURN_    PHI     RF         ;RETURN ROUTINE
           SEX     R2
           GHI     R6
           PHI     R3
           GLO     R6
           PLO     R3
           INC     R2
           LDA     R2
           PHI     R6
           LDN     R2
           PLO     R6
           GHI     RF
           BR      RETURN_S
           SEP     R3
FETCH_     LDA     R3         ;LOAD TEMP IMMEDIATE ROUTINE
           PLO     RD
           LDI     (PAGE)&255 ;MEMORY BASE PAGE
           PHI     RD
           LDA     RD
           SEX     RD         ;AND SET X TO D AND +
           BR      FETCH_-1
TABLE      DW      BACK
           DW      HOP
           DW      MATCH
           DW      TSTV
           DW      TSTN
           DW      TEND
           DW      RTN
           DW      HOOK
           DW      WARM
           DW      XINIT
           DW      CLEAR
           DW      INSRT
           DW      RETN
           DW      RETN
           DW      GETLN
           DW      RETN
           DW      RETN
           DW      STRNG
           DW      CRLF
           DW      TAB
           DW      PRS
           DW      PRN
           DW      LIST
           DW      RETN
           DW      NXT
           DW      CMPR
           DW      IDIV
           DW      IMUL
           DW      ISUB
           DW      IADD
           DW      INEG
           DW      XFER
           DW      RSTR
           DW      SAV
           DW      STORE
           DW      IND
           DW      RSBP
           DW      SVBP
           DW      RETN
           DW      RETN
           DW      BPOP
           DW      APOP
           DW      DUPS
           DW      LITN
           DW      LIT1
           DW      RETN
TBEND:
; COLD & WARM START INITIALIZATION ;
;
; COLD START;
;
COLD       LDI     ($+3)&255   ;CHANGE PROGRAM COUNTER
           PLO     R3        ;FROM R0 TO R3
           LDI     ($)>>8
           PHI     R3
           SEP     R3
; DETERMINE SIZE OF USER RAM
           PHI     AC        ;GET LOW END ADDR.
           LDI     (CONST)&255 ;OF USER PROGRAM
           PLO     AC        ;RAM (AT "CONST")
           LDA     AC
           PHI     R2        ;..AND PUT IN R2
           LDA     AC
           PLO     R2
           LDA     AC        ;SET PZ TO WRAP POINT
           PHI     PZ        ;(END OF SEARCH)
           LDI     0FFh
           PLO     PZ
           LDN     PZ        ;..AND SAVE BYTE
           PHI     X         ;NOW AT ADDR. PZ
SCAN       SEX     R2        ;REPEAT TO SEARCH RAM..
           INC     R2        ;- GET NEXT BYTE
           LDX
           PLO     X         ;- SAVE A COPY
           XRI     0FFh      ;- COMPLEMENT IT
           STR     R2        ;- STORE IT
           XOR               ;- SEE IF IT WORKED
           SEX     PZ
           LSNZ              ;- IF MATCHES, IS RAM
           GHI     X         ;SET CARRY IF AT
           XOR               ;WRAP POINT..
           ADI     0FFh      ;- ELSE IS NOT RAM
           GLO     X         ;RESTORE ORIGINAL BYTE
           STR     R2
           BNF     SCAN      ;- ..UNTIL END OR WRAP POINT
           DEC     R2
           LDN     AC        ;RAM SIZED: SET
           PHI     PZ        ;POINTER PZ TO
           LDI     STACK+1   ;WORK AREA
           PLO     PZ
           GLO     R2        ;STORE RAM END ADDRESS
           STXD
           GHI     R2
           STXD              ;GET & STORE RAM BEGINNIG
           DEC     AC        ;REPEAT TO COPY PARAMETERS..
           DEC     AC        ;- POINT TO NEXT
           LDN     AC        ;- GET PARAMETER
           STXD              ;- STORE IN WORK AREA
           GLO     PZ
           XRI     BS-1      ;- TEST FOR LAST PARAMETER
           BNZ     $-6       ;- ..UNTIL LAST COPIED
           SHR               ;SET DF=0 FOR "CLEAR"
           LSKP
;
; WARM START:
;
WARM       SMI     0         ;SET DF=1 FOR "DON'T CLEAR"
           LDI     ($+3)&255
           PLO     R3        ;BE SURE PROGRAM COUNTER IS R3
           LDI     ($)>>8
           PHI     R3
           SEP     R3
           PHI     R4        ;INITIALIZE R4, R5, R7
           PHI     R5        ;ASSUMES CALL,RETURN,FETCH
           PHI     R7        ;IS IN SAME PAGE AS WARM
           LDI     (CALL_)&255
           PLO     R4
           LDI     (RETURN_)&255
           PLO     R5
           LDI     (FETCH_)&255
           LBR     Z165       ;GOTO #00B6
CLEAR      FETCH   BASIC;- MARK PROGRAM EMPTY
           PHI     BP
           LDA     PZ
           PLO     BP
           LDI0               ;WITH LINE# = 0
           STR     BP
           INC     BP
           STR     BP
           FETCH   SPARE-1 ;SET MEND = START + SPARE
           GLO     BP         ;GET START
           ADD                ;ADD ;LOW BYTE OF SPARE
           PHI     X          ;SAVE TEMPORARILY
           FETCH   MEND       ;GET MEND
           GHI     X
           STXD               ;STORE LOW BYTE OF MEND
           GHI     BP
           ADCI    0          ;ADD CARRY
           STXD               ;STORE ;HIGH BYTE OF MEND
PEND       FETCH   STACK ;SET STACK TO END OF MEMORY
           PHI     R2
           LDA     PZ
           PLO     R2
           FETCH   TOPS
           GLO     R2        ;SET TOPS TO EMPTY
           STXD              ;(I.E. STACK END)
           GHI     R2
           STXD
           CALL    FORCE     ;SET TAPE MODE "OFF"
IIL        FETCH   AIL       ;SET IL PC
           PHI     PC
           LDA     PZ
           PLO     PC        ;CONTINUE INTO "NEXT"
;
; EXECUTE NEXT INTERMEDIATE LANGUAGE (IL) INSTRUCTION
;
NEXT       SEX     R2        ;GET OPCODE
           LDA     PC
           SMI     030h      ;IF JUMP OR BRANCH,
           BDF     TBR       ;GO HANDLE IT
           SDI     0D7h      ;IF STACK BYTE EXCHANGE,
           BDF     XCHG      ;GO HANDLE IT
           SHL               ;ELSE ;MULTIPLY BY 2
           ADI     (TBEND)&255  ;TO POINT INTO TABLE
           PLO     R6
           LDI     (NEXT)&255   ;& SET RETURN TO HERE
           DEC     R2           ;(DUMMY STACK ENTRY)
           DEC     R2
           STXD
           GHI     R3
           STXD
DOIT       GHI     R7        ;TABLE PAGE
           PHI     R6
           LDA     R6        ;FETCH SERVICE ADDRESS
           STR     R2
           LDA     R6
           PLO     R6
           LDX
           PHI     R6
           SEP     R5        ;GO DO IT
;
TBR        SMI     010h      ;IF JUMP OR CALL,
           BNF     TJMP      ;GO DO IT
           PLO     R6        ;ELSE BRANCH; SAVE OPCODE
           ANI     01Fh      ;COMPUTE DESTINATION
           BZ      TBERR     ;IF BRANCh, 0ADh DR = 0, GOTO ERROR
           STR     R2        ;PUSh, 0ADh DRESS ONTO STACK
           GLO     PC        ;ADD RELATIVE OFFSET
           ADD               ;LOW BYTE
           STXD
           GHI     PC        ;HIGH BYTE W. CARRY
           ADCI    0
           SKP
TBERR      STXD              ;STORE 0 FOR ERROR
           STXD
           GLO     R6        ;NOW COMPUTE SERVICE ADDRESS
           SHR               ;WHICH ;IS HIGH 3 BITS
           SHR
           SHR
           SHR
           ANI     0FEh
           ADI     (TABLE)&255 ;INDEX INTO TABLE
           PLO     R6
           BR      DOIT
;
TJMP       ADI     8         ;NOTE IF JUMP IN CARRY
           ANI     7         ;GET ADDRESS
           PHI     R6
           LDA     PC
           PLO     R6
           BDF     JMP       ;JUMP
           GLO     PC        ;PUSH PC
           STXD
           GHI     PC
           STXD
           CALL    STEST     ;CHECK STACK DEPTH
;
JMP        FETCH   AIL       ;ADD JUMP ADDRESS TO IL BASE
           GLO     R6
           ADD
           PLO     PC
           GHI     R6
           DEC     PZ
           ADC
           PHI     PC
           BR      NEXT
;
XCHG       SDI     7         ;SAVE OFFSET
           STR     R2
           FETCH   AEPTR
           PLO     PZ
           SEX     R2
           ADD
           PLO     R6        ;R6 IS OTHER POINTER
           GHI     PZ
           PHI     R6
           LDN     PZ        ;NOW SWAP THEM:
           STR     R2        ;SAVE OLD TOP
           LDN     R6        ;GET INNER BYTE
           STR     PZ        ;PUT ON TOP
           LDN     R2        ;GET OLD TOP
           STR     R6        ;PUT IN
           BR      NEXT
;
BACK       GLO     R6        ;REMOVE OFFSET
           SMI     020h      ;FOR BACKWARDS HOP
           PLO     R6
           GHI     R6
           SMBI    0
           SKP
;
HOP        GHI     R6        ;FORWARD HOP
           LBZ     ERR       ;IF ZERO, GOTO ERROR
           PHI     PC        ;ELSE PUT INTO PC
           GLO     R6
           PLO     PC
           BR      NEXT
;
           INC     BP        ;ADVANCE TO NEXT NON-BLANK CHAR.
NONBL      LDN     BP        ;GET CHARACTER
           SMI     020h      ;IF BLANK,
           BZ      NONBL-1   ;INCREMENT POINTER AND TRY AGAIN
           SMI     010h      ;IF NUMERIC (0-9),
           LSNF
           SDI     9         ;SET DF=1
NONBX      LDN     BP        ;GET CHARACTER
           RETURN              AND ;RETURN
;
STORE      CALL    APOP      ;GET VARIABLE
           LDA     PZ        ;GET POINTER
           PLO     PZ
           GHI     AC        ;STORE THE NUMBER
           STR     PZ
           INC     PZ
           GLO     AC
           STR     PZ
           BR      BPOP      ;GO POP POINTER
;
           CALL    APOP      ;POP 4 BYTES
APOP       CALL    BPOP      ;POP 2 BYTES
           PHI     AC        ;FIRST BYTE TO AC.1
BPOP       FETCH   AEPTR ;POP 1 BYTE
           DEC     PZ
           ADI     1         ;INCREMENT
           STR     PZ
           PLO     PZ
           DEC     PZ
           LDA     PZ        ;LEAVE IT IN D
           PLO     AC        ;AND AC.0
RETN       RETURN
;
TEND       CALL    NONBL     ;GET NEXT CHARACTER
           XRI     00Dh      ;IF CARRIAGE RETURN,
           BZ      NEXT      ;THEN FALL THRU IN IL
           BR      HOP       ;ELSE TAKE BRANCH
;
TSTV       CALL    NONBL     ;GET NEXT CHARACTER
           SMI     041h      ;IF LESS THAN 'A',
           BNF     HOP       ;THEN HOP
           SMI     01Ah      ;IF GREATER THAN 'Z'
           BDF     HOP       ;THEN HOP
           INC     BP        ;ELSE IS LETTER A-Z
           GHI     X         ;GET SAVED COPY
           SHL               ;CONVERT ;TO VARIABLE'S ADDRESS
           CALL    BPUSH     ;AND PUSH ONTO STACK
           BR      NEXT
;
TSTN       CALL    NONBL     ;GET NEXT CHARACTER
           BNF     HOP       ;IF NOT A DIGIT, HOP
           LDI0              ;ELSE COMPUTE NUMBER
           PHI     AC        ;INITIALLY 0
           PLO     AC
           CALL    APUSH     ;PUSH ONTO STACK
NUMB       LDA     BP        ;GET CHARACTER
           ANI     00Fh      ;CONVERT FROM ASCII TO NUMBER
           PLO     AC
           LDI0        
           PHI     AC
           LDI     10        ;ADD 10 TIMES THE..
           PLO     X
           SEX     PZ
NM10       INC     PZ
           GLO     AC        ;..PREVIOUS VALUE..
           ADD
           PLO     AC
           GHI     AC
           DEC     PZ        ;..WHICH IS ON STACK.
           ADC
           PHI     AC
           DEC     X         ;COUNT THE ITERATIONS
           GLO     X
           BNZ     NM10
           GHI     AC        ;SAVE NEW VALUE
           STR     PZ
           INC     PZ
           GLO     AC
           STXD
           CALL    NONBL     ;IF ANY MORE DIGITS,
           LBDF    NUMB      ;THEN DO IT AGAIN
NHOP       LBR     NEXT      ;UNTIL DONE
;
MATCH      GHI     BP        ;SAVE PB IN CASE NO MATCH
           PHI     AC
           GLO     BP
           PLO     AC
MAL        CALL    NONBL     ;GET A BYTE (IN CAPS)
;
           INC     BP        ;COMPARE THEM
           STR     R2
           LDA     PC
           XOR
           BZ      MAL       ;STILL EQUAL
           XRI     80H       ;END?
           BZ      NHOP      ;YES
           GHI     AC        ;NO GOOD
           PHI     BP        ;PUT POINTER BACK
           GLO     AC
           PLO     BP
JHOP       LBR     HOP       ;THEN TAKE BRANCH
;
STEST      FETCH   MEND ;POINT TO PROGRAM END
           GLO     R2        ;COMPARE TO STACK TOP
           SD
           DEC     PZ
           GHI     R2
           SDB
           BDF     ERR       ;AHA; OVERFLOW
           RETURN            ;ELSE ;EXIT
;
LIT1       LDA     PC        ;ONE BYTE
           BR      BPUSH
LITN       LDA     PC        ;TWO BYTES
           PHI     AC        ;FIRST IS HIGH BYTE,
           LDA     PC        ;THEN LOW BYTE
           BR      APUSH+1   ;PUSH RESULT ONTO STACK
;
HOOK       CALL    HOOP      ;GO DO IT, LEAVE EXIT HERE
           BR      APUSH+1   ;PUSH RESULT ONTO STACK
;
DUPS       CALL    APOP      ;POP 2 BYTES INTO AC
           CALL    APUSH     ;THEN PUSH TWICE
APUSH      GLO     AC        ;PUSH 2 BYTES
           CALL    BPUSH
           GHI     AC
BPUSH      STR     R2        ;PUSH ONE BYTE (IN D)
           FETCH   LEND ;CHECK FOR OVERFLOW
           SM                ;COMPARE ;AEPTR TO LEND
           BDF     ERR       ;OOPS!
           LDI     1
           SD
           STR     PZ
           PLO     PZ
           LDN     R2        ;GET SAVED BYTE
           STR     PZ        ;STORE INTO STACK
SEP5       RETURN            ;  & RETURN
;
IND        CALL    BPOP      ;GET POINTER
           PLO     PZ
           LDA     PZ        ;GET VARIABLE
           PHI     AC
           LDA     PZ
           BR      APUSH+1   ;GO PUSH IT
;
QUOTE      XRI     02Fh      ;TEST FOR QUOTE
           BZ      SEP5      ;IF QUOTE, GO EXIT
           XRI     022h      ;ELSE RESTORE CHARACTER
           CALL    TYPER
PRS        LDA     BP        ;GET NEXT BYTE
           XRI     00Dh      ;IF NOT CARRIAGE RETURN,
           BNZ     QUOTE     ;THEN CONTINUE
           DEC     PC        ;ELSE CONTINUE INTO ERROR
;
ERR        FETCH   XEQ       ;ERROR:
           PHI     XX        ;SAVE XEQ FLAG
           CALL    FORCE     ;TURN TAPE MODE OFF
           LDI     "!"       ;PRINT "!" ON NEW LINE
           CALL    TYPER
           FETCH   AIL
           GLO     PC        ;CONVERT IL PC TO ERROR#
           SM                ;BY ;SUBTRACTING
           PLO     AC        ;IL START FROM PC
           GHI     PC
           DEC     PZ        ;X MUST POINT TO
           SMB               ;PAGE0 ;REGISTER PZ=RD
           PHI     AC
           CALL    PRNA      ;PRINT ERROR#
           GHI     XX        ;GET XEQ FLAG
           BZ      BELL      ;IF XEQ SET,
           LDI     (ATMSG)&255 ;- THEN TYPE "AT"
           PLO     PC
           GHI     R3
           PHI     PC
           CALL    STRNG
           FETCH   LINO ;- GET LINE NUMBER
           PHI     AC        ;- AND PRINT IT, TOO
           LDA     PZ
           PLO     AC
           CALL    PRNA
BELL       LDI     7         ;RING THE BELL
           CALL    TYPEV
           CALL    CRLF      ;PRINT <CR><LF>
FIN        FETCH   TTYCC-1
           LDI0              ;TURN TAPE MODE OFF
           STR     PZ
EXIT       FETCH   TOPS ;RESET STACK POINTER
           PHI     R2
           LDA     PZ
           PLO     R2
           LBR     IIL       ;RESTART IL FROM BEGINNING
;
ATMSG      DB      ' ','A','T' ;ERROR MESSAGE TEMPLATE
           DB      ' ', 0A3H
;
TSTR       CALL    TYPER-2   ;PRINT CHARACTER STRING
STRNG      LDA     PC        ;GET NEXT CHARACTER OF STRING
           ADI     080h      ;IF HI BIT=0,
           BNF     TSTR      ;THEN GO PRINT & CONTINUE
           BR      TYPER-2   ;PRINT LAST CHAR AND EXIT
;
FORCE      FETCH   AEPTR-1
           LDI     AESTK     ;CLEAR A.E.STACK
           STXD
           LDI0              ;SET "NOT EXECUTING"
           STXD              ;LEND=0 ZERO LINE LENGTH
           STXD              ;XEQ=0 NOT EXECUTING
           LSKP              ;CONTINUE TO CRLF
;
CRLF       FETCH   TTYCC ;GET COLUMN COUNT
           SHL               ;IF IN TAPE MODE (MSB=1),
           BDF     SEP5      ;THEN JUST EXIT
           FETCH   PAD       ;GET # OF PAD CHARS
           PLO     AC        ;& SAVE IT
           LDI     00Dh      ;TYPE <CR>
PADS       CALL    TYPEV
           FETCH   TTYCC-1 ;POINT PZ TO COLUMN COUNTER
           GLO     AC        ;GET # OF PADS TO GO
           SHL               ;MSB SELECTS NULL OR DELETE
           BZ      PLF       ;UNTIL NO MORE PADS..
           DEC     AC        ;DECREMENT # OF PADS TO GO
           LDI0              ;PAD=NULL=0 IF MSB=0
           LSNF
           LDI     0FFh      ;PAD=DELETE=FFH IF MSB=1
           BR      PADS      ;..REPEAT
;
PLF        STXD              ;SET COLUMN COUNTER TTYCC=0
           LDI     08Ah      ;TYPE <LF>
;
           SMI     080h      ;FIX HI BIT
TYPER      PHI     X         ;SAVE CHAR
           FETCH   TTYCC ;CHECK OUTPUT MODE
           DEC     PZ
           ADI     081h      ;INCREMENT COLUMN COUNTER TTYCC
           ADI     080h      ;WITHOUT DISTURBING MSB
           BNF     SEP5      ;IF MSB=1, IN TAPE MODE, NOT PRINTIN
           STR     PZ        ;ELSE UPDATE COLUMN COUNTER
           GHI     X         ;GET CHAR
           LBR     TYPEV     ;AND GO TYPE IT
;
TAB        LDI     020h
           CALL    TYPER
           FETCH   TTYCC ;GET COLUMN COUNT
TABS       ANI     7          ;LOW 3 BITS
           BNZ     TAB
           RETURN
           CALL    TYPER
           DEC     AC        ;DECREMENT SPACES TO GO
           BR      TABS      ;...REPEAT
;
PRNA       CALL    APUSH     ;NUMBER IN AC
PRN        FETCH   AEPTR ;CHECK SIGN
           PLO     PZ
           CALL    DNEG      ;IF NEGATIVE,
           BNF     PRP
           LDI     '-'       ;PRINT '-'
           CALL    TYPER
PRP        LDI0              ;PUSH ZERO FLAG
           STXD              ;WHICH ;MARKS NUMBER END
           PHI     AC        ;PUSh, 010h  (=DIVISOR)
           LDI     10
           CALL    APUSH+1
           INC     PZ
PDVL       CALL    PDIV      ;DIVIDE BY 10
           GLO     AC        ;REMAINDER IS NEXT DIGIT
           SHR               ;BUT ;DOUBLED; HALVE IT
           ORI     030h      ;CONVERT TO ASCII
           STXD              ;PUSH ;IT
           INC     PZ        ;IS QUOTIENT=0?
           LDA     PZ
           SEX     PZ
           OR
           DEC     PZ        ;RESTORE POINTER
           DEC     PZ
           BNZ     PDVL      ;..REPEAT
PRNL       INC     R2        ;NOW, TO PRINT IT
           LDN     R2        ;GET CHAR
           LBZ     APOP-3    ;UNTIL ZERO (END FLAG)..
           CALL    TYPER     ;PRINT IT
           BR      PRNL      ;..REPEAT
;
RSBP       FETCH   SP        ;GET SP
           SKP
SVBP       GHI     BP        ;GET BP
           XRI     (LINE)>>8 ;IN THE LINE?
           BNZ     SWAP      ;NO, NOT IN SAME PAGE
           GLO     BP
           STR     R2
           LDX
           SMI     (AESTK)&255
           BDF     SWAP      ;NO, BEYOND ITS END
           FETCH   SP
           GLO     BP        ;YES, JUST COPY BP TO SP
           STXD
           GHI     BP
           STR     PZ
TYX        RETURN
;
SWAP       FETCH   SP        ;EXCHANGE BP AND SP
           PHI     XX        ;PUT SP IN TEMP
           LDN     PZ
           PLO     XX
           GLO     BP        ;STORE BP IN SP
           STXD
           GHI     BP
           STR     PZ
           GHI     XX        ;STORE TEMP IN BP
           PHI     BP
           GLO     XX
           PLO     BP
           RETURN
;
CMPR       CALL    APOP      ;GET FIRST NUMBER
           GHI     AC        ;PUSH ONTO STACK WITH BIAS
           XRI     080h      ;(FOR 2'S COMPLEMENT)
           STXD              ;(BACKWARDS)
           GLO     AC
           STXD
           CALL    BPOP      ;GET AND SAVE
           PLO     X         ;COMPARE BITS
           CALL    APOP      ;GET SECOND NUMBER
           INC     R2
           GLO     AC        ;COMARE THEM
           SM                ;LOW BYTE
           PLO     AC
           INC     R2
           GHI     AC        ;HIGH BYTE
           XRI     080h      ;BIAS: 0 TO 65535 INSTEAD
           SMB               ;OF -32768 TO +32767
           STR     R2
           BNF     CLT       ;LESS IF NO CARRY OUT
           GLO     AC
           OR
           BZ      CEQ       ;EQUAL IF BOTH BYTES 0
           GLO     X         ;ELSE GREATER
           SHR               ;MOVE PROPER BIT
           SKP
CEQ        GLO     X         ;(BIT 1)
           SHR
           SKP
CLT        GLO     X         ;(BIT 0)
           SHR               ;TO CARRY
           LSNF
           NOP
SKIP       INC     PC        ;SKIP ONE BYTE IF TRUE
           RETURN
;
ISUB       CALL    INEG      ;SUBTRACT IS ADD NEGATIVE
IADD       CALL    APOP      ;PUT ADDEND IN AC
           SEX     PZ
           INC     PZ        ;ADD TO AUGEND
           GLO     AC
           ADD
           STXD
           GHI     AC        ;CARRY INTO HIGH BYTE
           ADC
           STR     PZ
           RETURN
;
IMUL       CALL    APOP      ;MULTIPLIER IN AC
           LDI     010h      ;BIT COUNTER IN X
           PLO     X
           LDA     PZ        ;MULTIPLICAND IN XX
           PHI     XX
           LDN     PZ
           PLO     XX
MULL       LDN     PZ        ;SHIFT PRODUCT LEFT
           SHL               ;(ON STACK)
           STR     PZ
           DEC     PZ
           LDN     PZ
           SHLC              ;DISCARD HIGh, 016h  BITS
           STR     PZ
           CALL    SHAL      ;GET A BIT
           BNF     MULC      ;NOT THIS TIME
           SEX     PZ        ;IF MULTIPLIER BIT=1,
           INC     PZ
           GLO     XX        ;ADD MULTIPLICAND
           ADD
           STXD
           GHI     XX
           ADC
           STR     PZ
MULC       DEC     X         ;REPEAT 16 TIMES
           GLO     X
           INC     PZ
           BNZ     MULL
           RETURN
;
IDIV       CALL    APOP      ;GET DIVISOR
           GHI     AC
           STR     R2        ;CHECK FOR DIVIDE BY ZERO
           GLO     AC
           OR
           LBZ     ERR       ;IF YES, FORGET IT
           LDN     PZ        ;COMPARE SIGN OF DIVISOR
           XOR
           STXD              ;SAVE FOR LATER
           CALL    DNEG      ;MAKE DIVEDEND POSITIVE
           DEC     PZ        ;SAME FOR DIVISOR
           DEC     PZ
           CALL    DNEG
           INC     PZ
           LDI0        
           LSKP
PDIV       LDI0              ;MARK "NO SIGN CHANGE"
           STXD              ;FOR PRN ENTRY
           PLO     AC        ;CLEAR HIGH END
           PHI     AC        ;OF DIVIDEND IN AC
           LDI     17        ;COUNTER TO X
           PLO     X
DIVL       SEX     PZ        ;DO TRIAL SUBTRACT
           GLO     AC
           SM
           STR     R2        ;HOLD LOW BYTE FOR NOW
           DEC     PZ
           GHI     AC
           SMB
           BNF     $+5       ;IF NEGATIVE, CANCEL  IT
           PHI     AC        ;IF POSITIVE, MAKE IT REAL
           LDN     R2
           PLO     AC
           INC     PZ        ;SHIFT EVERYTHING LEFT
           INC     PZ
           INC     PZ
           LDX
           SHLC
           STXD
           LDX
           SHLC
           STXD
           GLO     AC        ;HIGh, 016h 
           SHLC
           CALL    SHCL
           DEC     X         ;DO IT 16 TIMES MORE
           GLO     X
           LBNZ    DIVL
           INC     R2        ;CHECK SIGN OF QUOTIENT
           LDN     R2
           SHL
           BNF     NEGX      ;POSITIVE IS DONE
INEG       FETCH   AEPTR ;POINT TO STACK
           PLO     PZ
           BR      NEG
DNEG       SEX     PZ
           LDX               ;FOR DIVIDE,
           SHL               ;TEST SIGN
           BNF     NEGX      ;IF POSITIVE, LEAVE IT ALONE
NEG        INC     PZ        ;IF NEGATIVE,
           LDI0              ;SUBTRACT IT FROM 0
           SM
           STXD
           LDI0        
           SMB
           STR     PZ
           SMI     0         ;AND SET CARRY=1
NEGX       RETURN
;
SHAL       GLO     AC        ;USED BY MULTIPLY
           SHL
SHCL       PLO     AC        ;AND DIVIDE
           GHI     AC
           SHLC
           PHI     AC
           RETURN
;
NXT        FETCH   XEQ       ;IF DIRECT EXECUTION
           LBZ     FIN       ;QUIT WITh, 0DFh =0
           LDA     BP        ;ELSE SCAN TO NEXT <CR>
           XRI     00Dh
           BNZ     $-3
           CALL    GLINO     ;GET LINE NUMBER
           BZ      BERR      ;ZERO IS ERROR
CONT       CALL    BREAKV    ;TEST FOR BREAK
           BDF     BREAK     ;IF BREAK,
           FETCH   NXA       ;RECOVER RESTART POINT
           PHI     PC        ;WHICH WAS SAVED BY INIT
           LDA     PZ
           PLO     PC
RUN        FETCH   XEQ-1 ;TURN OFF RUN MODE
           STR     PZ         ;(NON-ZERO)
           RETURN
;
BREAK      FETCH   AIL       ;SET BREAK ADDR=0
           PHI     PC        ;I.E. PC=IL START
           LDA     PZ
           PLO     PC
BERR       LBR     ERR
;
XINIT      FETCH   BASIC ;POINT TO START OF BASIC PROGRAM
           PHI     BP
           LDA     PZ
           PLO     BP
           CALL    GLINO     ;GET LINE NUMBER
           BZ      BERR      ;IF 0, IS ERROR (NO PROGRAM)
           FETCH   NXA       ;SAVE STATEMENT
           GLO     PC        ;ANALYZER ADDRESS
           STXD
           GHI     PC
           STR     PZ
           BR      RUN       ;GO START UP
;
XFER       CALL    FIND      ;GET THE LINE
           BZ      CONT      ;IF WE GOT IT, GO CONTINUE
GOAL       FETCH   LINO ;ELSE FAILED
           GLO     AC        ;MARK DESTINATION
           STXD
           GHI     AC
           STR     PZ
           BR      BERR      ;GO HANDLE ERROR
;
RSTR       CALL    TTOP      ;CHECK FOR UNDERFLOW
           LDA     R2        ;GET THE NUMBER
           PHI     AC        ;FROM STACK INTO AC
           LDN     R2
           PLO     AC
           FETCH   TOPS
           GLO     R2        ;RESET TOPS FROM R2
           STXD
           GHI     R2
           STXD
           CALL    FIND+3    ;POINT TO THIS LINE
           BNZ     GOAL      ;NOT THERE ANY MORE
           BR      BNEXT     ;OK
;
RTN        CALL    TTOP      ;CHECK FOR UNDERFLOW
           LDA     R2        ;(2 ALREADY INCLUDED)
           PHI     PC        ;PIP ADDRESS TO PC
           LDN     R2
           PLO     PC
BNEXT      LBR     NEXT
;
TTOP       FETCH   STACK ;GET TOP OF STACK
           INC     R2
           INC     R2
           GLO     R2        ;MATCH TO STACK POINTER
           ADI     2         ;(ADJUSTED FOR RETURN)
           XOR
           DEC     PZ
           BNZ     TTOK      ;NOT EQUAL
           GHI     R2
           ADCI    0
           XOR
           BZ      BERR      ;MATCH IS EMPTY STACK
;
TTOK       INC     R2        ;(ONCE HERE SAVES TWICE)
           RETURN
;
TAPE       FETCH   PAD+1 ;TURN OFF TYPEOUT
           SKP
NTAPE      LDI0              ;TURN ON TYPEOUT
           SHL               ;(FLAG TO CARRY)
           FETCH   TTYCC-1
           LDI0        
           SHRC              ;00 OR 80H
           STR     PZ
           BR      KLOOP
GETLN      LDI     (LINE)&255  ;POINT TO LINE
           PLO     BP
           CALL    APUSH     ;MARK STACK LIMIT
           GHI     PZ
           PHI     BP
KLOOP      CALL    KEYV      ;GET AN ECHOED BYTE
           ANI     7FH       ;SET HIGH BIT TO 0
           BZ      KLOOP     ;IGNORE NULL
           STR     R2
           XRI     07Fh
           BZ      KLOOP     ;IGNORE DELETE
           XRI     075h      ;IF <LF>,
           BZ      TAPE      ;THEN TURN TAPE MODE ON
           XRI     019h      ;IF <XOFF> (DC3=13H),
           BZ      NTAPE     ;THEN TURN TAPE MODE OFF
           FETCH   CAN-1
           LDN     R2
           XOR               ;IF CANCEL,
           BZ      CANCL     ;THEN GO TO CANCEL
           DEC     PZ
           LDN     R2
           XOR
           BNZ     STOK      ;NO
           DEC     BP        ;YES
           GLO     BP
           SMI     (LINE)&255  ;ANYTHING LEFT?
           BDF     KLOOP       ;YES
CANCL      LDI     (LINE)&255  ;IF NO, CANCEL THIS LINE
           PLO     BP
           LDI     00Dh      ;BY FORCING A <CR>
           SKP
STOK       LDN     R2        ;STORE CHARACTER IN LINE
           STR     BP
           FETCH   AEPTR-1
           GLO     BP        ;CHECK FOR OVERFLOW
           SM
           BNF     CHIN      ;OK
           LDI     7         ;IF NOT, RING BELL
           CALL    TYPER
           LDN     BP        ;NOW LOOK AT CHAR
           SKP
CHIN       LDA     BP        ;INCREMENT POINTER
           XRI     00Dh      ;IF NOT <CR>,
           BNZ     KLOOP     ;THEN GET ANOTHER
           CALL    CRLF      ;ELSE ECHO <LF>
           FETCH   LEND-1 ;AND MARK END
           GLO     BP
           STR     PZ
           LDI     (LINE)&255  ;RESET BP TO FRONT
           PLO     BP
           LBR     APOP      ;AND GO POP DUMMY
;
FIND       CALL    APOP      ;GET LINE NUMBER
           GLO     AC
           STR     R2        ;CHECK FOR ZERO
           GHI     AC
           OR
           LBZ     ERR       ;IF 0, GO TO ERROR
FINDX      FETCH   BASIC ;START AT FRONT
           PHI     BP
           LDA     PZ
           PLO     BP
FLINE      CALL    GLINO     ;GET LINE NUMBER
           LSNZ              ;NOT THER IF 0
           GLO     PZ        ;SET NON-ZERO,
FEND       RETURN            ;AND RETURN
           SEX     PZ
           GLO     AC        ;COMPARE THEM
           SD
           STR     R2        ;(SAVE LOW BYTE OF DIFFERENCE)
           GHI     AC
           DEC     PZ
           SDB
           SEX     R2
           OR                ;(D=0 IF EQUAL)
           BDF     FEND      ;LESS OR EQUAL IS END
           LDA     BP        ;NOT THERE YET
           XRI     00Dh      ;SCAN TO NEXT <CR>
           BNZ     $-3
           BR      FLINE
;
HOOP       CALL    HOOP+3    ;ADJUST STACK
           CALL    APOP      ;SET UP PARAMETERS:
           LDA     PZ        ;AC
           PHI     XX        ;MIDDLE ARGUMENT TO XX
           LDA     PZ
           PLO     XX
           LDA     PZ        ;SUBROUTINE ADDRESS BECOMES
           PHI     R6        ;"RETURN ADDRESS"
           LDA     PZ
           PLO     R6
           GLO     PZ        ;FIX STACK POINTER
           STR     R2
           FETCH   AEPTR-1
           LDN     R2        ;BY PUTTING CURRENT VALUE
           STR     PZ        ;VALUE BACK INTO IT
           PLO     PZ        ;LEAVE PZ AT STACK TOP
           GLO     AC        ;LEAVE AC.0 IN D
           RETURN              GO ;DO IT
;
LIST       FETCH   WORK+2
           GLO     BP        ;SAVE POINTERS
           STXD
           GHI     BP
           STR     PZ
           CALL    FIND      ;GET LIST LIMITS
           FETCH   WORK ;SAVE UPPER
           GLO     BP
           STXD
           GHI     BP
           STXD
           CALL    FIND      ;TWO ITEMS MARK BOUNDS
           DEC     BP        ;BACK UP OVER LINE#
           DEC     BP
LLINE      FETCH   WORK ;END?
           GLO     BP
           SM
           DEC     PZ
           GHI     BP
           SMB
           BDF     LIX       ;SO IF BP>BOUNDS,
           LDA     BP        ;GET LINE#
           PHI     AC
           LDA     BP
           PLO     AC
           BNZ     $+5
           GHI     AC
           BZ      LIX       ;QUIT IF ZERO (PROGRAM END)
           CALL    PRNA      ;ELSE PRINT LINE#
           LDI     02Dh      ;THEN A SPACE
LLOOP      XRI     00Dh      ;(RESTORE BITS FROM <CR> TEST)
           CALL    TYPER
           CALL    BREAKV    ;TEST FOR BREAK
           BDF     LIX       ;IF YES, THEN QUIT
           LDA     BP        ;NOW PRINT TEXT
           XRI     00Dh      ;UNTIL <CR>
           BNZ     LLOOP
           CALL    CRLF      ;END LINE WITH <CR><LF>
           BR      LLINE     ;..REPEAT UNTIL DONE
;
LIX        FETCH   WORK+2 ;RESTORE BP
           PHI     BP
           LDA     PZ
           PLO     BP
           RETURN
;
SAV        FETCH   TOPS ;ADJUST STACK TOP
           GLO     R2
           STXD
           GHI     R2
           STR     PZ
           FETCH   XEQ       ;IF NOT EXECUTING
           DEC     PZ
           LSZ               ;USE ZERO INSTEAD
           FETCH   LINO
           PLO     AC        ;HOLD HIGH BYTE
           LDA     PZ        ;GET LOW BYTE
           INC     R2
           INC     R2
           SEX     R2
           STXD              ;PUSH ONTO STACK
           GLO     AC        ;NOW THE HIGH BYTE
           STXD
           LBR     NEXT
;
GLINO      FETCH   LINO-1 ;SETUP POINTER
           LDA     BP        ;GET 1ST BYTE
           STR     PZ        ;STORE IN RAM
           INC     PZ
           LDA     BP        ;2ND BYTE
           STXD
           OR                ;D=0 IF LINE#=0
           INC     PZ
           RETURN
;
INSRT      CALL    SWAP      ;SAVE POINTER IN NEW LINE
           CALL    FIND      ;FIND INSERT POINT
           ADI     0FFh      ;IF DONE, SET DF
           LDI0        
           PLO     X         ;X IS SIZE DIFFERENCE
           BDF     NEW
           GHI     BP        ;SAVE INSERT POINT
           PHI     PZ
           GLO     BP
           PLO     PZ
           DEC     X         ;MEASURE OLD LINE LENGTH
           DEC     X         ;-3 FOR LINE# AND <CR>
           DEC     X         ;REPEAT..
           LDA     PZ        ;-1 FOR EACH BYTE OF TEXT
           XRI     00Dh      ;..UNTIL <CR>
           BNZ     $-4
NEW        DEC     BP        ;BACK OVER LINE#
           DEC     BP
           CALL    SWAP      ;TRADE LINE POINTERS
           FETCH   LINO
           LDN     BP
           XRI     00Dh      ;IF NEW LINE IS NULL,
           STXD
           STR     PZ
           BZ      HMUCH     ;THEN GO MARK IT
           GHI     AC        ;ELSE SAVE LINE NUMBER
           STR     PZ
           INC     PZ
           GLO     AC
           STR     PZ
           GHI     BP        ;MEASURE ITS LENGTH
           PHI     AC
           GLO     BP
           PLO     AC
           INC     X         ;LINE#
           INC     X         ;ENDING <CR>
           INC     X
           LDA     AC
           XRI     00Dh      ;AND ALL CHARS UNTIL FINAL <CR>
           BNZ     $-4
HMUCH      FETCH   SP        ;FIGURE AMOUNT OF MOVE
           PHI     AC
           LDA     PZ
           PLO     AC
           FETCH   MEND ;=DISTANCE FROM INSERT
           GLO     AC        ;TO END OF PROGRAM
           SM
           PLO     AC        ;LEAVE IT IN AC, NEGATIVE
           DEC     PZ
           GHI     AC
           SMB
           PHI     AC
           INC     PZ
           GLO     X         ;NOW COMPUTE NEW MEND,
           ADD               ;WHICH IS SUM OF OFFSET,
           PHI     X
           GLO     X
           ANI     080h      ;WITH SIGN EXTEND,
           LSZ
           LDI     0FFh
           DEC     PZ
           ADC               ;PLUS OLD MEND
           SEX     R2
           STXD              ;PUSH ONTO STACK
           PHI     XX
           GHI     X
           STXD              ;(BACKWARDS)
           STR     R2        ;CHECK FOR OVERFLOW
           GLO     R2
           SD
           GHI     XX
           STR     R2
           GHI     R2
           SDB
           LBDF    ERR-1     ;IF YES, THEN QUIT
           GLO     X         ;ELSE NO, PREPARE TO MOVE
           BZ      STUFF     ;NO MOVE NEEDED
           STR     R2
           SHL
           BNF     MORE      ;ADD SOME SPACE
           FETCH   SP        ;DELETE SOME
           PHI     X         ;X IS DESTINATION
           LDA     PZ
           PLO     X
           SEX     R2
           SM
           PLO     XX        ;XX IS SOURCE
           GHI     X
           ADCI    0
           PHI     XX
           LDA     XX        ;NOW MOVE IT
           STR     X
           INC     X
           INC     AC
           GHI     AC
           BNZ     $-5
           BR      STUFF
MORE       GHI     X         ;SET UP POINTERS
           PLO     X         ;X IS DESTINATION
           GHI     XX
           PHI     X
           FETCH   MEND
           PHI     XX
           LDA     PZ
           PLO     XX        ;XX IS SOURCE
           DEC     AC
           SEX     X         ;NOW MOVE IT
           LDN     XX
           DEC     XX
           STXD
           INC     AC
           GHI     AC
           BNZ     $-5
STUFF      FETCH   MEND ;UPDATE MEND
           INC     R2
           LDA     R2
           STXD
           LDN     R2
           STR     PZ
           FETCH   SP        ;POINT INTO PROGRAM
           PHI     AC
           LDA     PZ
           PLO     AC
           FETCH   LINO ;INSERT NEW LINE
           PLO     X
           OR                ;IF THERE IS ONE
           BZ      INSX      ;NO, EXIT
           GLO     X         ;ELSE INSERT LINE NUMBER
           STR     AC
           INC     AC
           LDA     PZ
           STR     AC
           INC     AC
           LDA     BP        ;NOW REST OF LINE
           STR     AC
           XRI     00Dh      ;TO <CR>
           BNZ     $-5
INSX       LBR     EXIT
IO         STXD              ;PUSH OUT BYTE
           STR     R2
           LDI0              ;CLEAR AC
           PHI     AC
           DEC     PZ
           LDA     R3        ;STORE RETURN IN RAM
           SEP     R5        ;(THIS IS NOT EXECUTED)
           STR     PZ
           DEC     PZ
           GLO     XX        ;MAKE IO INSTRUCTION
           ANI     00Fh
           ORI     060h
           STR     PZ
           ANI     8
           LSZ
           NOP               ;INPUT, SO
           INC     R2        ;DO INCREMENT NOW
           SEP     PZ        ;GO EXECUTE, RESULT IN D


;LOAD Tinybasic IL
           include "TB_IL.asm"
           DB 0,0

TVON       LDI     (INTERUPT)&255  ;SETUP INTERRUPT ROUTINE
           PLO     R1
           LDI     (INTERUPT)>>8
           PHI     R1
-          B1      -         ;LOOP UNTIL EF1 GOES FALSE
;                  (EF1 brackets the 1861 interrupt request)
           INP     1         ;TURN ON 1861
           SEX     R3
           RET               ;ENABLE INTERRUPTS
           RETURN
Z283       ORI     034h      ;MAKE FLG BRANCH
           PHI     RF        ;SAVE HIGH F
           DEC     RD
           SEX     RD        ;X=D
           GLO     RD        ;GLO D TO BRANCH TOO
           PLO     RF        ;SAVE IN LOW F
           LDI     0D5h       ;STORE RETURN
           STXD
           LDI     09Dh       ;LDI0      STORE CLEAR
           STXD
           GLO     RF        ;STORE BRANCH TOO ADDRESS
           STXD
           GHI     RF        ;STORE BRANCH INSTUCTION
           STR     RD
           LDI     1         ;LOAD 1
           SEP     RD        ;EXAMPLE  34XX  9D D5 BRANCH SKIPS C
ILFLG      BR      Z283
ILLOAD     LBR     Z284
ILSAVE     LDI     0F0h        ;SAVE TO TAPE
           PHI     RC
           LDI     065h
           PLO     RC
           LDI     080h
           PHI     RD
Z285       SMI     0
           SEP     RC         ;GOSUB #F065 ROM CALLS
           GHI     RD
           BNZ     Z285
Z287       SEQ
           LDA     RA
           PHI     RF
           LDI     9
           PLO     RF
           PLO     RD
           SHL
Z286       SEP     RC
           DEC     RF
           GHI     RF
           SHL
           PHI     RF
           GLO     RF
           BNZ     Z286
           GLO     RD
           SHR
           SEP     RC
           DEC     R8
           GHI     R8
           BNZ     Z287
           SEP     RC
           SEP     RC
           SEP     RC
           SEP     RC
           REQ
           RETURN
Z284       LDI     0F0h        ;LOAD FROM TAPE
           PHI     RC
           LDI     0BAh
           PLO     RC
Z288       LDI     0F9h
           PHI     RD
Z289       SEP     RC         ;GOSUB #F0BA ROM CALLS
           BNF     Z288
           GHI     RD
           BNZ     Z289
Z290       SEP     RC
           BDF     Z290
           LDI     1
           PHI     RD
           PLO     RD
Z291       SEP     RC
           GHI     RD
           SHLC
           PHI     RD
           BNF     Z291
           SEP     RC
           GLO     RD
           SHR
           BDF     Z292
           GHI     RD
           STR     RA
           SEX     RA
           OUT     4
           ADI     0FFh
           GLO     R8
           SHLC
           PLO     R8
           ANI     003h
           BNZ     Z290
           INC     R9
           GLO     RA
Z292       RETURN

INPUTR     GHI     RE         ;INPUT FROM KEYBOARD ROUTINE
           BZ      KEYIN
Z293       SERIAL_B      Z293       ;SERIAL INPUT ROUTINE
           SHR
           CALL    Z158
           SERIAL_B      INPUTR
           LDI     07Fh
Z298       PLO     RF
           GHI     RE
           SHR
           SERIAL_B      Z294
           BNF     Z295
           SEQ
           SKP
Z294       REQ
Z295       CALL    SERIAL_DELAY
           GLO     RF
           SHR
           PLO     RF
           SERIAL_BN    Z297
           ORI     080h
Z297       BDF     Z298
;           GHI RE
;           SHR
;           SERIAL_B  +       ; Short branch on EF4=1
;           SEQ            ; Set Q=1
;           SKP            ; Skip next byte
;+		   REQ            ; Reset Q=0
           REQ
           CALL   SERIAL_DELAY    ; Set P=R4 as program counter
           GLO RF         ; Get low register RF
           RETURN

OUTPUTR    PLO     RC         ;OUTPUT TO SCREEN ROUTINE
           PHI     RC
           GHI     RE
           BNZ     Z299
           GLO     RC
           BR      DISP
Z299       LDI     00Ah        ;SERIAL OUTPUT ROUTINE
           PLO     RF
           CALL    SERIAL_DELAY
           ADI     0
Z300       CALL    SERIAL_DELAY
           LSNF
           REQ
           SKP
           SEQ
           GHI     RC
           SMI     0
           SHRC
           PHI     RC
           DEC     RF
           GLO     RF
           BNZ     Z300
           GLO     RC
           RETURN
BLINK      FETCH   TIME_+2;   LOOK AT TIMER
           SHL
           SHL
           SHL
           BNZ     Z301
           SHLC               ;EVERY HALF SECOND
           CALL    TVD        ;TOGGLE CURSOR
KEYIN      CALL    TVON       ;TURN DISPLAY ON
Z301       KB_BN     BLINK      ;WAIT FOR KEYIN
           GHI     RD
           CALL    TVD        ;TURN CURSOR OFF
           KB_INP             ;GET KEYIN
DISP       CALL    TVD        ;DISPLAY CHAR
           PLO     RE
           XRI     00Ah       ;IF <LF>
           BZ      HOLD       ;YES
           SERIAL_B  HOLD     ;ALSO HOLD ON EF4
           GLO     RE
           RETURN
HOLD       CALL    TVON       ;TURN DISPLAY ON
TVOFF      LDI     00Ch       ;TV OFF AND DELAY
           PHI     RF
-          DEC     RF
           GHI     RF
           BNZ     -
-          SERIAL_B      -     ;THEN WAIT FOR /4
           SEX     R3
           OUT     1           ;TURN DISPLAY OFF
           IDL
           GLO     RE
           DIS
           RETURN
;
;       (ORG in last 40 bytes of page)
;
; Character Formatter - ASCII character in ACC.
;
TVD        ANI     07Fh       ;SET HIGH BIT TO 0
           PLO     RE         ;SAVE FOR EXIT
           SMI     060h       ;CHECK FOR UPPER CASE
           GLO     RE
           BNF     Z304       ;IF NOT JUMP
           SMI     020h       ;CONVERT TO UPPERCASE
           PLO     RE
Z304       SEX     R2
           GLO     RA         ;SAVE RA ON STACK
           STXD
           GHI     RA
           STXD
           GLO     R9         ;SAVE R9 ON STACK
           STXD
           GHI     R9
           STXD
           GLO     R8         ;SAVE R8 ON STACK
           STXD
           GHI     R8
           STXD
           LDI     (SHFT)&255       ;SET UP SHIFT PC
           PLO     RA
           LDI     (SHFT)>>8
           PHI     RA
           FETCH   TVXY            ;TVXY GET POINTER R8 = *0008-9
           PHI     R8         ;WHICH IS CURSOR
           LDA     RD
           PLO     R8
           LDA     RD         ;D = *000A AND BIT POINTER
           ANI     7          ;ONLY WONT LOW 3 BITS
           PHI     R9
           FETCH   BS         ;BS IS THIS CANCEL
           GLO     RE
           XOR                ;AT BS+1=CANCEL
           BZ      DOTON
           GLO     RE
           SMI     07Fh       ;IGNORE <DEL>=7F
           BZ      EXIT2
           ADI     05Fh
           BDF     CHAR       ;IS IT PRINTABLE
           ADI     013h
           BZ      ODBYTE     ;IS IT <CR>
           ADI     1
           BZ      OCBYTE     ;IS IT CLEARSCREEN
           ADI     2
           BZ      OABYTE     ;IS IF <LF>
           ADI     9
           BZ      DOTON      ;1 = TURN DOT ON
           BNF     DOTOFF     ;0 = TURN DOT OFF
EXIT2      FETCH   01Ah       ;01Ah RD = #001B
           GHI     R9
           ANI     7
           PHI     R9
           ADI     0FEh
           GLO     R8
           SHLC
           XOR
           ANI     7
           XOR
           STR     RD
           FETCH   TVXY+1     ;TVXY+1 RD = #000A
           GHI     R9
           STXD               ;STORE BIT POINTER
           GLO     R8
           STXD               ;STORE DISPLAY LOCATION
           GHI     R8
           STXD
           INC     R2         ;RESTORE R8
           LDA     R2
           PHI     R8
           LDA     R2
           PLO     R8
           LDA     R2         ;RESTORE R9
           PHI     R9
           LDA     R2
           PLO     R9
           LDA     R2         ;RESTORE RA
           PHI     RA
           LDN     R2
           PLO     RA
           GLO     RE         ;GET SAVED CHARACTER
           RETURN
DOTOFF     LDI     080h       ;POINT TO BIT
           SEP     RA
           XRI     0FFh       ;MAKE AND MASK
           AND
           BR      DOTON+4
DOTON      LDI     080h
           SEP     RA
           OR
           STR     R8
           BR      EXIT2
OABYTE     GLO     R8         ;0A ROUTINE   DOWN LINE
           ADI     030h
           PLO     R8
           GHI     R8
           ADCI    0
           PHI     R8
           BR      Z306
OCBYTE     LDI     (BUFX-1)&255  ;0C ROUTINE   CLEAR SCREEN
           PLO     R8
           LDI     (BUFX-1)>>8
           PHI     R8
           SEX     R8         ;REPEAT...
CLRS       GHI     RD         ;CLEAR BYTE
           STXD               ;DECREMENT POINTER
           GLO     R8
           SMI     (BUFF)&255 ;HAS POINTER REACHED
           GHI     R8         ;START OF BUFFER
           SMBI    (BUFF)>>8
           BDF     CLRS       ;...UNTIL DONE
           IRX                ;CONTINUE TO <CR>
ODBYTE     GHI     RD         ;0D ROUTINE   CARRAGE RETURN
           PHI     R9         ;LEFT END OF LINE
           GLO     R8
           ANI     0F8h       ;OF BYTE * 8
           PLO     R8
Z306       GLO     R8         ;CHEAK FOR BOTTOM OF SCREEN
           SMI     (BUFE)&255
           GHI     R8
           SMBI    (BUFE)>>8
           BNF     EXIT2
           LDI     (BUFF)&255
           PLO     RF
           GLO     R8
           ADI     (BUFF)&255
           ANI     0F8h
           PLO     RA
           LDI     (BUFF)>>8
           PHI     RF
           ADCI    0
           PHI     RA
Z308       LDA     RA         ;SCROLL SCREEN
Z309       STR     RF
           INC     RF
           GLO     RF
           SMI     (BUFE)&255
           GHI     RF
           SMBI    (BUFE)>>8
           BNF     Z308
           GLO     RF
           SMI     (BUFX)&255
           GHI     RD
           BNF     Z309
           GLO     R8
           ANI     7
           PLO     R8
           LDI     (BUFE-8)>>8
           PHI     R8
           BR      EXIT2
CHAR       GLO     RE         ;20 BYTE - 5A BYTE ROUTINE
           SHL                ;INDEX INTO CHARACTER TABLE
           ADI     (CTBL-64)&255
           PLO     RF
           GHI     RD
           ADCI    (CTBL-64)>>8
           PHI     RF
           FETCH   MASK-1
           LDA     RF         ;GET BIT MASK
           STR     RD         ;SAVE IT
           LDA     RF
           ADI     (DOTS)&255
           PLO     RF         ;POINT TO DOT MATRIX
           GHI     RD
           ADCI    (DOTS)>>8
           PHI     RF
           GHI     R9         ;SAVE CURSOR POSITION
           STR     R2
           DEC     R2
           LDN     RD         ;POSITION BIT MASK
           ANI     7
           PHI     R9
           LDN     RD
           ANI     0F8h       ;IT IS LEFT 5 BITS
           SEP     RA
           INC     RA         ;CANCEL 2ND CO-CALL
           INC     RA
           PLO     R9         ;SAVE MASK
           INC     R2
           SEX     R2         ;PREPARE TO FIND RELATIVE SHIFT
           LDN     RD
           ANI     7
           SD                 ;(X=2)
           PHI     R9
           GLO     R9         ;SAVE NEW MASK
           STR     RD
CHRL       LDA     RF         ;GET SOME DOTS
           SEX     RD
           AND                ;MASK INTO THIS CHARACTER
           SEP     RA         ;SHIFT IT
           OR                 ;INSERT INTO BUFFER
           STR     R8
           SEP     RA         ;UP TO NEXT LINE
           BDF     CHRL
           LDN     RD         ;CHECK FOR SPLIT WORD
           SEP     RA
           INC     RA         ;DON'T WANT COUNTER
           INC     RA
           BNF     Z310       ;NOT SPLIT
           GLO     RF         ;BACK UP DOT POINTER
           SMI     6
           PLO     RF
           INC     R8         ;POINT TO NEXT WORD
           GLO     R8
           ANI     7
           BNZ     Z311
           GHI     R9         ;OOPS,NEXT LINE
           ANI     087h
           ORI     050h
           PHI     R9
           DEC     RA
           DEC     RA
           SEP     RA
Z311       GHI     R9         ;CONVERT TO LEFT SHIFT
           ORI     0F8h
           PHI     R9
           BR     CHRL       ;GO DO IT AGAIN
Z310       SEX     R2
           ORI     080h
           SKP                ;FIND RIGHT EDGE
Z312       INC     R9
           SHR
           BNF     Z312       ;OF MASK
           GLO     R9
           SDI     9          ;ANY LEFT?
           PHI     R9
           SMI     8
           BNF     CSTK       ;YES
           PHI     R9         ;NO, ADVANCE WORD
           INC     R8
           GLO     R8
           ANI     7
           BNZ     CSTK
           LDI     050h
           PHI     R9
           DEC     RA
           DEC     RA
           SEP     RA
CSTK       NOP
           LBR     Z306
Z324       SEP     R3         ;< EXIT
SHFT       STR     R2         ;>ENTER SAVE BITS TO SHIFT
           GLO     R8         ;NOTE IF ADDRESS IS OUTSIDE DISPLAY
           SMI     (BUFF)&255
           GHI     R8         ;ONLY WORRY ABOUT TOO LOW
           SMBI    (BUFF)>>8
           GHI     RD         ;IF SO, RETURN 0
           BNF     Z314       ;SO PROGRAM ISN'T DESTROYED
           GHI     R9         ;LOOK AT COUNTER
           ANI     087h       ;MASK OUT WORD COUNTER
           PLO     R9
           BZ      Z315       ;NO SHIFT
           SHL
           BDF     Z316       ;LEFT
Z317       LDN     R2         ;SHIFT RIGHT ONCE
           SHR
           STR     R2
           DEC     R9
           GLO     R9
           BNZ     Z317       ;REPEAT N TIMES
           BR      Z318       ;DONE
Z316       SHR                ;LEFT SHIFT
           SDI     008h       ;SET UP COUNTER
           PLO     R9
Z319       LDN     R2         ;DO IT
           SHL
           STR     R2
           DEC     R9
           GLO     R9
           BNZ     Z319       ;REPEAT
Z315       SHL                ;CLEAR CARRY
Z318       LDN     R2         ;GET BITS
Z314       SEX     R8
           SEP     R3         ;EXIT, C=SHIFT OFF RIGHT
           BR      Z320       ;SECOND ENTRY POINT
           BR      SHFT       ;OPTIONAL RERUNS
;
; Count words, moving pointer up
;
Z320       GHI     R9
           ADI     018h       ;CONVERT IF NEGATIVE, ADD 1
           BNF     Z321
           ORI     080h       ;(RESTORE SIGN)
Z321       ANI     0F7h
           PHI     R9
           SHL                ;SIGN IN C
           ANI     0E0h       ;IS THIS END?
           XRI     0C0h
           BNZ     Z322       ;NO
           GHI     R9         ;YES, RESTORE ORIGINAL
           ANI     7
           BNF     Z323
           ORI     0F8h       ;(NEGATIVE)
Z323       PHI     R9
           GLO     R8         ;BUMP R8 BACK TO BOTTOM
           ADI     028h
           PLO     R8
           GHI     R8
           ADCI    0
           PHI     R8
           BR      Z324       ;C=0
Z322       GLO     R8         ;GO TO NEXT LINE UP
           SMI     8
           PLO     R8
           GHI     R8
           SMBI    0
           PHI     R8
           BR      Z324       ;C=1
;
; Interrupt service routine for 1861
;
Z327       LDI     3
           PLO     R0
           SEX     R2
           LDA     R2
           SHL
           LDA     R2         ;RECOVER D
           RET                ;< EXIT
INTERUPT   NOP                ;> ENTRY DISPLAY INT. ROUTINE
           DEC     R2
           SAV                ;SAVE T
           DEC     R2
           STXD               ;SAVE D
           LDI     (BUFF)>>8
           PHI     R0         ;SET UP R0 FOR DMA
           LDI     (BUFF)&255
           PLO     R0
-          B1      -          ;SYNCHRONIZE
Z326       GLO     R0
           DEC     R0
           PLO     R0
           SEX     R0         ;NOT A NOP
           DEC     R0
           PLO     R0         ;THREE LINES PER PIXEL
           GHI     R0         ;LAST LINE
           XRI     (BUFE)>>8  ;IS NEW PAGE
           BNZ     Z326
           PHI     R0
           LDI     (TIME_+2)&255 ;NOW UPDATE CLOCK
           PLO     R0
           SHRC               ;SAVE CARRY
           STR     R2
           LDX
           ADI     1          ;INCREMENT FRAME COUNT
           STR     R0
           SMI     03Dh       ;ONE SECOND
           BNF     Z327       ;NOT YET
           STXD               ;IF YES,
           LDX                ;BUMP SECONDS
           ADI     1
           STR     R0
           BR      Z327
CTBL       DW      08608h     ;SP MASK BYTE AND DATA POINTER
           DW      0820Ah     ;! MASK BYTE AND DATA POINTER
           DW      0E508h     ;" MASK BYTE AND DATA POINTER
           DW      0F823h     ;# MASK BYTE AND DATA POINTER
           DW      0E435h     ;$ MASK BYTE AND DATA POINTER
           DW      0E55Ah     ;% MASK BYTE AND DATA POINTER
           DW      0F423h     ;& MASK BYTE AND DATA POINTER
           DW      0C200h     ;' MASK BYTE AND DATA POINTER
           DW      0C111h     ;( MASK BYTE AND DATA POINTER
           DW      0C211h     ;) MASK BYTE AND DATA POINTER
           DW      0E03Ch     ;* MASK BYTE AND DATA POINTER
           DW      0E547h     ;+ MASK BYTE AND DATA POINTER
           DW      0C307h     ;, MASK BYTE AND DATA POINTER
           DW      0C441h     ;- MASK BYTE AND DATA POINTER
           DW      08407h     ;. MASK BYTE AND DATA POINTER
           DW      0E529h     ;/ MASK BYTE AND DATA POINTER
           DW      0E111h     ;0 MASK BYTE AND DATA POINTER
           DW      0E243h     ;1 MASK BYTE AND DATA POINTER
           DW      0E44Fh     ;2 MASK BYTE AND DATA POINTER
           DW      0E103h     ;3 MASK BYTE AND DATA POINTER
           DW      0E01Dh     ;4 MASK BYTE AND DATA POINTER
           DW      0E042h     ;5 MASK BYTE AND DATA POINTER
           DW      0E249h     ;6 MASK BYTE AND DATA POINTER
           DW      0E073h     ;7 MASK BYTE AND DATA POINTER
           DW      0E303h     ;8 MASK BYTE AND DATA POINTER
           DW      0E049h     ;9 MASK BYTE AND DATA POINTER
           DW      08304h     ;: MASK BYTE AND DATA POINTER
           DW      0C335h     ;; MASK BYTE AND DATA POINTER
           DW      0E541h     ;< MASK BYTE AND DATA POINTER
           DW      0E503h     ;= MASK BYTE AND DATA POINTER
           DW      0E517h     ;> MASK BYTE AND DATA POINTER
           DW      0E05Ah     ;? MASK BYTE AND DATA POINTER
           DW      0E079h     ;@ MASK BYTE AND DATA POINTER
           DW      0E12Fh     ;A MASK BYTE AND DATA POINTER
           DW      0E56Dh     ;B MASK BYTE AND DATA POINTER
           DW      0E417h     ;C MASK BYTE AND DATA POINTER
           DW      0E217h     ;D MASK BYTE AND DATA POINTER
           DW      0E560h     ;E MASK BYTE AND DATA POINTER
           DW      0E534h     ;F MASK BYTE AND DATA POINTER
           DW      0E24Fh     ;G MASK BYTE AND DATA POINTER
           DW      0E33Bh     ;H MASK BYTE AND DATA POINTER
           DW      08217h     ;I MASK BYTE AND DATA POINTER
           DW      0E155h     ;J MASK BYTE AND DATA POINTER
           DW      0E060h     ;K MASK BYTE AND DATA POINTER
           DW      0E343h     ;L MASK BYTE AND DATA POINTER
           DW      0F96Dh     ;M MASK BYTE AND DATA POINTER
           DW      0F41Dh     ;N MASK BYTE AND DATA POINTER
           DW      0E017h     ;O MASK BYTE AND DATA POINTER
           DW      0E573h     ;P MASK BYTE AND DATA POINTER
           DW      0E00Bh     ;Q MASK BYTE AND DATA POINTER
           DW      0E53Bh     ;R MASK BYTE AND DATA POINTER
           DW      0E029h     ;S MASK BYTE AND DATA POINTER
           DW      0E066h     ;T MASK BYTE AND DATA POINTER
           DW      0E21Dh     ;U MASK BYTE AND DATA POINTER
           DW      0E379h     ;V MASK BYTE AND DATA POINTER
           DW      0FB2Eh     ;W MASK BYTE AND DATA POINTER
           DW      0E260h     ;X MASK BYTE AND DATA POINTER
           DW      0E273h     ;Y MASK BYTE AND DATA POINTER
           DW      0E035h     ;Z MASK BYTE AND DATA POINTER
           DW      0C017h     ;[ MASK BYTE AND DATA POINTER
           DW      0E329h     ;\ MASK BYTE AND DATA POINTER
           DW      0C117h     ;] MASK BYTE AND DATA POINTER
           DW      0E200h     ;^ MASK BYTE AND DATA POINTER
           DW      0E507h     ;_ MASK BYTE AND DATA POINTER
DOTS       DW      00000h     ;START OF DOT TABLE
           DW      08080h
           DW      0E897h
           DW      0A897h
           DW      06840h
           DW      04020h
           DW      040ADh
           DW      0B6ADh
           DW      04404h
           DW      02056h
           DW      0DD57h
           DW      02000h
           DW      0F4AAh
           DW      0A9AAh
           DW      0F400h
           DW      039E9h
           DW      0ABADh
           DW      02900h
           DW      055FAh
           DW      054F8h
           DW      05400h
           DW      0C024h
           DW      04A91h
           DW      0600Ah
           DW      05575h
           DW      05151h
           DW      02014h
           DW      0EC86h
           DW      04C27h
           DW      0E400h
           DW      01515h
           DW      0BE55h
           DW      0B600h
           DW      001C2h
           DW      03CD2h
           DW      091F0h
           DW      01002h
           DW      0572Ah
           DW      070A0h
           DW      05800h
           DW      0DE68h
           DW      0A462h
           DW      01C06h
           DW      02355h
           DW      01518h
           DW      01051h
           DW      00422h
           DW      0A144h
           DW      000AFh
           DW      0ACD6h
           DW      0ACAFh
           DW      00042h
           DW      0425Fh
           DW      052F9h
           DW      01000h
           DW      04645h
           DW      0566Dh
           DW      04600h
           DW      09494h
           DW      0562Dh
           DW      0EE00h
           DW      06894h
           DW      0B4B4h
           DW      05400h
           SEP     RF
