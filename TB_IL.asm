;        IF TESTING_IL
;BUFF       EQU     0DB0h     ;ONLY CHANGE PAGE, UNLESS YOU
;TVXY       EQU     8
;TYPEV      EQU     0109h
;ILPOKE     EQU     0118h
;ILINPOUT   EQU     0126h
;KEYV       EQU     0106h
;ILSAVE     EQU     09FDh
;ILLOAD     EQU     09FAh
;MEND       EQU     024h
;BASIC      EQU     020h
;ILFLG      EQU     9F8h
;ILPEEK     EQU     114h
;        ENDIF

IL_SX   MACRO stack             ;Duplicate Top number (two bytes) on Stack
        DB 00h + (stack & 07h)
    ENDM

IL_NO   MACRO                   ;Duplicate Top number (two bytes) on Stack
        DB 08h
    ENDM
IL_LB   MACRO val
        DB 09H, val & 0FFh      ;Push Literal byte onto stack
    ENDM

IL_LN   MACRO val               ;Push Literal number
        DB 0Ah
        DW val & 0FFFFh
    ENDM

IL_DS   MACRO                   ;Duplicate Top number (two bytes) on Stack
        DB 0Bh
    ENDM

IL_SP   MACRO                   ;Stack Pop
        DB 0Ch
    ENDM

IL_SB   MACRO                   ;Save basic pointer
        DB 10h
    ENDM

IL_RB   MACRO                   ;Restore basic pointer
        DB 11h
    ENDM

IL_FV   MACRO                   ;Fetch Variable
        DB 12h
    ENDM

IL_SV   MACRO                   ;Store Variable
        DB 13H
    ENDM

IL_GS   MACRO                   ;GOSUB Save
        DB 14H
    ENDM

IL_RS   MACRO                   ;Restore Saved Line
        DB 15H
    ENDM

IL_GO   MACRO                   ;GOTO
        DB 16H
    ENDM

IL_NE   MACRO                   ;Negate(Two's Complement)
        DB 17H
    ENDM

IL_AD   MACRO                   ;Add
        DB 18H
    ENDM

IL_SU   MACRO                   ;Subtract
        DB 19H
    ENDM

IL_MP   MACRO                   ;Multiply
        DB 1AH
    ENDM

IL_DV   MACRO                   ;Divide
        DB 1BH
    ENDM

IL_CP   MACRO                   ;Compare
        DB 1CH
    ENDM

IL_NX   MACRO                   ;Next BASIC Statement
        DB 1DH
    ENDM

IL_LS   MACRO                   ;List the program
        DB 1FH
    ENDM

IL_PN   MACRO                   ;Print Number
        DB 20H
    ENDM

IL_PQ   MACRO                   ;Print String
        DB 21H
    ENDM

IL_PT   MACRO                   ;Print Tab
        DB 22H
    ENDM

IL_NL   MACRO                   ;New Line
        DB 23H
    ENDM

IL_PC   MACRO char, endchar     ;Print literal string  
        DB 24h
        IF char>''
        DB char
        ENDIF
        IF endchar>''
        DB endchar | 80h
        ENDIF             
    ENDM

IL_GL   MACRO                   ;Get Input Line.
        DB 27h
    ENDM

IL_IL   MACRO                   ;Insert Basic line  
        DB 2Ah
    ENDM

IL_MT   MACRO                   ;Mark the basic program space empty
        DB 2Bh
    ENDM

IL_XQ   MACRO                   ;Execute
        DB 2Ch
    ENDM

IL_WS   MACRO                   ;Stop
        DB 2Dh
    ENDM

IL_US   MACRO                   ;Machine Language Subroutine
        DB 2Eh
    ENDM

IL_RT   MACRO                   ;IL Subroutine Return
        DB 2Fh
    ENDM

IL_JS   MACRO addr              ;IL Subroutine Call
        DW 3000H | addr-STRT & 7FFh
    ENDM

IL_J    MACRO addr              ;Jump
        DW 3800H | addr-STRT & 7FFh
    ENDM

IL_BR_B MACRO  addr             ;Relative Branch Back
        DB 60h - (1+$-addr)
    ENDM

IL_BR_F MACRO  addr             ;Relative Branch Forward
        DB 60h + (addr-$-1)
    ENDM

IL_BC MACRO  addr, char, endchar;String Match Branch
        DB 80h + ((addr-$-1)&31)
        IF char > ''
        DB char
        ENDIF
        DB endchar | 80h               
    ENDM

IL_BV MACRO  addr               ;Branch if not a Variable
        DB 0A0h + ((addr-$-1)&31)
    ENDM

IL_BN   MACRO  addr             ;Branch if not a number
        DB 0C0h + ((addr-$-1)&31)
    ENDM

IL_BE   MACRO  addr             ;Branch if not end of line
        DB 0E0h + ((addr-$-1)&31)
    ENDM

IL_ENDCHAR  MACRO char
        DB char | 80h
    ENDM

IL_CHAR  MACRO char, endchar
        IF char>0
        DB char
        ENDIF
        DB endchar | 80h
    ENDM

STRT    IL_PC ':', 11h          ;':Q^'  Start Of IL Program
        IL_GL                   ;GL
        IL_SB                   ;SB
        IL_BE LO                ;BE      :LO
        IL_BR_B STRT            ;BR      :STRT
LO      IL_BN STMT              ;BN      :STMT
        IL_IL                   ;IL
        IL_BR_B STRT            ;BR      :STRT
XEC     IL_SB                   ;SB
        IL_RB                   ;RB
        IL_XQ                   ;XQ
STMT    IL_BC GOTO, "LE", 'T'   ;BC      :GOTO     'LET'
        IL_BV $+1               ;BV      * !18
        IL_BC $+1, 0, '='       ;BC      * !20     '='         
LET     IL_JS EXPR              ;JS      :EXPR
        IL_BE $+1               ;BE      * !23
        IL_SV                   ;SV
        IL_NX                   ;NX
GOTO    IL_BC PRNT, 'G','O'     ;BC      :PRNT     'GO'      
        IL_BC GOSB, 'T','O'     ;BC      :GOSB     'TO'
        IL_JS EXPR              ;JS      :EXPR
        IL_BE $+1               ;BE      * !34
        IL_SB                   ;SB
        IL_RB                   ;RB
        IL_GO                   ;GO
GOSB    IL_BC $+1, "SU", 'B'    ;BC      * !39     'SUB'
        IL_JS EXPR              ;JS      :EXPR
        IL_BE $+1               ;BE      * !44
        IL_GS                   ;GS
        IL_GO                   ;GO
PRNT    IL_BC SKIP_, 'P', 'R'   ;BC      :SKIP     'PR'
        IL_BC P0, "IN", 'T'     ;BC      :P0       'INT'
P0      IL_BE P3                ;BE      :P3
        IL_BR_F S3              ;BR      Z233
P1      IL_BC S1, 0, ';'        ;BC      Z234      ';'
P2      IL_BE P3                ;BE      :P3
        IL_NX                   ;NX
P3      IL_BC S4, 0, '"'        ;BC      Z235      '"'
        IL_PQ                   ;PQ
        IL_BR_B P1              ;BR      :P1
SKIP_   IL_BR_B IF_             ;BR      :IF
S1      IL_BC S2, 0, ','        ;Z234       BC      Z236      ','
        IL_PT                   ;PT
        IL_BR_B P2              ;BR      :P2
S2      IL_BC S3, 0, ':'        ;Z236       BC      Z233      ':'
        IL_PC 0, 13h            ;PC                'S^'
S3      IL_BE $+1               ;Z233       BE      * !73
        IL_NL                   ;NL
        IL_NX                   ;NX
S4      IL_JS EXPR
        IL_PN                   ;PN
        IL_BR_B P1
IF_     IL_BC INPT, 'I', 'F'    ;BC      :INPT     'IF'
        IL_JS EXPR
        IL_JS RELOP             ;JS      RELOP
        IL_JS EXPR
        IL_BC I1, "THE", 'N'    ;BC      :I1       'THEN' 
I1      IL_CP                   ;CP
        IL_NX                   ;NX
        IL_J STMT
INPT    IL_BC RETN_, "INPU", 'T';BC      :RETN     'INPUT'
I0      IL_BV $+1               ;Z242       BV      * !104
        IL_SB                   ;SB
        IL_BE I3                ;BE      Z238
I2      IL_PC "? ", 11h         ;Z239       PC                '? Q^'
        IL_GL                   ;GL
        IL_BE I3                ;BE      Z238
        IL_BR_B I2              ;BR      Z239
I3      IL_BC I4, 0, ","        ;Z238       BC      Z240      ','
I4      IL_JS EXPR
        IL_SV                   ;SV
        IL_RB                   ;RB
        IL_BC I5, 0, ','        ;BC      Z241      ','
        IL_BR_B I0              ;BR      Z242
I5      IL_BE $+1               ;Z241      BE      * !123
        IL_NX                   ;NX
RETN_   IL_BC END, 'RETUR', 'N' ;BC      :END      'RETURN'
        IL_BE $+1               ;BE      * !132
        IL_RS                   ;RS
        IL_NX                   ;NX
END     IL_BC LIST_, "EN", 'D'  ;BC      :LIST     'END'
        IL_BE $+1               ;BE      * !139
        IL_WS                   ;WS
LIST_   IL_BC RUN_, "LIS", 'T'  ;BC      :RUN      'LIST'
        IL_BE L1                ;BE      Z243
        IL_LN 0001h             ;LN      #0001
        IL_LN 7FFFh             ;LN      #7FFF
        IL_BR_F L2              ;BR      Z244
L1      IL_JS EXPR
        IL_JS PE4               ;JS      Z245
L2      IL_BE $+1               ;BE      * !158
        IL_PC 0,0               ;Z244       PC                '@^@^@^@^J^@^'
        DB 0,0,0,0,0Ah,80h      ;--
        IL_LS                   ;LS
        IL_NX                   ;NX
RUN_    IL_BC CLER, "RU",'N'    ;BC      :CLER     'RUN'
        IL_J XEC
CLER    IL_BC PLOT, "CLEA",'R'  ;BC      :PLOT     'CLEAR'
        IL_MT                   ;MT
PLOT    IL_BC PO1, "PLO",'T'    ;BC      Z246      'PLOT'
        IL_JS EXPR
        IL_BC PO2, 0, ','       ;BC      Z247      ','
        IL_DS                   ;DS
        IL_DS                   ;DS
        IL_LN 42                ;LN      42
        IL_JS RELOP1            ;JS      Z248      COMPARE >0 AND <42
        IL_LN 64                ;LN      64        MULTIPLY BY 64
        IL_MP                   ;MP
        IL_LN 64                ;LN      64
        IL_JS EXPRJS            ;JS      EXPRJS    GET NEXT EXPR AND COMP >0 AND <64
        IL_AD                   ;AD                ADD TOGETHER (X*64+Y)
        IL_BR_F PO3             ;BR      Z250      SKIP JUMPS
PO1     IL_J POKE
PO2     IL_J PO4                ;Z247       J       Z251
PO3     IL_LN BUFF<<3           ;Z250       LN      BUFF<<3         BUFF*8
        IL_AD                   ;AD                ADD (BUFF*8)+(X*64+Y)
        IL_DS                   ;DS
        IL_LN 8                 ;LN      8
        IL_DV                   ;DV                ((BUFF*8)+(X*64+Y))/8
        IL_DS                   ;DS
        IL_SX 4                 ;SX 4
        IL_SX 2                 ;SX 2
        IL_SX 5                 ;SX 5
        IL_SX 3                 ;SX 3
        IL_SX 5                 ;SX 5
        IL_LN 8                 ;LN      8
        IL_MP                   ;MP                 MULTIPLY BY 8
        IL_SU                   ;SU                 GET REMANDER
        IL_LB TVXY+2            ;LB      TVXY+2
        IL_SX 2                 ;SX 2
        IL_SV                   ;SV                 STORE NEW BIT POINTER
        IL_LB TVXY              ;LB      TVXY
        IL_SX 2                 ;SX 2
        IL_SX 1                 ;SX 1
        IL_SV                   ;SV                 STORE NEW CURSOR
        IL_BC PO5, '', ','      ;BC      Z252      ','
        IL_JS EXPR
PO4     IL_BE $+1               ;Z251       BE      * !245
        IL_LN TYPEV             ;LN      TYPEV
        IL_SX 2                 ;SX 2
        IL_SX 1                 ;SX 1
        IL_SX 3                 ;SX 3
        IL_SX 1                 ;SX 1
        IL_DS                   ;DS
        IL_US                   ;US                CALL TYPEV AND OUTPUT BYTE
        IL_SP                   ;SP                POP RETURNED VALUE
        IL_NX                   ;NX                NEXT STATEMENT
PO5     IL_BE $+1               ;Z252       BE      * !257
        IL_NX                   ;NX
POKE    IL_BC OUT_, "POK", 'E'  ;BC      :OUT      'POKE'
        IL_LN ILPOKE            ;LN      ILPOKE
        IL_JS EXPR              ;JS      :EXPR
        IL_BR_F OU1             ;BR      Z253
OUT_    IL_BC SAVE, "OU", 'T'   ;BC      :SAVE     'OUT'
        IL_LN ILINPOUT          ;LN      ILINPOUT
        IL_LN 8                 ;LN      8
        IL_JS EXPRJS            ;JS      Z249      GET EXPR AND COMP >0 AND <8
OU1     IL_JS PE4               ;Z253       JS      Z245      CHECK FOR ,AND GET EXPR
        IL_BE $+1               ;BE      * !284
        IL_US                   ;US                CALL ILINPOUT OR ILPOKE
        IL_SP                   ;SP                POP RETURNED VALUE
        IL_NX                   ;NX                NEXT STATEMENT
SAVE    IL_BC SA1, "SAV", 'E'   ;BC      Z254      'SAVE'
        IL_BE $+1               ;BE      * !293
        IL_PC "TURN ON RECOR",'D';PC                'TURN ON RECORD'
        IL_NL                   ;NL
        IL_PC "HIT KE", 'Y'     ;PC                'HIT KEY'
        IL_BR_F SA2             ;BR      Z255
SA1     IL_J LOAD               ;Z254       J       :LOAD
SA2     IL_LN KEYV              ;Z255       LN      KEYV
        IL_DS                   ;DS
        IL_DS                   ;DS
        IL_US                   ;US                CALL KEY INPUT
        IL_SP                   ;SP                POP RETURNED VALUE
        IL_NL                   ;NL                NEW LINE
        IL_LN ILSAVE            ;LN      ILSAVE
        IL_LB MEND              ;LB      MEND
        IL_FV                   ;FV
        IL_LB BASIC             ;LB      BASIC
        IL_FV                   ;FV
        IL_SU                   ;SU                END PROGRAM-BEGIN PROGRAM
        IL_LN 256               ;LN      256
        IL_AD                   ;AD                ADD 256 ????
        IL_LB BASIC             ;LB      BASIC
        IL_FV                   ;FV
        IL_US                   ;US                CALL ILSAVE
        IL_SP                   ;SP                POP RETURNED VALUE
        IL_NX                   ;NX                NEXT STATEMENT
LOAD    IL_BC LO1, "LOA",'D'    ;BC      Z256      'LOAD'
        IL_BE $+1               ;BE      * !354
        IL_BR_F LO2             ;BR      Z257
LO1     IL_J REM                ;Z256       J       Z258
LO2     IL_LB MEND              ;Z257       LB      MEND
        IL_LN ILLOAD            ;LN      ILLOAD
        IL_LN 1                 ;LN      1
        IL_LB BASIC             ;LB      BASIC
        IL_FV                   ;FV
        IL_US                   ;US                CALL ILLOAD
        IL_BR_F LO3             ;BR      Z259      ILLOAD SKIPS THIS IF NO ERROR
        IL_LN 018h              ;LN      0x18      SPARE STACK SIZE-1,DOES NOT GET IT?
        IL_AD                   ;AD                ADD TO RETURN VALUE
        IL_SV                   ;SV                SAVE MEM END
        IL_WS                   ;WS                WARM START
LO3     IL_NL                   ;Z259       NL
        IL_PC "TAPE ERRO", 'R'  ;PC                'TAPE ERROR'
        IL_MT                   ;MT
REM     IL_BC DFLT, "RE",'M'    ;Z258       BC      :DFLT     'REM'
        IL_NX                   ;NX
DFLT    IL_BV $+1               ;BV      * !395
        IL_BC $+1, '', '='      ;BC      * !397    '='
        IL_J LET                ;J       :LET
EXPR    IL_BC EX1, '', '-'      ;BC      Z260      '-'
        IL_JS TERM              ;JS      :TERM
        IL_NE                   ;NE
        IL_BR_F EX3             ;BR      Z261
EX1     IL_BC EX2, '','+'       ;Z260       BC      Z262      '+'
EX2     IL_JS TERM              ;Z262       JS      :TERM
EX3     IL_BC EX4, '','+'       ;Z261       BC      Z263      '+'
        IL_JS TERM              ;JS      :TERM
        IL_AD                   ;AD
        IL_BR_B EX3             ;BR      Z261
EX4     IL_BC ERT, '','-'       ;Z263       BC      Z264      '-'
        IL_JS TERM              ;JS      :TERM
        IL_SU                   ;SU
        IL_BR_B EX3             ;BR      Z261
ERT     IL_RT                   ;Z264       RT
TERM    IL_JS RND               ;JS      :RND
TE1     IL_BC TE2, '','*'       ;Z266       BC      Z265      '*'
        IL_JS RND               ;JS      :RND
        IL_MP                   ;MP
        IL_BR_B TE1             ;BR      Z266
TE2     IL_BC TER, '','/'       ;Z265       BC      Z267      '/'
        IL_JS RND               ;JS      :RND
        IL_DV                   ;DV
        IL_BR_B TE1             ;BR      Z266
TER     IL_RT                   ;Z267       RT
RND     IL_BC RUSR, "RND",'('   ;BC      Z268      'RND('
        IL_LN 8080h             ;LN      0x8080
        IL_FV                   ;FV
        IL_LN 0929h             ;LN      0x0929
        IL_MP                   ;MP
        IL_LN 1A85h             ;LN      0x1A85
        IL_AD                   ;AD
        IL_SV                   ;SV
        IL_LB 80h               ;LB      0x80
        IL_FV                   ;FV
        IL_SX 1                 ;SX 1
        IL_DS                   ;DS
        IL_JS PEXPR             ;JS      Z269
        IL_BR_F RN1             ;BR      Z270
RUSR    IL_BR_F USR
RN1     IL_DS                   ;Z270       DS
        IL_SX 4                 ;SX 4
        IL_SX 2                 ;SX 2
        IL_SX 3                 ;SX 3
        IL_SX 5                 ;SX 5
        IL_SX 3                 ;SX 3
        IL_DV                   ;DV
        IL_MP                   ;MP
        IL_SU                   ;SU
        IL_DS                   ;DS
        IL_LB 6                 ;LB      6
        IL_LN 0                 ;LN      0
        IL_CP                   ;CP
        IL_NE                   ;NE
        IL_RT                   ;RT
USR     IL_BC INP, "USR",'('    ;BC      :INP      'USR('
        IL_JS EXPR              ;JS      :EXPR
        IL_JS PE4               ;JS      Z245
        IL_JS PE4               ;JS      Z245
        IL_BC $+1, '',')'       ;BC      * !495    ')'
        IL_US                   ;US
        IL_RT                   ;RT
INP     IL_BC FLG, "INP",'('    ;BC      :FLG      'INP('
        IL_LN ILINPOUT          ;LN      ILINPOUT
        IL_LN 8                 ;LN      8
        IL_JS EXPRJS            ;JS      Z249      GET EXPR AND COMP >0 AND <8
        IL_LN 8                 ;LN      8         ADD 8 TO OP FOR INPUT
        IL_AD                   ;AD
        IL_BR_F PE1             ;BR      Z271
FLG     IL_BC PEEK_, "FLG",'('  ;BC      :PEEK     'FLG('
        IL_LN ILFLG             ;LN      ILFLG     FLG WAS NEVER DOCUMENTED
        IL_LN 5                 ;LN      5
        IL_JS EXPRJS            ;JS      Z249      GET EXPR AND COMP >0 AND <5
        IL_LN 1                 ;LN      1
        IL_SU                   ;SU                SUB 1
        IL_BR_F PE1             ;BR      Z271
PEEK_   IL_BC PE5, "PEEK",'('   ;BC      Z272      'PEEK('
        IL_LN ILPEEK            ;LN      ILPEEK
        IL_JS EXPR              ;JS      :EXPR
PE1     IL_BC $+1, '',')'       ;Z271       BC      * !546    ')'
        IL_DS                   ;DS
        IL_US                   ;US
        IL_RT                   ;RT
PE5     IL_BV PE2               ;Z272       BV      Z273
        IL_FV                   ;FV
        IL_RT                   ;RT
PE2     IL_BN PE3               ;Z273       BN      Z274
        IL_RT                   ;RT
PE3     IL_BC $+1, '','('       ;Z274       BC      * !556    '('
PEXPR   IL_JS EXPR              ;JS      :EXPR
        IL_BC $+1, '', ')'      ;BC      * !560    ')'
        IL_RT                   ;RT
PE4     IL_BC PE6, '',','       ;Z245       BC      Z275      ','
        IL_J EXPR               ;JS      :EXPR
PE6     IL_DS                   ;Z275       DS
        IL_RT                   ;RT
RELOP   IL_BC RELESS, '','='    ;Z237       BC      Z276      '='
        IL_LB 2                 ;LB      2
        IL_RT                   ;RT
RELESS  IL_BC REMORE2, '', '<'  ;Z276       BC      Z277      '<'
        IL_BC REMORE, '', '='   ;BC      Z278      '='
        IL_LB 3                 ;LB      3
        IL_RT                   ;RT
REMORE  IL_BC REMORE1, '','>'   ;Z278       BC      Z279      '>'
        IL_LB 5                 ;LB      5
        IL_RT                   ;RT
REMORE1 IL_LB 1                 ;Z279       LB      1
        IL_RT                   ;RT
REMORE2 IL_BC $+1, '','>'       ;Z277       BC      * !589    '>'
        IL_BC RELESS1, '','='   ;BC      Z280      '='
        IL_LB 6                 ;LB      6
        IL_RT                   ;RT
RELESS1 IL_BC RELESS2, '','<'   ;Z280       BC      Z281      '<'
        IL_LB 5                 ;LB      5
        IL_RT                   ;RT
RELESS2 IL_LB 4                 ;Z281       LB      4
        IL_RT                   ;RT
EXPRJS  IL_JS EXPR              ;JS      :EXPR
        IL_DS                   ;DS
        IL_DS                   ;DS
        IL_SX 6                 ;SX 6
        IL_SX 1                 ;SX 1
        IL_SX 7                 ;SX 7
        IL_SX 1                 ;SX 1
RELOP1  IL_LB 1                 ;Z248       LB      1
        IL_SX 2                 ;SX 2
        IL_SX 1                 ;SX 1
        IL_CP                   ;CP
        IL_BR_F $+1             ;BR      * !616
        IL_LB 6                 ;LB      6
        IL_LN 0                 ;LN      0
        IL_CP                   ;CP
        IL_BR_F $+1             ;BR      * !623
ENDIL   IL_RT                   ;RT                 End Of IL Program
