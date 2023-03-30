;;; Lunar lander game for zx81 
;;; by Adrian Pilkington 2023
;;; https://youtube.com/@byteforever7829

;; game title inspired by the film Wargames (1983)
;; "A strange game. The only winning move is not to play"

;some #defines for compatibility with other assemblers
#define         DEFB .byte 
#define         DEFW .word
#define         EQU  .equ
#define         ORG  .org

;; note if assembling with intension of running in an emulator the timings are different
;; at least on my PAL TV zx81, it runs slower on real zx81, so comment in this #defines to 
;; alter delay timings
#define RUN_ON_EMULATOR


;;;;;#define DEBUG_NO_SCROLL

; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 
#define SCREEN_WIDTH 32
#define SCREEN_HEIGHT 23   ; we can use the full screen becuase we're not using PRINT or PRINT AT ROM subroutines

#define NON_FIRED 0
#define MAIN_ENGINE 1
#define LEFT_THRUSTER 2
#define RIGHT_THRUSTER 3
#define GRAVITY 1

#define LEM_0   6
#define LEM_1   10       ; roof
#define LEM_2   134

#define LEM_3_THR_OFF   144
#define LEM_4   8
#define LEM_5_THR_OFF  145

#define LEM_3_THR_ON   18
#define LEM_4   8
#define LEM_5_THR_ON  19

#define LEM_6   6       ;; lander left leg
#define LEM_7_E_OFF   3   ;; this is meant to be the descent engine when off
#define LEM_7_E_ON   137   ;; this is meant to be the descent engine when on 
#define LEM_8   134       ;; lander right leg

; character set definition/helpers
__:				EQU	$00	;spacja
_QT:			EQU	$0B	;"
_PD:			EQU	$0C	;funt 
_SD:			EQU	$0D	;$
_CL:			EQU	$0E	;:
_QM:			EQU	$0F	;?
_OP:			EQU	$10	;(
_CP:			EQU	$11	;)
_GT:			EQU	$12	;>
_LT:			EQU	$13	;<
_EQ:			EQU	$14	;=
_PL:			EQU	$15	;+
_MI:			EQU	$16	;-
_AS:			EQU	$17	;*
_SL:			EQU	$18	;/
_SC:			EQU	$19	;;
_CM:			EQU	$1A	;,
_DT:			EQU	$1B	;.
_NL:			EQU	$76	;NEWLINE

_BL             EQU $80; solid block

_0				EQU $1C
_1				EQU $1D
_2				EQU $1E
_3				EQU $1F
_4				EQU $20
_5				EQU $21
_6				EQU $22
_7				EQU $23
_8				EQU $24
_9				EQU $25
_A				EQU $26
_B				EQU $27
_C				EQU $28
_D				EQU $29
_E				EQU $2A
_F				EQU $2B
_G				EQU $2C
_H				EQU $2D
_I				EQU $2E
_J				EQU $2F
_K				EQU $30
_L				EQU $31
_M				EQU $32
_N				EQU $33
_O				EQU $34
_P				EQU $35
_Q				EQU $36
_R				EQU $37
_S				EQU $38
_T				EQU $39
_U				EQU $3A
_V				EQU $3B
_W				EQU $3C
_X				EQU $3D
_Y				EQU $3E
_Z				EQU $3F


;;;; this is the whole ZX81 runtime system and gets assembled and 
;;;; loads as it would if we just powered/booted into basic

           ORG  $4009             ; assemble to this address
                                                                
VERSN:          DEFB 0
E_PPC:          DEFW 2
D_FILE:         DEFW Display
DF_CC:          DEFW Display+1                  ; First character of display
VARS:           DEFW Variables
DEST:           DEFW 0
E_LINE:         DEFW BasicEnd 
CH_ADD:         DEFW BasicEnd+4                 ; Simulate SAVE "X"
X_PTR:          DEFW 0
STKBOT:         DEFW BasicEnd+5
STKEND:         DEFW BasicEnd+5                 ; Empty stack
BREG:           DEFB 0
MEM:            DEFW MEMBOT
UNUSED1:        DEFB 0
DF_SZ:          DEFB 2
S_TOP:          DEFW $0002                      ; Top program line number
LAST_K:         DEFW $fdbf
DEBOUN:         DEFB 15
MARGIN:         DEFB 55
NXTLIN:         DEFW Line2                      ; Next line address
OLDPPC:         DEFW 0
FLAGX:          DEFB 0
STRLEN:         DEFW 0
T_ADDR:         DEFW $0c8d
SEED:           DEFW 0
FRAMES:         DEFW $f5a3
COORDS:         DEFW 0
PR_CC:          DEFB $bc
S_POSN:         DEFW $1821
CDFLAG:         DEFB $40
MEMBOT:         DEFB 0,0 ;  zeros
UNUNSED2:       DEFW 0

Line1:          DEFB $00,$0a                    ; Line 10
                DEFW Line1End-Line1Text         ; Line 10 length
Line1Text:      DEFB $ea                        ; REM
        
firstTimeInit
    ld a, 1
    ld (Score), a    
initVariables
    ;; some variable initialisation
    ld hl, (DF_CC)
    ld de, 69
    add hl, de
    ld (playerPosAbsolute), hl      ; this is the centre of the LEM (Lander) which is a 3 by 3 grid
    
    call drawLEM
      
    ld a, 1
    ld (lemRowPos), a
    inc a
    inc a
    ld (lemColPos), a
        
    xor a
    ld (everyOther), a
    ld (countSinceEngineOn), a
    ld (leftThrustOn), a
    ld (rightThrustOn), a
    ld (firstTime), a
    ld (y_vel), a    
    ld (x_vel), a
    ld a, $20       ; store altitude in bcd for display
    ld (altitude), a 
    
    ld a, (Score)
    dec a
    ld (Score), a
    ld a, 102
    ld (agc_program), a
    
    ld a, 96
    ld (agc_noun), a
    
    ld a, 6
    ld (agc_verb), a
    
    ld a, 117
    ld (status_FuelQty), a    
    

gameLoop    
    ld a, (firstTime)
    cp 1
    jp z, firstTimeInit

    ld d, NON_FIRED
    ld e, 0

    call eraseLEM
    
    ;; read keys
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 1, a                            ; Z
    jp z, rightThruster

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 2, a						    ; N
    jp z, leftThruster							    ; jump to move shape right	

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 3, a					        ; M
    jp z, thrustMainEngine
    
    ld d, NON_FIRED
    ld e, 0
    jp updateStateAndDrawLEM

leftThruster            ; firing left thruster causes lem to move right
    ld a, 1
    ld (leftThrustOn), a    
    ld a, (lemColPos)    
    cp 17
    jp z, updateStateAndDrawLEM
    inc a
    ld (lemColPos), a
    ld hl, (playerPosAbsolute)
    inc hl
    ld (playerPosAbsolute), hl
    ld a, (x_vel)
    dec a
    ld (x_vel), a
    jr updateStateAndDrawLEM    
rightThruster    ; firing right thruster causes lem to move left
    ld a, 1
    ld (rightThrustOn), a
    
    ld a, (lemColPos)    
    cp 0
    jp z, updateStateAndDrawLEM 
    dec a
    ld (lemColPos), a
    ld hl, (playerPosAbsolute)    
    dec hl
    ld (playerPosAbsolute), hl
    ld a, (x_vel)
    inc a
    ld (x_vel), a
    jr updateStateAndDrawLEM
thrustMainEngine    ; firing main engine causes lem to move up by two (but gravity brings down always by 1)
    ; check fuel quanity remaining
    ld a, (status_FuelQty)
    cp 0
    jp z, updateStateAndDrawLEM  ; if fuel run out then that's it only one way and that's down via gravity :)
    
    ld a, 1
    ld (mainEngineOn), a
    call moveLemUp 
    call moveLemUp   
    call moveLemUp
    ld a, (status_FuelQty)
    dec a
    daa
    ld (status_FuelQty), a
    
    ld a, (countSinceEngineOn)
    cp 0
    jp nz, resetCountSinceEngineOn
    
    ld a, (everyOther)
    cp 3
    jp z, zeroEveryOther
    inc a
    ld (everyOther), a
    ld a, (countSinceEngineOn)    
    inc a
    ld (countSinceEngineOn), a
    jr updateStateAndDrawLEM
zeroEveryOther
    xor a
    ld (everyOther), a
resetCountSinceEngineOn
    ld a, 0 
    ld (countSinceEngineOn), a
    jr updateStateAndDrawLEM    
    ;;;;;;;;; NO CODE SHOULD GO BETWEEN THIS AND  call updateLEMPhysicsState unless push/pop de
    
updateStateAndDrawLEM        
    ;call updateLEMPhysicsState    
    ;; simple physics to get a demo going is to always ad 33 for gravity of + one row        
    ld a, (countSinceEngineOn)
    ld b, a
    inc b
accelerateToGround        
    call moveLemDown
    djnz accelerateToGround
     
    
    ld a, (countSinceEngineOn)
    inc a
    ld (countSinceEngineOn), a
    
    call drawLEM            
    call updateAGC
    
    xor a 
    ld (leftThrustOn), a
    ld (rightThrustOn), a
    ld (mainEngineOn), a
       
    ld a, (altitude)         
    cp 2
    jp z, checkCrash
    cp 1
    jp z, checkCrash
    cp 0
    jp z, checkCrash
    
aftercheckCrash
    call waitLoop
    jp gameLoop    
    
checkCrash
    ld a, (countSinceEngineOn)
    cp 4
    jp nc, hitGroundGameOver    
    jp playerWon

hitGroundGameOver
    ld hl, (playerPosAbsolute)
    ld a, 6
    ld (hl), a
    dec hl
    ld (hl), a
    inc hl
    inc hl
    ld (hl), a
    inc hl
    ld (hl), a
    ld hl, (playerPosAbsolute)
    ld de, 33
    sbc hl, de
    dec hl
    ld (hl), a
    dec hl
    ld (hl), a
    inc hl
    inc hl
    inc hl
    ld (hl), a
        
   
#ifdef RUN_ON_EMULATOR
    ld e, 20 
#else
    ld e, 15 
#endif        
    
waitPlayerOver           
    call waitLoop   
    dec e
    jp nz, waitPlayerOver
    jp initVariables
    ;; never gets to here
   
playerWon    
   
#ifdef RUN_ON_EMULATOR
    ld e, 20 
#else
    ld e, 15 
#endif   
waitPlayerWon     
    call waitLoop   
    dec e
    jp nz, waitPlayerWon
    
    call eraseLEM
    jp initVariables
    
    ;; never gets to here


updateLEMPhysicsState
    ;todo   
    ret
    
waitLoop
#ifdef RUN_ON_EMULATOR
    ld bc, $2fff     ; set wait loop delay for emulator
#else
    ld bc, $0eff     ; set wait loop delay 
#endif    
waitloop1
    dec bc
    ld a,b
    or c
    jr nz, waitloop1
    ret


eraseLEM
    ld hl, (playerPosAbsolute)      ; playerPosAbsolute is the top left of the lander    
    ld a, 0
    ld (hl), a
    inc hl
    ld a, 0
    ld (hl), a   
    inc hl
    ld a, 0
    ld (hl), a
    
    ld hl, (playerPosAbsolute)      ; playerPosAbsolute is the centre of the lander    
    ld de, $0021
    add hl, de 
    ld a, 0
    ld (hl), a
    inc hl
    ld a, 0    
    ld (hl), a   
    inc hl
    ld a, 0
    ld (hl), a      
    
    ld de, $001f
    add hl, de   
    ld a, 0
    ld (hl), a
    inc hl
    ld a, 0    
    ld (hl), a   
    inc hl
    ld a, 0
    ld (hl), a  
    
    ld a, (mainEngineOn)
    cp 1
    jp z, mainEngineNotOn
    ld de, 32
    add hl, de    
    ld a, 0
    ld (hl), a
mainEngineNotOn    
    ret

drawLEM         ;; on zx81 with blcok characters the LEM is a 3 by 3 grid, the hash defines are for each character 0 1 2 etc
                ;; we could, and maybe will, just deine these in a small memory block and effectively do a mem copy


    
    ld hl, (playerPosAbsolute)      ; playerPosAbsolute is the top left of the lander    
    ld a, LEM_0
    ld (hl), a
    inc hl
    ld a, LEM_1
    ld (hl), a   
    inc hl
    ld a, LEM_2
    ld (hl), a
    
    ld hl, (playerPosAbsolute)      ; playerPosAbsolute is the centre of the lander    
    ld de, $0021
    add hl, de 
    ld a, (leftThrustOn)
    cp 0
    ld a, LEM_3_THR_OFF
    jp z, leftThrustIsOn
    ld a, LEM_3_THR_ON
leftThrustIsOn    
    ld (hl), a
    inc hl
    ld a, LEM_4    
    ld (hl), a   
    inc hl
    ld a, (rightThrustOn)
    cp 0
    ld a, LEM_5_THR_OFF
    jp z, rightThrustIsOn
    ld a, LEM_5_THR_ON
rightThrustIsOn            
    ld (hl), a      
    
    ld de, $001f
    add hl, de   
    ld a, LEM_6
    ld (hl), a
    inc hl
    
    ld a, (mainEngineOn)
    cp 0
    ld a, LEM_7_E_OFF    
    jp z, mainEngineIsOn
    push hl
    ld de, 33
    add hl, de    
    ld a, 8
    ld (hl), a
    pop hl
    
    ld a, LEM_7_E_ON

mainEngineIsOn         
    ld (hl), a   
    inc hl
    ld a, LEM_8
    ld (hl), a  
    ret

moveLemUp 
    ld a, (lemRowPos)
    cp 0     
    jp z, endOfmoveLemUp
    dec a
    ld (lemRowPos), a
     
    ld hl, (playerPosAbsolute)
    ld de, 33
    sbc hl, de
    ld (playerPosAbsolute), hl
    ld a, (altitude)
    inc a
    daa
    ld (altitude), a
endOfmoveLemUp    
    ret

moveLemDown
    ld a, (lemRowPos)
    cp 20
    jp z, endOfmoveLemDown
    inc a
    ld (lemRowPos), a

    ld hl, (playerPosAbsolute)
    ld de, 33
    add hl, de
    ld (playerPosAbsolute), hl
    ld a, (altitude)
    dec a
    daa
    ld (altitude), a
endOfmoveLemDown    
    ret

updateAGC

    ;ld de, 291  
    ;ld a, (y_vel_disp)    
    ;call print_number8bits

    ld de, 357
    ld a, (altitude)    
    call print_number8bits

    ; print the agc program
    ld de, 94
    ld a, (agc_program) ; stored as bcd, so 66 is 102 in decimal
    call print_number8bits
   
    ;print the current verb and noun
    ld de, 155
    ld a, (agc_verb) ; stored as bcd
    call print_number8bits    

    ld de, 160
    ld a, (agc_noun) ; stored as bcd
    call print_number8bits    

   
    ; flash the COMP ACTY (Computer actvity) inverse video and back as in real agc
    ld a, (compActyEvenOdd)
    cp 1
    jp z, inverseVideoPrintCA
    ld a, 1
    ld (compActyEvenOdd), a
    ld bc,56
    ld de, inverseVidStrCApt1
    call printstring
    ld bc,89
    ld de, inverseVidStrCApt2
    call printstring
        
    jp afterPrint
inverseVideoPrintCA    
    xor a
    ld (compActyEvenOdd), a 
    ld bc,56
    ld de, noramlVidStrCApt1
    call printstring
    ld bc,89
    ld de, noramlVidStrCApt2
    call printstring       
afterPrint    

    ld de, 485
    ld a, (status_FuelQty) ; stored as bcd
    call print_number8bits 
    
    ld de, 291 
    ld a, (countSinceEngineOn)
    call print_number8bits 
    ret

        
; this prints at to any offset (stored in bc) from the top of the screen Display, using string in de
printstring
    ld hl,Display
    add hl,bc	
printstring_loop
    ld a,(de)
    cp $ff
    jp z,printstring_end
    ld (hl),a
    inc hl
    inc de
    jr printstring_loop
printstring_end	
    ret  
    
print_number8bits
    ld hl, (DF_CC)    
    add hl, de    
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a  
    
    ret

    
                DEFB $76                        ; Newline        
Line1End
Line2			DEFB $00,$14
                DEFW Line2End-Line2Text
Line2Text     	DEFB $F9,$D4                    ; RAND USR
				DEFB $1D,$22,$21,$1D,$20        ; 16514                
                DEFB $7E                        ; Number
                DEFB $8F,$01,$04,$00,$00        ; Numeric encoding
                DEFB $76                        ; Newline
Line2End            
endBasic
                                                                
Display        	DEFB $76                                                  ;agc
				DEFB _L,_A,_N,_D,_E,_R, ,_S,_I,_M,_U,_L,_A,_T,_I,_O,_N,0,0,0,0,7,3,3,3,3,3,3,3,3,3,132,$76;Line0
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_C,_O,_M,_P,0,_P,_R,_O,_G,133,$76;Line1
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_A,_C,_T,_Y,0,0,_0,_0,0,133,$76;Line2
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_V,_E,_R,_B,0,_N,_O,_U,_N,133,$76;Line3
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,_0,_0,0,0,0,_0,_0,0,133,$76;Line4
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_BL,_BL,_BL,_BL,_BL,_BL,_BL,_BL,_BL,133,$76;Line5
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,21,_0,_0,_0,_0,_0,0,0,133,$76;Line6
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_BL,_BL,_BL,_BL,_BL,_BL,_BL,_BL,_BL,133,$76;Line7
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,21,_0,_0,_0,_0,_0,0,0,133,$76;Line8
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_BL,_BL,_BL,_BL,_BL,_BL,_BL,_BL,_BL,133,$76$76;Line9
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,21,_0,_0,_0,_0,_0,0,0,133,$76;Line10
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_BL,_BL,_BL,_BL,_BL,_BL,_BL,_BL,_BL,133,$76;Line11
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line12
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_F,_U,_E,_L,0,0,0,0,0,133,$76;Line13
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,_X,_X,0,0,0,0,0,0,133,$76;Line14
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line15
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line16
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line17
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line18
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line19
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line20
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line21
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line22
                DEFB 128,129,131,131,136,136,138,136,128,128,136,8,137,137,137,130,130,130,129,130,129,130,131,131,131,131,131,131,131,131,131,129,$76;Line23

                                                               
Variables:      
inverseVidStrCApt1
    DEFB	$a8,$b4,$b2,$b5,$ff    
inverseVidStrCApt2
    DEFB	$a6,$a8,$b9,$be,$ff        
noramlVidStrCApt1
    DEFB    $28,$34,$32,$35,$ff
noramlVidStrCApt2
    DEFB    $26,$28,$39,$3e,$ff
tempChar
    DEFB 0
playerPosAbsolute
    DEFB 0,0
lemRowPos
    DEFB 0
lemColPos
    DEFB 0  
firstTime    
    DEFB 1
Score    
    DEFB 0
leftThrustOn    
    DEFB 0
rightThrustOn    
    DEFB 0
mainEngineOn
    DEFB 0
status_FuelQty    
    DEFB 0
countSinceEngineOn
    DEFB 0
;;;;; LEM state
x_vel
    DEFB 0  
y_vel
    DEFB 0
y_vel_disp
    DEFB 0
altitude
    DEFB 0
agc_program
    DEFB 0
compActyEvenOdd
    DEFB 0
agc_noun
    DEFB 0
agc_verb   
    DEFB 0   
;; for number conversion
tempStr
    DEFB 0,0,0,0,0,0,0,0,0,0,$ff
everyOther
    DEFB 0
VariablesEnd:   DEFB $80
BasicEnd: 
#END
