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
      
    ld a, 2
    ld (playerRowPosition), a
    inc a
    ld (playerColPosition), a
        
    xor a
    ld (firstTime), a
    
    ld a, (Score)
    dec a
    daa
    ld (Score), a


gameLoop    
    ld a, (firstTime)
    cp 1
    jp z, firstTimeInit
    
    ;; read keys
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 1, a                            ; Z
    jp z, leftThruster

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 2, a						    ; M
    jp z, rightThruster							    ; jump to move shape right	

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 3, a					        ; N
    jp z, thrustMainEngine
    
    ld d, NON_FIRED
    ld e, 0
    jr updateStateAndDrawLEM

leftThruster
    ld d,1    ; d storing the thrust 
    ld e,LEFT_THRUSTER     ; e storing which engine or thruster
    jr updateStateAndDrawLEM    
rightThruster    
    ld d,1    ; d storing the thrust 
    ld e,RIGHT_THRUSTER     ; e storing which engine or thruster
    jr updateStateAndDrawLEM
thrustMainEngine
    ld d,3    ; d storing the thrust 
    ld e,MAIN_ENGINE     ; e storing which engine or thruster
    jr updateStateAndDrawLEM
    ;;;;;;;;; NO CODE SHOULD GO BETWEEN THIS AND  call updateLEMPhysicsState
    
updateStateAndDrawLEM    
    call updateLEMPhysicsState
    call drawLEM            
    
    call waitLoop
    jp gameLoop    

hitGroundGameOver
    ;ld a,       ; draw player one last time
    ;ld hl, (playerPosAbsolute)
    ;ld (hl), a

	ld bc,57
	ld de,youLostText    
    call printstring
    ld a, 1                 ;; reset score
    ld (Score), a   
   
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
    ;ld a, PLAYER_CHARACTER      ; draw player one last time
    ;ld hl, (playerPosAbsolute)
    ;ld (hl), a

	ld bc,57
	ld de,youWonText
	call printstring   
    
#ifdef RUN_ON_EMULATOR
    ld e, 20 
#else
    ld e, 15 
#endif   
waitPlayerWon     
    call waitLoop   
    dec e
    jp nz, waitPlayerWon
    
    jp initVariables
    ;; never gets to here


updateLEMPhysicsState

                        ; d stores thruster, e stores the thrust to apply set by caller
    ld a, d
    cp MAIN_ENGINE
    jp z, mainEngine    
    ;cp LEFT_THRUSTER
    ;jp z, leftThrust    
    ;cp RIGHT_THRUSTER
    ;jp z, rightThrust
    ;cp NON_FIRED
    jp calculateNewPosition  
    
leftThrust
    ld a, (x_vel)
    dec a
    ld (x_vel), a
rightThrust
    ld a, (x_vel)
    inc a
    ld (x_vel), a
mainEngine
    ld a, (y_vel)
    add a, e
    ld (y_vel), a
    
calculateNewPosition  
    ; always subtract the gravity until max v reached
    ld a, (y_vel)
    ;; need to check if a is > zero
    ld b, 1
    
    add a, b
    ld (y_vel), a
    
    ;ld hl, (playerPosAbsolute)
    ;ld d, 0
    ;ld e, (x_vel)
    ;ld a, 128
    ;cp e
    ;jp c, goingLeft
    ;jp goingRight
    jp addVertical
    
goingLeft    
    sbc hl, de 
    jp addVertical
goingRight
    add hl, de
    jp addVertical
    ld (playerPosAbsolute), hl
        
addVertical    
    ld hl, (playerPosAbsolute)
    ld de, $0021
    ld a, (y_vel)  
    ld c, 0
addVerticalLoop    
    add hl, de    
    dec a
    cp a
    jr nz,  addVerticalLoop
    
    ld (playerPosAbsolute), hl
    
endOfUpdatePhysics
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
;    ld a, SPACE_CHARACTER
;    ld hl, (playerPosAbsolute)
;    ld (hl), a
    
    ret

drawLEM         ;; on zx81 with blcok characters the LEM is a 3 by 3 grid, the hash defines are for each character 0 1 2 etc
                ;; we could, and maybe will, just deine these in a small memory block and effectively do a mem copy
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
    ld a, LEM_3_THR_OFF
    ld (hl), a
    inc hl
    ld a, LEM_4    
    ld (hl), a   
    inc hl
    ld a, LEM_5_THR_OFF
    ld (hl), a      
    
    ld de, $001f
    add hl, de   
    ld a, LEM_6
    ld (hl), a
    inc hl
    ld a, LEM_7_E_OFF    
    ld (hl), a   
    inc hl
    ld a, LEM_8
    ld (hl), a  
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
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line13
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line14
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line15
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line16
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line17
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line18
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line19
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line20
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line21
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line22
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,130,131,131,131,131,131,131,131,131,131,129,$76;Line23

                                                               
Variables:      
youWonText    
    DEFB	_Y,_O,_U,__,_W,_O,_N,$ff
youLostText    
    DEFB	_Y,_O,_U,__,_L,_O,_S,_T,$ff
blankText    
    DEFB	__,__,__,__,__,__,__,__,$ff    
tempChar
    DEFB 0
playerPosAbsolute
    DEFB 0,0
playerRowPosition
    DEFB 0
playerColPosition
    DEFB 0
firstTime    
    DEFB 1
Score    
    DEFB 0


;;;;; LEM state
x_vel
    DEFB 0
pad1    
    DEFB 0                
y_vel
    DEFB 0            
pad2 
    DEFB 0            
    
VariablesEnd:   DEFB $80
BasicEnd: 
#END
