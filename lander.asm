;;; Lunar lander game for zx81 
;;; by Adrian Pilkington 2023
;;; https://youtube.com/@byteforever7829

;some #defines for compatibility with other assemblers
#define         DEFB .byte 
#define         DEFW .word
#define         EQU  .equ
#define         ORG  .org

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

VSYNCLOOP       EQU      5
VSYNCLOOPFASTATSTART       EQU      2

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

            ORG 16509       ;; we have to push the place in memory for this here becuase basic has 
                    ;; to start at 16514 if memory was tight we could use the space between UNUSED2
                    ;; and Line1 for variables

Line1:          DEFB $00,$0a                    ; Line 10
                DEFW Line1End-Line1Text         ; Line 10 length
Line1Text:      DEFB $ea                        ; REM

    

preinit
    ld a, 1  
    ld (firstTime), a    
    xor a
    ld (scoreCrashes), a     
    ld (scoreCrashes+1), a     
    ld (scoreGoodLand), a    
    ld (scoreGoodLand+1), a    
    
    call printInstructions        
    
    ld de, brakingBurnText
    ld bc, 463
    call printstring
    
    ld b, 30     
    ld a, $30   ;; for bcd has to be in hex and "look like 30" to come out as 30 in bcd decimal
    ld (brakingBurnCountDown), a
waitBeofreStart  
    push bc         
    
    call waitLoop       
    
    ld a, (brakingBurnCountDown)
    dec a
    daa
    ld (brakingBurnCountDown), a
    ld de, 507 
    call print_number8bits    
    
    pop bc
    djnz waitBeofreStart
    
    
    ld de, clearBrakingBurnText
    ld bc, 463
    call printstring
    ld de, clearBrakingBurnText
    ld bc, 496
    call printstring    
    
initVariables
    ld a, (firstTime)
    cp 0
    jp z, preinit
    call printStarsAndStripesCLEAR    
    
    ld de, blankPartLine
    ld bc, 518
    call printstring 
    ld bc, 551
    call printstring     

    ld de, commOKText
    ld bc, 490
    call printstring
    
    ld bc, 1
    ld de, titleText
    call printstring
    ;; some variable initialisation
    ld hl, (DF_CC)
    ld de, 69
    add hl, de
    ld (playerPosAbsolute), hl      ; this is the centre of the LEM (Lander) which is a 3 by 3 grid
    ld hl, (DF_CC)
    ld de, 200
    add hl, de
    ld (starPositionAbsolute), hl


    ld a, (firstTime)
    cp 0
    jp z, preinit
    
    
    ld a, (scoreCrashes)
    ld de, 753
    call print_number8bits

    ld a, (scoreGoodLand)
    ld de, 749
    call print_number8bits 

    ld a, (firstTime)
    cp 0
    jp z, preinit
        
    call drawLEM       

    
    ld a, 1
    ld (lemRowPos), a
    inc a
    inc a
    ld (lemColPos), a 
    
    ld a, 2
    ld (fuelWarningFlash),a
    
    xor a    
    ld (everyOther), a    
    ld (fuelWarningLatch), a
    ld (countSinceEngineOnNeg), a
    ld (countSinceEngineOnPosi), a
    ld (leftThrustOn), a
    ld (rightThrustOn), a
    
    ld (y_vel), a    
    ld (x_velPosi), a
    ld (x_velNeg), a
    ld a, $20       ; store altitude in bcd for display
    ld (altitude), a 
    
    ld a, 102
    ld (agc_program), a
    
    ld a, 96
    ld (agc_noun), a
    
    ld a, 6
    ld (agc_verb), a
    
    ld a, 153    
    ld (status_FuelQty), a    
    
    ld bc, $299    
    ld (distanceToLandingZonePos), bc
    
    ld a, 128
    ld (prog64Countdown), a
    
    ;; clear the lower 4 rows of play area, as may be "crash" debris    
    
    ld de, clearRow
    ld bc,595    
    call printstring    
    ld bc,628
    call printstring
    ld bc,661
    call printstring
    ld bc,694
    call printstring
    
    ld bc,694
    ld de, moonSurface0+40
    ld (ptrToGround0), de
    call printGroundSurface0
    
    ld bc,727
    ld de, moonSurface1+40
    ld (ptrToGround1), de
    call printGroundjustAbove
    
    ld bc,760
    ld de, moonSurface2+40
    ld (ptrToGround2), de
    call printGroundBottom
       
    ;;; zero registers
    xor a
    ld bc, 0   
    ld hl, 0   
    ld de, 0
    
    ld a, (firstTime)
    cp 0
    jp z, preinit
    
gameLoop_main_prog_64    ; this is the initial game loop which is like agc program "64 LEM: LM Approach", but hands off like an intro to gam
    ld a, (firstTime)
    cp 0
    jp z, preinit

    ld d, NON_FIRED
    ld e, 0

	ld b,VSYNCLOOPFASTATSTART
waitForTVSync_prog64	
	call vsync
	djnz waitForTVSync_prog64

    ;call eraseLEM    

    
    ld a, 1
    ld (x_velPosi), a       

    ;call drawLEM
    
    ;;; display thrust from "braking thruster" on right of lem    
    ld hl,Display
    ld de, 106
    add hl, de
    ld a, LEM_5_THR_ON    
    ld (hl), a
    inc hl
    ld (hl), a    

    call scrollGroundLeft
    
    ld a, 100
    ld (agc_program), a ; stored as bcd, so 64 is 100 in decimal
    
    
    ld bc, (distanceToLandingZonePos)    
    dec bc
    ld a, b
    or c        ; if b or c is zero then bc must be zero!
    cp 0
    jp z, noDecrementDistPos

    ld a, b
    daa
    ld b, a
    ld a, c
    daa
    ld c, a
    ld (distanceToLandingZonePos), bc
    
    
    call updateAGC

    ld a, (prog64Countdown)
    ld de, 225       
    call print_number8bits        
    
    ld a, (prog64Countdown)    
    dec a
    daa
    cp 0
    jp z, gameLoop_main_prog66
    ld (prog64Countdown), a
        
    ld bc, 683
    ld de, prog64Text
    call printstring    
    
    jp gameLoop_main_prog_64

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

gameLoop_main_prog66    
    ld a, 102
    ld (agc_program), a ; stored as bcd, so 66 is 102 in decimal
    
    ld bc, 683
    ld de, prog66Text
    call printstring
    
    ld a, (firstTime)
    cp 0
    jp z, preinit

    ld d, NON_FIRED
    ld e, 0

	ld b,VSYNCLOOP
waitForTVSync	
	call vsync
	djnz waitForTVSync

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
    
checkMainEngineKey
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 3, a					        ; M
    jp z, thrustMainEngine
    
    ld d, NON_FIRED
    ld e, 0
    jp updateStateAndDrawLEM

leftThruster            ; firing left thruster causes lem to move right
    ;; check if last time the other thruster was on, then zero rate
    ld a, (x_velNeg)
    cp 1
    jp z, zeroBothL
    
    ld a, 1         
    ld (leftThrustOn), a    ;; this is to change "sprite"        
    ld (x_velPosi), a       
    xor a
    ld (x_velNeg), a
    jr checkMainEngineKey
    
zeroBothL
    xor a
    ld (x_velNeg), a           
    ld (x_velPosi), a
    jr updateStateAndDrawLEM
    
rightThruster    ; firing right thruster causes lem to move left
    ;; check if last time the other thruster was on, then zero rate
    ld a, (x_velPosi)
    cp 1
    jp z, zeroBothR
    ld a, 1
    ld (rightThrustOn), a  ;; this is to change "sprite"           
    ld (x_velNeg), a       
    xor a
    ld (x_velPosi), a
    jr checkMainEngineKey
zeroBothR    
    xor a
    ld (x_velNeg), a           
    ld (x_velPosi), a
    jr updateStateAndDrawLEM
thrustMainEngine    ; firing main engine causes lem to move up by two (but gravity brings down always by 1)
    ; check fuel quanity remaining
    ld a, (status_FuelQty)
    cp 0
    jp z, updateStateAndDrawLEM  ; if fuel run out then that's it only one way and that's down via gravity :)
    
    ld a, 1
    ld (mainEngineOn), a
    
    ld a, (countSinceEngineOnNeg)
    ;cp 7
    ;jp z, skipIncMainEngine    ;; if greater than 7 then skip inc, limit to 8
    inc a
    daa
    ld (countSinceEngineOnNeg), a    
    ld a, (status_FuelQty)
    dec a
    daa
    ld (status_FuelQty), a    
    xor a
    ld (countSinceEngineOnPosi), a
skipIncMainEngine 
      
    jp updateStateAndDrawLEM
    
    
    ;;;;;;;;; NO CODE SHOULD GO BETWEEN THIS AND  call updateLEMPhysicsState unless push/pop de    
updateStateAndDrawLEM        

    ld a, (everyOther)
    inc a
    ld (everyOther), a
    cp 2
    jp z, doStuffEveryOther
    jp afterDoStuffEveryOther
doStuffEveryOther
    xor a
    ld (everyOther), a        
    ld a, (countSinceEngineOnNeg)    
    cp 0
    jp z, mainEngineEffectZero        
    dec a
    daa
    ld (countSinceEngineOnNeg), a    

    jr afterDoStuffEveryOther    
    
mainEngineEffectZero
    ld a, (countSinceEngineOnPosi)    
    inc a
    daa
    cp 9
    jp z, noIncDownRate
    
    ld (countSinceEngineOnPosi), a        
noIncDownRate    
    
afterDoStuffEveryOther    
    call moveLemLeftRight  
    
    ld a, (countSinceEngineOnNeg)
    cp 0
    jp z, checkMoveLemDown    
    ld b, a
moveLemUpLoop    
    push bc
    call eraseLEM
    call moveLemUp    
    call updateAGC  
    call drawLEM 
    pop bc
    djnz moveLemUpLoop
checkMoveLemDown  
    ld a, (countSinceEngineOnPosi)
    cp 0
    jp z, afterCheckMoveLemUp    
    ld b, a
moveLemDownLoop 
    push bc    
    call eraseLEM
    call moveLemDown
    ld a, (altitude)         
    cp 2
    jp z, checkCrash
    call updateAGC  
    call drawLEM 
    pop bc
    djnz moveLemDownLoop      
afterCheckMoveLemUp    
    
    call drawLEM 
    
    xor a 
    ld (leftThrustOn), a
    ld (rightThrustOn), a
    ld (mainEngineOn), a
       
    ld a, (altitude)         
    cp 2
    jp z, checkCrash
    
aftercheckCrash
    jp gameLoop_main_prog66    
    
checkCrash
    ld a, (countSinceEngineOnPosi)
    cp 2
    jp nc, hitGroundGameOver    
    ld a, (x_velNeg)
    cp 0    
    jp nz, hitGroundGameOver
    ld a, (x_velPosi) 
    cp 0
    jp nz, hitGroundGameOver    
    
    ld hl, (playerPosAbsolute)   ; check that have landed with L under centre of the LEM, ie landing zone
    ld de, 100    
    add hl, de
    ld a, (hl)
    cp 177
    jp z, playerWon
    jp hitGroundGameOver        
    

hitGroundGameOver
    ld a, 6
    ld hl, (playerPosAbsolute)
    ld de, 33
    sbc hl, de
    ld (hl), a    
    inc hl
    ld (hl), a   
    inc hl
    ld (hl), a
    sbc hl, de
    ld (hl), a
    dec hl
    ld (hl), a
    dec hl
    ld (hl), a   
    ld bc, 1
    ld de, youCrashedText    
    call printstring
    
    ld a, (scoreCrashes)
    inc a
    daa
    ld (scoreCrashes), a
    ld de, 753
    call print_number8bits
    
    ld de, commFailText
    ld bc, 490
    call printstring
    
    call printYouDiedAsHonorableHeros
    
    call drawLEM
  
   
    ld e, 20     
waitPlayerOver           
    call waitLoop   
    dec e
    jp nz, waitPlayerOver
    jp initVariables
    ;; never gets to here
   
playerWon    
    ld bc, 1
    ld de, goodLandingText
    call printstring    
    
    ld a, (scoreGoodLand)
    inc a
    daa
    ld (scoreGoodLand), a
    ld de, 749
    call print_number8bits

    call printStarsAndStripes
    
    call drawLEM 

    ld e, 20 
waitPlayerWon           
    call waitLoop   
    dec e
    jp nz, waitPlayerWon
    jp initVariables    
    ;; never gets to here


updateLEMPhysicsState
    ;todo   
    ret
    
waitLoop
    ld bc, $1aff     ; set wait loop delay for emulator
waitloop1
    dec bc
    ld a,b
    or c
    jr nz, waitloop1
    ret
    
moveLemLeftRight
    ld a, (x_velNeg)
    cp 0
    jp z, checkMoveOtherWay
    ; move left
    ld bc, (distanceToLandingZonePos)    
    inc bc
    ld a, b
    daa
    ld b, a
    ld a, c
    daa
    ld c, a
    ld (distanceToLandingZonePos), bc
    
    ld a, (lemColPos)    
    cp 0
    jp z, scrollGroundRight
    dec a
    ld (lemColPos), a
    ld hl, (playerPosAbsolute)    
    dec hl
    ld (playerPosAbsolute), hl    
    
    
checkMoveOtherWay    
    ld a, (x_velPosi)
    cp 0
    jp z, endMoveLemLeftRightEnd    
    ;; move lem right
    
    ld bc, (distanceToLandingZonePos)    
    dec bc
    ld a, b
    or c        ; if b or c is zero then bc must be zero!
    cp 0
    jp z, noDecrementDistPos
        
    ld a, b
    daa
    ld b, a
    ld a, c
    daa
    ld c, a
    ld (distanceToLandingZonePos), bc
noDecrementDistPos
    ld a, (lemColPos)    
    cp 17
    jp z, scrollGroundLeft
    inc a
    ld (lemColPos), a
    ld hl, (playerPosAbsolute)
    inc hl
    ld (playerPosAbsolute), hl
    
    jp endMoveLemLeftRightEnd

scrollGroundLeft  

    ld hl, (ptrToGround0)
    inc hl
    ld (ptrToGround0), hl
    
    ld hl, (ptrToGround1)
    inc hl
    ld (ptrToGround1), hl
    
    ld hl, (ptrToGround2)
    inc hl    
    ld (ptrToGround2), hl
    
    ld bc,694
    ld de, (ptrToGround0)
    call printGroundSurface0    
    
    ld bc,727
    ld de, (ptrToGround1)
    call printGroundjustAbove
   
    ld bc, 760
    ld de, (ptrToGround2)
    call printGroundBottom
        
    
    jp endMoveLemLeftRightEnd
    
scrollGroundRight


    ld hl, (ptrToGround0)
    dec hl
    ld (ptrToGround0), hl
    
    ld hl, (ptrToGround1)
    dec hl
    ld (ptrToGround1), hl
    
    ld hl, (ptrToGround2)
    dec hl
    ld (ptrToGround2), hl
    

    ld bc,694
    ld de, (ptrToGround0)
    call printGroundSurface0
    
    ld bc,727
    ld de, (ptrToGround1)
    call printGroundjustAbove 
   
    ld bc, 760
    ld de, (ptrToGround2)
    call printGroundBottom  
    
endMoveLemLeftRightEnd
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
                ;; we could, and maybe will, just define these in a small memory block and effectively do a mem copy
   
    ; ld hl, (starPositionAbsolute)   ; print the stars first , they stationary play area, maybe they should move as well?!
    ; ld a, 23   ; star/asterisk
    ; ld (hl), a
    ; ld de, 112
    ; add hl, de
    ; ld (hl), a
    ; ld de, 29
    ; add hl, de    
    ; ld (hl), a

    ; ld de, 259
    ; sbc hl, de    
    ; ld (hl), a
    ; ld de, 22
    ; add hl, de    
    ; ld (hl), a
    ; ld de, 36
    ; add hl, de    
    ; ld (hl), a
    ; ld de, 35
    ; add hl, de    
    ; ld (hl), a
    
   
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
    ;push hl        ;; did have rocket exhaust "effect" below LEM but it wasn't being erased reliably
    ;ld de, 33
    ;add hl, de    
    ;ld a, 8
    ;ld (hl), a
    ;pop hl
    
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


    ld de, 651
    ld bc, (distanceToLandingZonePos) ; stored as bcd    
    call print_number16bits 
    
    ld de, 485
    ld a, (status_FuelQty) ; stored as bcd
    call print_number8bits 
    
    ld a, (fuelWarningLatch)
    cp 1
    jp z, flashFuelWarning
    ld a, (status_FuelQty)
    ;;; flash a warning message if fuel low
    cp $15              ; this is bcd remember so hex 15 ($15) _is_ 15
    jp z, flashFuelWarning
    cp $14              ; this is bcd remember so hex 14 ($14) _is_ 14
    jp z, flashFuelWarning    
    jp afterFlashFuelWarning
flashFuelWarning
    ld a, 1
    ld (fuelWarningLatch), a
    ld a, (fuelWarningFlash)    
    dec a
    ld (fuelWarningFlash), a
    cp 0
    jp z, printFuelWarningNormal   

    ld de, fuelWarningTextNormal    
    ld bc, 518
    call printstring 
    ld de, fuelWarningTextInverse
    ld bc, 551
    call printstring 
    jp afterFlashFuelWarning
printFuelWarningNormal    
    ld a, 2
    ld (fuelWarningFlash), a
    ld de, fuelWarningTextNormal    
    ld bc, 551
    call printstring 
    ld de, fuelWarningTextInverse
    ld bc, 518
    call printstring 
    
afterFlashFuelWarning    
    ld de, 291 
    ld a, (countSinceEngineOnPosi)
    cp 0
    jp z, checkVerticalRiseRate        
    call print_number8bits 
    
    ld de, 288      ; print + symbol
    ld hl, Display
    add hl, de
    ld a, 21   ; symbol code for "+"
    ld (hl), a
      
    jr afterCheckVerticalRiseRate
checkVerticalRiseRate
    ld a, (countSinceEngineOnNeg)    ; should really be called vertical rate negatgive!
    cp 0
    jp z, printZeroVerticalRate
    ld a, (countSinceEngineOnNeg)
    call print_number8bits  
    ld de, 288
    ld hl, Display
    add hl, de
    ld a, 22   ; symbol code for "-"
    ld (hl), a

    jp afterCheckVerticalRiseRate
printZeroVerticalRate
    ld a, 0
    call print_number8bits  

afterCheckVerticalRiseRate            

    ld a, (x_velPosi)
    cp 0
    jp z, checkVelocityNegative
    ld de, 222
    ld hl, Display
    add hl, de
    ld a, 21
    ld (hl), a
    
    ld de, 225   
    ld a, 1
    call print_number8bits    
    
    jp afterDisplayVelocity    
    
checkVelocityNegative      
    ld a, (x_velNeg)
    cp 0 
    jp z, displayZeroVelocity    
    ; change sign to neative on display
    ld de, 222
    ld hl, Display
    add hl, de
    ld a, 22
    ld (hl), a
    
    ld de, 225   
    ld a, 1
    call print_number8bits    
    jp afterDisplayVelocity
    
displayZeroVelocity
    ld de, 225
    ld a, 0
    call print_number8bits      
afterDisplayVelocity    

    ret
    
printStarsAndStripes
    ld de, starsAndStripes
    ld (tempStars), de    
    ld de, 69              
    ld (tempIndex), de
    
    ld b, 10       ;; this is how many rows there are
starStripePrintLoop    
    push bc
    
    ld de, (tempStars)
    ld bc, (tempIndex)
    
    call printstring
    
    ;;; increment the index to both screen memory and the stars and stripes memory
    ld hl, 15    ; the flag is 12 wide but we also have the $ff at then of the "line"
    ld bc, (tempStars)
    add hl, bc
    ld (tempStars),hl

    ld bc, (tempIndex)
    ld hl, 33
    add hl, bc
    ld (tempIndex), hl
    
    pop bc
    djnz starStripePrintLoop
    ret

printInstructions    
    ld de, instructionsText
    ld (tempStars), de    
    ld de, 69              
    ld (tempIndex), de
    
    ld b, 10
printInstructionsLoop    
    push bc
    
    ld de, (tempStars)
    ld bc, (tempIndex)
    
    call printstring
    
    ;;; increment the index to both screen memory and the stars and stripes memory
    ld hl, 15    ; the flag is 12 wide but we also have the $ff at then of the "line"
    ld bc, (tempStars)
    add hl, bc
    ld (tempStars),hl

    ld bc, (tempIndex)
    ld hl, 33
    add hl, bc
    ld (tempIndex), hl
    
    pop bc
    djnz printInstructionsLoop
    ret
    
printYouDiedAsHonorableHeros  
    ld de, diedHerosText
    ld (tempStars), de    
    ld de, 69              
    ld (tempIndex), de
    
    ld b, 10
DiedAsHonorableHerosLoop    
    push bc
    
    ld de, (tempStars)
    ld bc, (tempIndex)
    
    call printstring
    
    ;;; increment the index to both screen memory and the stars and stripes memory
    ld hl, 15    ; the flag is 12 wide but we also have the $ff at then of the "line"
    ld bc, (tempStars)
    add hl, bc
    ld (tempStars),hl

    ld bc, (tempIndex)
    ld hl, 33
    add hl, bc
    ld (tempIndex), hl
    
    pop bc
    djnz DiedAsHonorableHerosLoop
    ret

printStarsAndStripesCLEAR
    ld de, starsAndStripesClear
    ld (tempStars), de    
    ld de, 69              
    ld (tempIndex), de
    
    ld b, 10
clearStarStripePrintLoop    
    push bc
    
    ld de, (tempStars)
    ld bc, (tempIndex)
    
    call printstring
    
    ;;; increment the index to both screen memory and the stars and stripes memory
    ld hl, 15    ; the flag is 12 wide but we also have the $ff at then of the "line"
    ld bc, (tempStars)
    add hl, bc
    ld (tempStars),hl

    ld bc, (tempIndex)
    ld hl, 33
    add hl, bc
    ld (tempIndex), hl
    
    pop bc
    djnz clearStarStripePrintLoop
    ret
    
    

        
; this prints at to any offset (stored in bc) from the top of the screen Display, using string in de
printstring
    push de ; preserve de
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
    pop de  ; preserve de
    ret  
    
printGroundBottom  ; am duplicating code for now is eaiser to get the two end limits working for moonSurface1 and 2
    push bc
    push hl
    push de
; print ground is different to print string, it has a specific loop count and starts from a pointer to 
; a location within the ground memory so it can appear to scroll left or right (stored in bc)
    ld hl,Display
    add hl,bc	
    ld b, 21
printGround_loop
    ld a,(de)
    cp $fe
    jp z,resetPtrPrintGroundLeft    
    cp $ff
    jp z,resetPtrPrintGroundRight
    ld (hl),a
    inc hl
    inc de
    djnz printGround_loop
    jp printGround_loopEND
resetPtrPrintGroundLeft
    ld hl, (ptrToGround2)
    dec hl    
    ld (ptrToGround2), hl
    jp printGround_loopEND
resetPtrPrintGroundRight
    ld hl, (ptrToGround2)
    inc hl    
    ld (ptrToGround2), hl    
    
printGround_loopEND
    pop de
    pop hl        
    pop bc
    ret  
    
    
printGroundjustAbove  ; am duplicating code for now is eaiser to get the two end limits working for moonSurface1 and 2
    push bc
    push hl
    push de
; print ground is different to print string, it has a specific loop count and starts from a pointer to 
; a location within the ground memory so it can appear to scroll left or right (stored in bc)
    ld hl,Display
    add hl,bc	
    ld b, 21
printGround_loopJA
    ld a,(de)
    cp $fe
    jp z,resetPtrPrintGroundLeftJA    
    cp $ff
    jp z,resetPtrPrintGroundRightJA
    ld (hl),a
    inc hl
    inc de
    djnz printGround_loopJA
    jp printGround_loopENDJA
resetPtrPrintGroundLeftJA
    ld hl, (ptrToGround1)
    dec hl    
    ld (ptrToGround1), hl
    jp printGround_loopENDJA
resetPtrPrintGroundRightJA
    ld hl, (ptrToGround1)
    inc hl    
    ld (ptrToGround1), hl    
    
printGround_loopENDJA
    pop de
    pop hl        
    pop bc
    ret  
    
printGroundSurface0  ; am duplicating code for now is eaiser to get the two end limits working for moonSurface1 and 2
    push bc
    push hl
    push de
; print ground is different to print string, it has a specific loop count and starts from a pointer to 
; a location within the ground memory so it can appear to scroll left or right (stored in bc)
    ld hl,Display
    add hl,bc	
    ld b, 21
printGround_loopSurface0
    ld a,(de)
    cp $fe
    jp z,resetPtrPrintGroundLeftSurf0   
    cp $ff
    jp z,resetPtrPrintGroundRightSurf0
    ld (hl),a
    inc hl
    inc de
    djnz printGround_loopSurface0
    jp printGround_loopENDSurface0
resetPtrPrintGroundLeftSurf0
    ld hl, (ptrToGround0)
    dec hl    
    ld (ptrToGround0), hl
    jp printGround_loopENDSurface0
resetPtrPrintGroundRightSurf0
    ld hl, (ptrToGround1)
    inc hl    
    ld (ptrToGround0), hl    
    
printGround_loopENDSurface0
    pop de
    pop hl        
    pop bc
    ret      

print_number16bits    ; bc stores the 16bits, print b then c
    ld a, b
    call print_number8bits
    ld a, c
    inc de  ; move de over by 2
    inc de
    call print_number8bits
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

;check if TV synchro (FRAMES) happend
vsync	
	ld a,(FRAMES)
	ld c,a
sync
	ld a,(FRAMES)
	cp c
	jr z,sync
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
				DEFB _L,_U,_N,_A,0,_L,_A,_N,_D,_E,_R,0,0,0,0,0,0,0,0,0,0,7,3,3,3,3,3,3,3,3,3,132,$76  ;Line0
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_C,_O,_M,_P,0,_P,_R,_O,_G,133,$76          ;Line1
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_A,_C,_T,_Y,0,0,_0,_0,0,133,$76            ;Line2
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_V,_E,_R,_B,0,_N,_O,_U,_N,133,$76          ;Line3
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,_0,_0,0,0,0,_0,_0,0,133,$76              ;Line4
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,3,3,3,3,3,3,3,3,3,132,$76;Line5
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,21,_0,_0,_0,_0,_0,0,0,133,$76            ;Line6
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,3,3,3,3,3,3,3,3,3,132,$76;Line7
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,21,_0,_0,_0,_0,_0,0,0,133,$76            ;Line8
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,3,3,3,3,3,3,3,3,3,132,$76$76;Line9
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,21,_0,_0,_0,_0,_0,0,0,133,$76            ;Line10
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,3,3,3,3,3,3,3,3,3,132,$76;Line11
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0 , 0, 0, 0, 0, 0, 0, 0, 0,133,$76         ;Line12
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_F,_U,_E,_L, 0, _C, _O, _M, _M,133,$76         ;Line13
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0 ,_X,_X, 0, 0, 0, _O, _K, 0,133,$76;Line14
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,8,8,8,8,8,8,8,8,8,133,$76;Line15
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,8,8,8,8,8,8,8,8,8,133,$76;Line16
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_E,_S,_T,_I,_M,_A,_T,_E,_D,133,$76;Line17
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_D,_I,_S,_T,__,_T,_O,_G,_O,133,$76;Line18
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,8,8,8,8,8,8,8,8,8,133,$76;Line19
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,133,$76;Line20
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,_W,_I,_N,0,_C,_R,_A,_S,_H,133,$76;Line21                
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,8,8,8,8,8,8,8,8,8,133,$76;Line22
                DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,130,131,131,131,131,131,131,131,131,131,129,$76;Line23

                                                               
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
pad1
    DEFB 0    
firstTime    
    DEFB 0
pad2
    DEFB 0        
leftThrustOn    
    DEFB 0
rightThrustOn    
    DEFB 0
mainEngineOn
    DEFB 0
status_FuelQty    
    DEFB 0
countSinceEngineOnPosi
    DEFB 0   
countSinceEngineOnNeg    
    DEFB 0
;;;;; LEM state
x_velPosi
    DEFB 0  
x_velNeg 
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
distanceToLandingZonePos
    DEFB 0, 0
blankPartLine
    DEFB 5,5,5,5,5,5,5,5,5,$ff
    DEFB _F+128,_U+128,_E+128,_L+128,128,128,_L+128,_O+128,_W+128,$ff
fuelWarningTextInverse    
    DEFB _F+128,_U+128,_E+128,_L+128,128,128,_L+128,_O+128,_W+128,$ff
fuelWarningTextNormal
    DEFB _F,_U,_E,_L,0,0,_L,_O,_W,$ff
fuelWarningFlash
    DEFB 0
fuelWarningLatch
    DEFB 0
youCrashedText
    DEFB _Y,_O,_U,__,_C,_R,_A,_S,_H,_E,_D,__,6,6,6,6,6,6,6,6,6,$ff     ; padding to overwrite the title
goodLandingText
    DEFB _G,_O,_O,_D,__,_L,_A,_N,_D,_I,_N,_G,__,14,22,17,17,0,0,0,0,$ff  ; padding to overwrite the title
titleText
    DEFB _L,_U,_N,_A,0,_L,_A,_N,_D,_E,_R,0,0,9,10,9,10,9,10,9,10,$ff
prog64Text
    DEFB _L,_M,0,_A,_P,_P,_R,_O,_C,$ff
prog66Text
    DEFB _M,_A,_N,0,_L,_A,_N,_D,_I,$ff
clearRow    
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
ptrToGround0
    DEFB   0,0    
ptrToGround1
    DEFB   0,0
ptrToGround2
    DEFB   0,0

; starsAndStripes  ;; for testing position and draw loops, comment out later
   ; DEFB   28,29,30,31,32,33,34,35,36,37,38,39,40,41,$ff
   ; DEFB   42,43,44,45,46,47,48,49,50,51,52,53,54,55,$ff
   ; DEFB   0,23,0,23,0,23,8,8,8,8,8,8,8,8,$ff
   ; DEFB   23,0,23,0,23,0,0,0,0,0,0,0,0,0,$ff
   ; DEFB   0,23,0,23,0,23,8,8,8,8,8,8,8,8,$ff
   ; DEFB   23,0,23,0,23,0,0,0,0,0,0,0,0,8,$ff
   ; DEFB   0,23,0,23,0,23,8,8,8,8,8,8,8,8,$ff
   ; DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
   ; DEFB   8,8,8,8,8,8,8,8,8,8,8,8,8,8,$ff
   ; DEFB   0,0,0,0,0,0,0,0,0,0,0,40,41,42,$ff        
starsAndStripes
   DEFB   0,23,0,23,0,23,8,8,8,8,8,8,8,8,$ff
   DEFB   23,0,23,0,23,0,0,0,0,0,0,0,0,0,$ff
   DEFB   0,23,0,23,0,23,8,8,8,8,8,8,8,8,$ff
   DEFB   23,0,23,0,23,0,0,0,0,0,0,0,0,0,$ff
   DEFB   0,23,0,23,0,23,8,8,8,8,8,8,8,8,$ff
   DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
   DEFB   8,8,8,8,8,8,8,8,8,8,8,8,8,8,$ff
   DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
   DEFB   8,8,8,8,8,8,8,8,8,8,8,8,8,8,$ff
   DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff    
starsAndStripesClear
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff    
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
instructionsText
    DEFB   _K,_E,_Y,_S,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   0,_Z,0,_M,_O,_V,_E,0,_L,_E,_F,_T,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff    
    DEFB   0,_M,0,_M,_O,_V,_E,0,_R,_I,_G,_H,_T,0,$ff
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff    
    DEFB   0,_N,0,_F,_I,_R,_E,0,_M,_A,_I,_N,0,0,$ff
    DEFB   0,0,0,0,0,0,0,0,_E,_N,_G,_I,_N,_E,$ff    
    DEFB   0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
    DEFB   _B,_Y,0,_B,_Y,_T,_E,_F,_O,_R,_E,_V,_E,_R,$ff
    DEFB   16,_A,0,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,17,$ff
brakingBurnText 
    DEFB _B,_R,_A,_K,_E,0,_B,_U,_R,_N,0,_C,_O,_U,_N,_T,_D,_O,_W,_N, $ff
clearBrakingBurnText        
    DEFB  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$ff
diedHerosText
    DEFB   7,3,3,3,3,3,3,3,3,3,3,3,3,132,$ff
    DEFB   5,_T,_H,_E,0,_C,_R,_E,_W,0,_D,_I,_D,133,$ff
    DEFB   5,_N,_O,_T,0,_S,_U,_R,_V,_I,_V,_E,0,133,$ff
    DEFB   5,134,134,134,134,134,134,134,134,134,134,134,134,133,$ff
    DEFB   5,0,0,_R,_E,_S,_T,0,_I,_N,0,0,0,133,$ff    
    DEFB   5,0,0,0,_P,_E,_A,_C,_E,0,0,0,0,133,$ff
    DEFB   5,134,134,134,134,134,134,134,134,134,134,134,134,133,$ff
    DEFB   5,_T,_H,_E,0,_N,_A,_T,_I,_O,_N,0,0,133,$ff
    DEFB   5,_W,_I,_L,_L,0,_M,_O,_U,_R,_N,0,0,133,$ff
    DEFB   5,_F,_A,_L,_L,_E,_N,0,_H,_E,_R,_O,_S,133,$ff    
    DEFB   130,131,131,131,131,131,131,131,131,131,131,131,131,129,$ff    
tempStars
    DEFB 0,0
tempIndex
    DEFB 0,0
    
moonSurface0
    DEFB $ff,0,0,0,0,0,0,0,0,0,8,8,0,8,0,8,8,0,8,0,8
    DEFB 8,0,8,8,0,8,0,8,0,0,0,8,8,8,8,0,0,8,8,132,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,8,8,8,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,132,0,145,0,0,0,0,0,0,8
    DEFB 0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,8
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DEFB 8,8,8,0,0,0,0,0,0,0,0,  0,8,8,0,0,0,0,0,8,8  
    DEFB 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    DEFB 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    DEFB 8,8,6,10,134,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,$fe
moonSurface1
    DEFB $ff,0,0,0,0,0,0,0,0,0,8,8,0,8,0,8,8,0,8,0,8
    DEFB 8,0,8,8,0,8,0,8,0,0,0,8,8,8,8,0,0,8,8,132,8
    DEFB 8,8,8,8,0,0,0,0,0,0,8,0,0,0,0,0,0,8,8,8,8
    DEFB 8,8,8,0,0,0,0,0,0,0,0,8,8,8,8,8,8,8,8,8,8        
    DEFB 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    DEFB 8,8,8,8,0,0,0,0,0,0,8,0,0,0,0,0,0,8,8,8,8
    DEFB 8,8,8,8,0,0,0,0,0,0,8,0,0,0,0,0,0,8,8,8,8
    DEFB 8,8,8,8,8,8,0,8,8,8,0,  0,8,8,8,8,8,8,8,8,       
    DEFB 0,0,0,0,0,0,0,0,0,0,8,8,0,8,0,8,8,0,8,0,8
    DEFB 8,0,8,8,0,8,0,8,0,0,0,8,8,8,8,0,0,8,8,132,8
    DEFB 8,8,8,8,0,0,0,0,0,0,8,0,0,0,0,0,0,8,8,8,8
    DEFB 8,8,8,0,0,0,0,0,0,0,0,8,8,8,8,8,8,8,8,8,8        
    DEFB 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    DEFB 8,8,8,8,0,0,0,0,0,0,8,0,0,0,0,0,0,8,8,8,8
    DEFB 8,8,8,8,0,0,0,0,0,0,8,0,0,0,0,0,0,8,8,8,8
    DEFB 8,8,8,8,8,8,0,8,8,8,0,  0,8,8,8,8,8,8,8,8,8        
    DEFB 8,8,8,0,0,0,0,0,0,0,0,  0,8,8,8,0,0,0,0,0,8  
    DEFB 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    DEFB 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
    DEFB 8,8,8,144,8,145,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,$fe
moonSurface2
    DEFB $ff,136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131,136
    DEFB 136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131,132,131
    DEFB 131,131,136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131
    DEFB 131,131,136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131    
    DEFB 131,131,8,8,8,8,8,8,131,131,136,136,137,137,136,136,131,131,136,136,131    
    DEFB 131,131,136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131
    DEFB 131,131,136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131    
    DEFB 128,129,131,131,136,136,137,137,136,136,136,  8,137,137,137,128,129,131,131,136,136        
    DEFB 136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131,136
    DEFB 136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131,132,131
    DEFB 131,131,136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131
    DEFB 131,131,136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131    
    DEFB 131,131,8,8,8,8,8,8,131,131,136,136,137,137,136,136,131,131,136,136,131    
    DEFB 131,131,136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131
    DEFB 131,131,136,136,137,137,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131    
    DEFB 128,129,131,131,136,136,137,137,136,136,136,  8,137,137,137,128,129,131,131,136,136        
    DEFB 128,129,131,131,136,136,137,137,136,136,136,  8,137,137,137,177,177,177,177,177,136  ; landing zone 177 (inverse L)    
    DEFB 131,131,136,6,10,134,136,136,131,131,136,136,137,137,136,136,131,131,136,136,131,
    DEFB 131,131,136,144,8,145,136,136,131,_T,_H,_E,137,_E,_N,_D,131,_I,_S,136,_N,
    DEFB _I,_G,_H,6,3,134,136,136,_C,_E,_R,_T,_A,_I,_N,136,_D,_E,_A,_T,_H,136,$fe    
everyOther
    DEFB 0
starPositionAbsolute
    DEFB 0
prog64Countdown
    DEFB 0
scoreCrashes
    DEFB 255,255
scoreGoodLand
    DEFB 255,255
brakingBurnCountDown
    DEFB 0
commOKText
    DEFB 0,_O,_K,0,$ff
commFailText
    DEFB _D,_E,_A,_D,$ff   
VariablesEnd:   DEFB $80
BasicEnd: 
#END
