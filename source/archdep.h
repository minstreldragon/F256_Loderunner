BUGFIX :?= false
FULL_COPY_PROTECTION :?= false

.enc "high"
.cdef $00,$1f,$00
.cdef $20,$7f,$a0

;                   N
;              _____|_____
;             /  4  | 3   \
;            +.     |    . +
;           / 8 ' . |. '  7 \
;      W --.--------+--------.-- E
;           \ 6 . ' |'  . 5 /
;            +'  2  | 1   '+
;             \_____|_____/
;                   |
;                   S


; key codes see:
; https://sta.c64.org/cbm64kbdcode1.html

KEY_CODE_RETURN = $01
KEY_CODE_LEFT_RIGHT = $02
KEY_CODE_UP_DOWN = $07
KEY_CODE_RUN_STOP = $3f

KEY_CODE_PLUS  = $2b
KEY_CODE_MINUS = $2d

KEY_CODE_0 = $30
KEY_CODE_1 = $31
KEY_CODE_2 = $32
KEY_CODE_3 = $33
KEY_CODE_4 = $34
KEY_CODE_5 = $35
KEY_CODE_6 = $36
KEY_CODE_7 = $37
KEY_CODE_8 = $37
KEY_CODE_9 = $39

KEY_CODE_A = $41
KEY_CODE_B = $42
KEY_CODE_C = $43
KEY_CODE_D = $44
KEY_CODE_E = $45
KEY_CODE_F = $46
KEY_CODE_I = $49
KEY_CODE_J = $4a
KEY_CODE_K = $4b
KEY_CODE_L = $4c
KEY_CODE_M = $4d
KEY_CODE_N = $4e
KEY_CODE_O = $4f
KEY_CODE_P = $50
KEY_CODE_Q = $51
KEY_CODE_R = $52
KEY_CODE_S = $53
KEY_CODE_T = $54
KEY_CODE_U = $55
KEY_CODE_X = $58
KEY_CODE_Y = $59
KEY_CODE_Z = $60

.comment
KEY_CODE_PLUS  = $28
KEY_CODE_MINUS = $2b

KEY_CODE_0 = $23
KEY_CODE_1 = $38
KEY_CODE_2 = $3b
KEY_CODE_3 = $08
KEY_CODE_4 = $0b
KEY_CODE_5 = $10
KEY_CODE_6 = $13
KEY_CODE_7 = $18
KEY_CODE_8 = $1b
KEY_CODE_9 = $20

KEY_CODE_A = $0a
KEY_CODE_B = $1c
KEY_CODE_C = $14
KEY_CODE_D = $12
KEY_CODE_E = $0e
KEY_CODE_F = $15
KEY_CODE_I = $21
KEY_CODE_J = $22
KEY_CODE_K = $25
KEY_CODE_L = $2a
KEY_CODE_M = $24
KEY_CODE_N = $27
KEY_CODE_O = $26
KEY_CODE_P = $29
KEY_CODE_Q = $3e
KEY_CODE_R = $11
KEY_CODE_S = $0d
KEY_CODE_T = $16
KEY_CODE_U = $1e
KEY_CODE_X = $17
KEY_CODE_Y = $19
KEY_CODE_Z = $0c
.endcomment

KEY_CODE_NONE = $40

KEY_CODE_CTRL = $80


.comment
        * = $01

R6510                                   ; processor port ($01)
        .byte ?

        .byte ?

zpPlayerX                               ; player, X position on board
        .byte ?                         ; $03

zpPlayerY                               ; player, Y position on board
        .byte ?                         ; $04

zpPlayerStepX                           ; player, X position (fine)
        .byte ?                         ; $05

zpPlayerStepY                           ; player, Y position (fine)
        .byte ?                         ; $06

zpPlayerAnimPhase                       ; player animation phase
        .byte ?                         ; $07

zpPlayerOrientation                     ; $ff: player facing left, $01: facing right
        .byte ?                         ; $08

zpBoardActionPtr
        .word ?                         ; $09/$0a

zpBoardLayoutPtr
        .word ?                         ; $0b/$0c

zpWideShapePtr
zpSrcPtr
zpUnpackedDataPtr                       ; pointer to unpacked RLE data
        .word ?                         ; $0d/$0e

zpRleDataPtr                            ; pointer to RLE packed data
zpMemPtr
zpBmpPtr
zpBmpPtr0                               ; pointer into bitmap 0
        .word ?                         ; $0f/$10

zpBmpPtr1                               ; pointer into bitmap 1
        .word ?                         ; $11/$12

zpPrintDataPtr                          ; $13/$14
        .word ?

zpEnemyX                                ; current enemy, X position on board
        .byte ?                         ; $15

zpEnemyY                                ; current enemy, Y position on board
        .byte ?                         ; $16

zpEnemyAnimPhase                        ; enemy animation phase
        .byte ?                         ; $17

zpEnemyOrientation                      ; $ff: enemy facing left, $01: facing right
        .byte ?                         ; $18

zpEnemyActionCtr                        ; current enemy, special action counter ($19)
        .byte ?                         ; <0: remaining time until enemy drops gold (increasing)
                                        ; >0: remaining time enemy is trapped (decreasing)

zpEnemyStepX                            ; current enemy, X position (fine)
        .byte ?                         ; $1a

zpEnemyStepY                            ; current enemy, Y position (fine)
        .byte ?                         ; $1b

        .byte ?
        .byte ?

zpBmpLinePtrLb                          ; bitmap line pointer (lb)
zpCombinedNibbles
zpJoystickPort
zpNibbleSelect                          ; even: extract low nibble, odd: high nibble
zpSrcCol
zpTmpProbeX                             ; temporary variable enemy probe X
zpTmpProbeY                             ; temporary variable enemy probe Y
zpRleCounter                            ; RLE uncompressor counter
zpDriveStatus                           ; combined drive status: '0': OK
zpCopyProtByteOffset                    ; copy protection check byte offset
        .byte ?                         ; $1e

zpBmpLinePtrHb                          ; bitmap line pointer (hb)
zpRleValue                              ; current RLE run value
        .byte ?                         ; $1f

zpEnemyTestY                            ; enemy test position Y
zpPixelPosY                             ; pixel position Y
zpBmpByteOffset                         ; bitmap byte offset (in steps of 8 within same line)
        .byte ?                         ; $20

zpPixelOffsetXlb                        ; pixel offset x in bitmap, low byte
        .byte ?                         ; $21

zpShapeRowIter                          ; iterator over rows in shape
zpHighScoreIter
        .byte ?                         ; $22

zpEditorNewShape                        ; user selected shape (0-9)
zpEditorUserKey                         ; key pressed by user
zpShapeId                               ; current shape ID
zpLowNibble                             ; low nibble for combining packed byte
zpGameInputKey                          ; input key during game play
zpTmpPixelPosX                          ; temporary storage for pixel position x
        .byte ?                         ; $23

zpBitmapPage                            ; $20 (Bitmap0), $40 (Bitmap1)
        .byte ?                         ; $24

zpSignatureChecksum                     ; 0: signature found, else: not found
zpBmpAndMask                            ; 2-byte and mask for drawing to bitmap
        .word ?                         ; $25/$26

zpPlayerEnemyCollision                  ; 0: no collision between player and enemy; 1: collision detected
        .byte ?                         ; $27

zpEnemyXOrigin
zpHighScoreEntryNumber                  ; number of current high score entry (1-10)
        .byte ?                         ; $28

zpEnemyYOrigin
zpHighScoreEntryOffset                  ; offset of current high score entry
        .byte ?                         ; $29

zpEnemyPlayerRowX                       ; enemy path on player's row position X
        .byte ?                         ; $2a

zpEnemyMoveDir                          ; next move for enemy (pathfinder algorithm)
        .byte ?                         ; $2b

zpEnemyPositionScore                    ; score for projected enemy position (pathfinder algorithm)
        .byte ?                         ; $2c

zpEnemyMoveLeftPosX                     ; minimum reachable X position on current row
        .byte ?                         ; $2d

zpEnemyMoveRightPosX                    ; maximum reachable X position on current row
        .byte ?                         ; $2e

zpLevelCompleteScoreCounter             ; iterator for increasing score (total: 1500 points)
zpEnemyProbeBestY                       ; best Y position probed so far
        .byte ?                         ; $2f

zpEnemyProbeX                           ; enemy path probe position X
        .byte ?                         ; $30

zpEnemyProbeY                           ; enemy path probe position Y
        .byte ?                         ; $31


zpEnemyMoveCycleTbl                     ; table of enemy move cycles (length: 3)
zpEnemyMoveCycle0                       ; enemy move cycle 0
        .byte ?                         ; $32

zpEnemyMoveCycle1                       ; enemy move cycle 1
        .byte ?                         ; $33

zpEnemyMoveCycle2                       ; enemy move cycle 2
        .byte ?                         ; $34

zpEnemyMoveCycleCur                     ; current enemy move cycle
        .byte ?                         ; $35

zpEnemyMoveCycleIdx                     ; index into current enemy move cycle (0-2)
        .byte ?                         ; $36

zpCircleY                               ; circle algorithm: y
        .word ?                         ; $37/$38

zpNewHighScoreEntryId
zpCircleX                               ; circle algorithm: x (word)
        .byte ?                         ; $39
        .byte ?                         ; $3a
zpIrisDiameter                          ; iris: current diameter
zpCircleA                               ; circle algorithm: a
        .word ?                         ; $3b/$3c

zpIrisRadius                            ; iris: current radius
        .byte ?                         ; $3d

        .byte ?                         ; $3e

zpIrisXPosHbTemp                        ; iris: temporary variable high byte of x pos
zpCircleTmpTimes4
        .byte ?                         ; $3f

        * = $41

zpShapeDblPixelShift                    ; double-pixel shift applied to shape for printing
        .byte ?                         ; $41

zpIrisAnimDirection                     ; 0: closing, 1: opening
        .byte ?                         ; $42

zpIrisPosYOct34                         ; Y position for octants 3/4
        .byte ?                         ; $43

zpIrisPosYOct78                         ; Y position for octants 7/8
        .byte ?                         ; $44

zpIrisPosYOct56                         ; Y position for octants 5/6
        .byte ?                         ; $45

zpIrisPosYOct12                         ; Y position for octants 1/2
        .byte ?                         ; $46

zpIrisColXOct68                         ; X position (column) for octants 6/8
        .byte ?                         ; $47

zpIrisColXOct24                         ; X position (column) for octants 2/4
        .byte ?                         ; $48

zpIrisColXOct13                         ; X position (column) for octants 1/3
        .byte ?                         ; $49

zpIrisColXOct57                         ; X position (column) for octants 5/7
        .byte ?                         ; $4a

zpIrisOffXOct68                         ; X position (offset) for octants 6/8
        .byte ?                         ; $4b

zpIrisOffXOct24                         ; X position (offset) for octants 2/4
        .byte ?                         ; $4c

zpIrisOffXOct13                         ; X position (offset) for octants 1/3
        .byte ?                         ; $4d

zpIrisOffXOct57                         ; X position (offset) for octants 5/7
        .byte ?                         ; $4e

        * = $4f

zpCursorCol                             ; $4f
        .byte ?

zpCursorRow
zpBitmapLine
zpEnemyRespawnRow                       ; row for respawning enemy (local)
zpLongDelayCtr                          ; long delay counter
        .byte ?                         ; $50

zpBitmapPage2                           ; active bitmap page for printing
        .byte ?                         ; $51

zpHoleIdx                               ; current hole index
        .byte ?                         ; $52

zpPackedDataIdx                         ; source index into packed board data
        .byte ?                         ; $53

zpHoleDigAnimCtr                        ; animation counter for a hole being drilled
        .byte ?                         ; $54

zpDemoPtr                               ; pointer into demo game sequence
        .word ?                         ; $55

zpDemoCommandInput                      ; direction code during game demo
        .byte ?                         ; $57

zpDemoRepeatCounter                     ; command repeat counter during game demo
        .byte ?                         ; $58

zpDemoUnused                            ; variable unused (initialized to $01)
        .byte ?                         ; $59

zpEditorBoardUnchanged                  ; 0: board unchanged, 1: changes pending
        .byte ?                         ; $5a

zpShiftedShapeHopper                    ; 33 bytes of shifted shape data
        .fill 33                        ; $5b

zpSoundEffectPitch                      ; current pitch of sound effect
        .byte ?                         ; $7c

zpJingleDataPtr
        .word ?                         ; $7d

exitLaddersX = $1200
notAllExitLaddersDrawn = exitLaddersX   ; 0: all exit ladders drawn, 0xff: couldn't draw one or more ladder segments

exitLaddersY = $1230

enemiesX = $1260

enemiesY = $1268

enemiesActionCtr = $1270                ; special action counter:
                                        ; <0: remaining time until enemy drops gold (increasing)
                                        ; >0: remaining time enemy is trapped (decreasing)
enemiesStepX = $1278

enemiesStepY = $1280

enemiesAnimPhase = $1288                ; enemy animation phase

enemiesOrientation = $1290              ; $ff: enemy facing left, $01: facing right

enemiesRespawnCtr = $1298               ; time left until enemy respawns

holesPosX = $12a0

holesPosY = $12c0

holesOpenCtr = $12e0                    ; array: time until hole closes

        * = $1300

boardRequiresFullInit                   ; 0: board has been initialized before, 1: board requires full init
        .byte ?                         ; $1300

numExitLadders                          ; number of exit ladders
        .byte ?                         ; $1301

soundFxFallingPitch                     ; sound effect player falling, current pitch
        .byte ?                         ; $1302

        .byte ?

irisPhase                               ; iris animation phase: 0: animate open only, $ff: animate both
        .byte ?                         ; $1304

displayedLevel                          ; current level (display)
        .byte ?                         ; $1305

gameMode                                ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        .byte ?                         ; $1306

digitHigh                               ; converted number, highest digit
        .byte ?                         ; $1307

digitMid                                ; converted number, middle digit
                                        ; split BCD digit, mid nibble
        .byte ?                         ; $1308

digitLow                                ; converted number, low digit
                                        ; split BCD digit, low nibble
        .byte ?                         ; $1309

score                                   ; current score
        .byte ?                         ; $130a (low)
        .byte ?                         ; $130b (mid)
        .byte ?                         ; $130c (high)
        .byte ?                         ; $130d (highest)

goldLeft                                ; >0: remaining gold, 0: no gold left, $ff: hidden ladders visible
        .byte ?                         ; $130e

controllerMode                          ; 'J': Joystick, 'K': Keyboard
        .byte ?                         ; $130f

currentLevel                            ; current level (0-based)
        .byte ?                         ; $1310

enemySpeed                              ; enemy speed (0-10)
        .byte ?                         ; $1311

lives                                   ; number of lives
        .byte ?                         ; $1312

volume                                  ; sound playback volume - 0: off, $ff: on
        .byte ?                         ; $1313

playerAlive                             ; 0: player dead, 1: player alive
        .byte ?                         ; $1314

playerNotFalling                        ; 0: player is falling, >0: not falling
        .byte ?                         ; $1315

playerDiggingState                      ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        .byte ?                         ; $1316

soundFxPlayerDigging                    ; sound effect player digging, current pitch
        .byte ?                         ; $1317

allowHighScoreEntry                     ; 0: player cheated, no high score entry, 1: did not cheat
        .byte ?                         ; $1318

inputVertical
        .byte ?                         ; $1319

inputHorizontal
        .byte ?                         ; $131a

gameDelay                               ; 3: fastest, 8: slowest
        .byte ?                         ; $131b

numEnemies                              ; total number of enemies in current level
        .byte ?                         ; $131c

playerNoGoldPickedUp                    ; 0: gold pick up in process, 1: no gold picked up
        .byte ?                         ; $131d

enemyRespawnCol                         ; column for respawning enemy
        .byte ?                         ; $131e

enemyIndex                              ; current enemy, index
        .byte ?                         ; $131f

enemyTrappedDuration                    ; duration an enemy is trapped in a hole
        .byte ?                         ; $1320

irisAnimationOn                         ; $ff: iris animation on, $00: turned off
        .byte ?                         ; $1321

flashCursorCtr
        .word ?                         ; $1322/$1323

charAtCursor                            ; current character at cursor
        .byte ?                         ; $1324

bitmapOffsetX                           ; horizontal bitmap offset of shape
        .word ?                         ; $1325/$1326

rowIterator                             ; row iterator for init routine (25..0)
        .byte ?                         ; $1327

keyboardCode                            ; keyboard matrix code (no key: $00)
        .byte ?                         ; $1328

previousKeyboardCode                    ; previous keyboard code (prevent repeat)
        .byte ?                         ; $1329

soundFxPlayerDying                      ; sound effect player dying, current pitch
        .byte ?                         ; $132a

        .byte ?                         ; $132b

skipShapeConversion                     ; 0: do not skip shape conversion, >0: skip shape conversion ($1312)
        .byte ?                         ; $132c

irqFrameCounter                         ; free running counter increased with each IRQ
        .byte ?                         ; $132d

tunePlayOffset                          ; offset: playback position in current tune ($6d)
        .byte ?                         ; $132e

tuneDataEnd                             ; offset: end of current tune ($6e)
        .byte ?                         ; $132f

        .byte ?                         ; $1330

jingleGoldMinimumPitchDelta             ; minimum delta between consecutive pitch values
        .byte ?                         ; $1331

jingleGoldComplete                      ; jingle ID: player collected all gold
        .byte ?                         ; $1332

jingleGoldCompleteTranspose             ; transpose value for jingle
        .byte ?                         ; $1333

        .byte ?                         ; $1334

jingleGoldBeamPosPrev                   ; previous raster beam position for gold jingle
        .byte ?                         ; $1335

jingleGoldBeamPosCur                    ; current raster beam position for gold jingle
        .byte ?                         ; $1336

ctrlRegVoice2                           ; control reg for SID voice 2
        .byte ?                         ; $1337

ctrlRegVoice3                           ; control reg for SID voice 3
        .byte ?                         ; $1338

digDirection                            ; $ff: dig forward, $00: dig behind runner
        .byte ?                         ; $1339

        .byte ?

reloadBoard                             ; force loading board if != currentLevel
        .byte ?                         ; $133b



;======================== Cartridge Version ===========================

BUGFIX :?= false

;lr      .encode
;lower   .encode
;        .tdef "#",$01
;        .cdef "09",$3d                          ; numbers 0-9
;        .cdef "AI",$50                          ; Map A-Z to start from $50
;        .cdef "LP",$59                          ; (with gaps, not all letters are defined)
;        .cdef "RW",$5e
;        .edef "\n",$fe
;        .tdef " ",$ff
;        .endencode

.enc "lr"
;        .tdef "#",$01
.cdef "##",$01
.cdef "09",$3d                          ; numbers 0-9
.cdef "AI",$50                          ; Map A-Z to start from $50
.cdef "LP",$59                          ; (with gaps, not all letters are defined)
.cdef "RW",$5e
;        .edef "\n",$fe
.cdef "()",$64
.edef "\n",$fe
.cdef "  ",$ff
;        .endencode

.enc "crtstart"
.cdef $00,$3f,$00
.cdef $40,$7f,$c0

.enc "high"
.cdef $00,$1f,$00
.cdef $20,$7f,$a0
.endcomment

; CHAR_0 = $3d                          ; cartridge version
CHAR_0 = $3b                            ; disk version

CHAR_NEWLINE = $fe
CHAR_SPACE = $ff

SHAPE_0 = $2c
; SHAPE_A = $36                         ; cartridge version
SHAPE_A = $45                           ; disk version

PETSCII_0 = $30                         ; PETSCII code for digit 0
PETSCII_J = $ca
PETSCII_K = $cb

COL_BLACK = $00
COL_TRANSPARENT = $00
COL_WHITE = $01
COL_RED = $02
COL_CYAN = $03
COL_PURPLE = $04
COL_GREEN = $05
COL_BLUE = $06
COL_YELLOW = $07
COL_ORANGE = $08
COL_BROWN = $09
COL_LIGHTRED = $0a
COL_DARKGREY = $0b
COL_MIDGREY = $0c
COL_LIGHTGREEN = $0d
COL_LIGHTBLUE = $0e
COL_LIGHTGREY = $0f
COL_SOLIDBLACK = $10
.comment

        * = $01
R6510                                   ; processor port ($01)
        .byte ?

zpPlayerX                               ; player, X position on board ($02)
        .byte ?

zpPlayerY                               ; player, Y position on board ($03)
        .byte ?

zpPlayerStepX                           ; player, X position (fine) ($04)
        .byte ?

zpPlayerStepY                           ; player, Y position (fine) ($05)
        .byte ?

zpPlayerAnimPhase                       ; $06 - player animation phase
        .byte ?

zpPlayerOrientation                     ; $07 - $ff: player facing left, $01: facing right
        .byte ?

zpBoardActionPtr                        ; $08/$09
        .word ?

zpBoardLayoutPtr                        ; $0a/$0b
        .word ?

zpWideShapePtr                          ; $0c/$0d
zpDstPtr
zpSrcPtr
        .word ?

zpMemPtr                                ; $0e/$0f
zpCursorPtr
zpBmpPtr
zpBmpPtr0                               ; pointer into bitmap 0
zpShapeShiftPtrLeft
        .word ?

zpShapeDataRightPtr                     ; $10/$11
zpBmpPtr1                               ; pointer into bitmap 1
zpShapeShiftPtrRight
        .word ?

zpPrintDataPtr                          ; $12/$13
        .word ?

zpEnemyX                                ; current enemy, X position on board ($14)
        .byte ?

zpEnemyY                                ; current enemy, Y position on board ($15)
        .byte ?

zpEnemyAnimPhase                        ; enemy animation phase ($16)
        .byte ?

zpEnemyOrientation                      ; $ff: enemy facing left, $01: facing right ($17)
        .byte ?

zpEnemyActionCtr                        ; current enemy, special action counter ($18)
        .byte ?                         ; <0: remaining time until enemy drops gold (increasing)
                                        ; >0: remaining time enemy is trapped (decreasing)

zpEnemyStepX                            ; current enemy, X position (fine) ($19)
        .byte ?

zpEnemyStepY                            ; current enemy, Y position (fine) ($1a)
        .byte ?

zpBmpLinePtrLb                          ; bitmap line pointer (lb) ($1b)
zpShapeIdx                              ; shape index ($1b)
zpCombinedNibbles
zpJoystickPort
zpNibbleSelect                          ; even: extract low nibble, odd: high nibble
zpByteShiftedHb                         ; shifted byte value, lb
zpMultiColorValues                      ; multi color values for screen RAM
zpSrcCol                                ; source column
zpTmpProbeX                             ; temporary variable enemy probe X
zpTmpProbeY                             ; temporary variable enemy probe Y
        .byte ?

zpBmpLinePtrHb                          ; bitmap line pointer (hb) ($1c)
zpShapeRightBits                        ; rightmost 2 shape bits (packed) ($1c)
zpShapeShiftTblPage                     ; current shapeShiftTable page (setup)
        .byte ?

zpRightBitsCounter                      ; rightmost 2 bits counter ($1d)
        .byte ?

        .byte ?                         ; ($1e)

zpEnemyTestY                            ; enemy test position Y ($1f)
zpPixelPosY                             ; pixel position Y
zpBmpByteOffset                         ; bitmap byte offset (in steps of 8 within same line)
        .byte ?


zpGameOverFrameIdx                      ; "GAME OVER" animation frame index
zpPixelOffsetXlb                        ; pixel offset x in bitmap, low byte
        .byte ?                         ; ($20)

zpShapeRowIter                          ; iterator over rows in shape ($21)
zpGameOverStopFrameIdx                  ; "GAME OVER" animation stop frame index
        .byte ?

zpRleValue                              ; current RLE run value ($22)
zpEditorNewShape                        ; user selected shape (0-9)
zpEditorUserKey                         ; key pressed by user
zpShapeId                               ; current shape ID
zpLowNibble                             ; low nibble for combining packed byte
zpGameInputKey                          ; input key during game play
zpTmpPixelPosX                          ; temporary storage for pixel position x

        .byte ?

zpBitmapPage                            ; $20 (Bitmap0), $40 (Bitmap1) ($23)
        .byte ?                         ;

zpSaveStartAddrPtr                      ; $24/$25 - pointer to start address for SAVE
zpBmpAndMask                            ; 2-byte and mask for drawing to bitmap
        .word ?

zpPlayerEnemyCollision                  ; 0: no collision between player and enemy; 1: collision detected
        .byte ?                         ; ($26)

zpEnemyXOrigin                          ; $27
        .byte ?

zpEnemyYOrigin                          ; $28
        .byte ?

zpEnemyPlayerRowX                       ; $28 - enemy path on player's row position X
        .byte ?

zpEnemyMoveDir                          ; $2a - next move for enemy (pathfinder algorithm)
        .byte ?

zpEnemyPositionScore                    ; $2b - score for projected enemy position (pathfinder algorithm)
        .byte ?

zpEnemyMoveLeftPosX                     ; $2c - minimum reachable X position on current row
        .byte ?

zpEnemyMoveRightPosX                    ; $2d - maximum reachable X position on current row
        .byte ?

zpLevelCompleteScoreCounter             ; $2e
zpEnemyProbeBestY                       ; best Y position probed so far
        .byte ?

zpEnemyProbeX                           ; $2f - enemy path probe position X
        .byte ?

zpEnemyProbeY                           ; $30 - enemy path probe position Y
        .byte ?

zpEnemyMoveCycleTbl                     ; $31 - table of enemy move cycles (length: 3)
zpEnemyMoveCycle0                       ; $31 - enemy move cycle 0
        .byte ?

zpEnemyMoveCycle1                       ; $32 - enemy move cycle 1
        .byte ?

zpEnemyMoveCycle2                       ; $33 - enemy move cycle 2
        .byte ?

zpEnemyMoveCycleCur                     ; $34 - current enemy move cycle
        .byte ?

zpEnemyMoveCycleIdx                     ; $35 - index into current enemy move cycle (0-2)
        .byte ?

zpShapeDblPixelShift                    ; $36 - double-pixel shift applied to shape for printing
        .byte ?

zpCursorCol                             ; $37
        .byte ?

zpCursorRow                             ; $38
zpBitmapLine
zpEnemyRespawnRow                       ; $38 - row for respawning enemy (local)
zpLongDelayCtr                          ; $38 - long delay counter
        .byte ?

zpBitmapPage2                           ; $39 - active bitmap page for printing
        .byte ?

zpHoleIdx                               ; $3a - current hole index
        .byte ?

zpPackedDataIdx                         ; $3b - source index into packed board data
zpSourceIdx
        .byte ?

zpHoleDigAnimCtr                        ; $3c - animation counter for a hole being drilled
        .byte ?

zpDemoPtr                               ; $3d/$3e - pointer into demo game sequence
        .word ?

zpDemoCommandInput                      ; $3f - direction code during game demo
        .byte ?

zpDemoRepeatCounter                     ; $40 - command repeat counter during game demo
        .byte ?

zpDemoUnused                            ; $41 - variable unused (initialized to $01)
        .byte ?

zpEditorBoardUnchanged                  ; $42 - 0: board unchanged, 1: changes pending
        .byte ?

zpShiftedShapeHopper                    ; $43 - 33 bytes of shifted shape data
        .fill 33

zpSoundEffectPitch                      ; $64 - current pitch of sound effect
        .byte ?

zpJingleDataPtr                         ; $65
        .word ?

zpEnemyRespawnCol                       ; $67 - column for respawning enemy
        .byte ?

zpEnemyTrappedDuration                  ; $68 - duration an enemy is trapped in a hole
        .byte ?

zpCursorFlashTimer                      ; busy wait timer for flashing cursor ($69/$6a)
        .word ?

zpKeyboardCode                          ; $6b - keyboard matrix code (no key: $00)
        .byte ?

zpIrqFrameCounter                       ; $6c - free running counter increased with each IRQ
        .byte ?

zpTunePlayOffset                        ; offset: playback position in current tune ($6d)
        .byte ?

zpTuneDataEnd                           ; offset: end of current tune ($6e)
        .byte ?

zpCharAtCursor                          ; current character at cursor ($6f)
        .byte ?

zpRleCounter                            ; RLE uncompressor counter ($70)
zpRowIterator                           ; row iterator for init routine (25..0)
        .byte ?

zpPlayerAlive                           ; $71 - 0: player dead, 1: player alive
        .byte ?

zpControllerMode                        ; $72 - 'J': Joystick, 'K': Keyboard
        .byte ?

zpLevel                                 ; $73 - current level (0-based)
        .byte ?

zpDigitHigh                             ; $74
        .byte ?

zpDigitMid                              ; $75
        .byte ?

zpDigitLow                              ; $76
        .byte ?

zpScore                                 ; $77
        .byte ?
        .byte ?
        .byte ?
        .byte ?

zpEnemyIndex                            ; current enemy, index ($7b)
        .byte ?

zpEnemiesLeft                           ; $7c
zpNumEnemies                            ; $7c - total number of enemies in current level
        .byte ?

zpInputVertical                         ; $7d
        .byte ?

zpInputHorizontal                       ; $7e
        .byte ?

COLOR = $0286                           ; current foreground color for text
MODE = $0291                            ; flag: enable/disable changing character sets with SHIFT/C= keypress

exitLaddersX = $1200
notAllExitLaddersDrawn = exitLaddersX   ; 0: all exit ladders drawn, 0xff: couldn't draw one or more ladder segments

exitLaddersY = $1230

enemiesX = $1260

enemiesY = $1268

enemiesActionCtr = $1270                ; special action counter:
                                        ; <0: remaining time until enemy drops gold (increasing)
                                        ; >0: remaining time enemy is trapped (decreasing)
enemiesStepX = $1278

enemiesStepY = $1280

enemiesAnimPhase = $1288                ; enemy animation phase

enemiesOrientation = $1290              ; $ff: enemy facing left, $01: facing right

enemiesRespawnCtr = $1298               ; time left until enemy respawns

holesPosX = $12a0

holesPosY = $12c0

holesOpenCtr = $12e0                    ; array: time until hole closes

        * = $1300

boardRequiresFullInit                   ; 0: board has been initialized before, 1: board requires full init
        .byte ?

numExitLadders                          ; number of exit ladders ($1301)
        .byte ?

soundFxFallingPitch                     ; $1302 - sound effect player falling, current pitch
        .byte ?

level                                   ; $1303
        .byte ?

gameMode                                ; $1304 - 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        .byte ?

goldLeft                                ; $1305 - 1-n: remaining gold, 0: no gold left, $ff: hidden ladders visible
        .byte ?

enemySpeed                              ; $1306 - enemy speed (0-10)
        .byte ?

lives                                   ; $1307
        .byte ?

volume                                  ; sound playback volume - 0: off, $ff: on ($1308)
        .byte ?

playerNotFalling                        ; $1309 - 0: player is falling, >0: not falling
        .byte ?

playerDiggingState                      ; $130a - 0: player not digging, 1: digging right, -1 ($ff): digging left
        .byte ?

soundFxPlayerDigging                    ; $130b - sound effect player digging, current pitch
        .byte ?

gameDelay                               ; $130c - 3: fastest, 8: slowest
        .byte ?

playerNoGoldPickedUp                    ; $130d - 0: gold pick up in process, 1: no gold picked up
        .byte ?

bitmapOffsetX                           ; $130e/$130f - horizontal bitmap offset of shape
        .word ?

previousKeyboardCode                    ; $1310 - previous keyboard code (prevent repeat)
        .byte ?

soundFxPlayerDying                      ; $1311 - sound effect player dying, current pitch
        .byte ?

skipShapeConversion                     ; 0: do not skip shape conversion, >0: skip shape conversion ($1312)
        .byte ?

jingleGoldComplete                      ; jingle ID: player collected all gold ($1313)
        .byte ?

jingleGoldCompleteTranspose             ; transpose value for jingle ($1314)
        .byte ?

ctrlRegVoice2                           ; control reg for SID voice 2 ($1315)
        .byte ?

ctrlRegVoice3                           ; control reg for SID voice 3 ($1316)
        .byte ?

digDirection                            ; $ff: dig forward, $00: dig behind runner ($1317)
        .byte ?

textOutputMode                          ; $00: game internal (shapes), >0: ROM charset-based ($1318)
        .byte ?

inCartBoardsActive                      ; $ff: in-cart boards active, $00: user boards active ($1319)
        .byte ?

gameDelayUnused                         ; contains the initial value of gameDelay. Not actually used in the game ($131a)
        .byte ?


boardAction = $0800                     ; current board with replacments for player, enemies, holes

boardLayout = $0a00                     ; current board w/o replacments for player, enemies, holes

spriteDefinitions = $0c00               ; sprite definitions for player and enemies

boardPacked = $1000                     ; current board, packed by combining two shapes in one byte

highScoreData = $1100                   ; high score data (disk only)

levelSetPacked = $6000                  ; uncompressed, but packed level set (several boards) (page aligned!)

;======================== Cartridge Version ===========================
Shapes = $1800                          ; expanded shape data:
                                        ;   row 0, col 0 (8 bit) for each shape
                                        ;   row 0, col 1 (2 bit) for each shape
                                        ;   row 1, col 0 (8 bit) for each shape
                                        ;   ...

shapeShiftTables = $c000                ; tables for shifting shapes into position
shapeShiftTablesEnd = $c800             ; end of shapeShiftTable

tuneDataNoteLength = $c000
tuneDataVoice0 = $c100
tuneDataVoice1 = $c200
tuneDataVolume = $c300

highScoreDataBackup = $c800
highScoreDataUpdated = $c900
.endcomment

LSTX = $c5                              ; Matrix code of last key pressed ($40: none pressed)
SHFLAG = $028d                          ; Shift Flag (bit 0: SHIFT, bit 1: C=, bit 2: CTRL)

;;; ScreenRAM = $0400
;;; SpritePtrs = $07f8                      ; sprite pointers location
TblBmpLinePtrLb = $0e00                 ; Table: line to bitmap pointer (lb)
TblBmpLinePtrHb = $0f00                 ; Table: line to bitmap pointer (hb)
;;;TblScreenLinePtrLb = $1380              ; Table: line to screen pointer (lb)
;;;TblScreenLinePtrHb = $1399              ; Table: line to screen pointer (hb)
Bitmap0 = $2000
Bitmap1 = $4000
;;;ColorRAM = $d800

C64KernelStandardIrq = $ea31


NDX = $c6
KEYD = $277
PALNTS = $2a6
CINV = $314
NMINV = $0318

VicSprite0XPos = $d000
VicSprite0YPos = $d001
VicSprite1XPos = $d002
VicSprite1YPos = $d003
VicSprite2XPos = $d004
VicSprite2YPos = $d005
VicSprite3XPos = $d006
VicSprite3YPos = $d007
VicSprite4XPos = $d008
VicSprite4YPos = $d009
VicSprite5XPos = $d00a
VicSprite5YPos = $d00b
VicSprite6XPos = $d00c
VicSprite6YPos = $d00d
VicSprite7XPos = $d00e
VicSprite7YPos = $d00f
VicSpritesXPosMsb = $d010
VicScreenCtrlReg1 = $d011
VicRasterValue = $d012
VicLightPenLatchX = $d013
VicLightPenLatchY = $d014
VicSpriteEnable = $d015
VicScreenCtrlReg2 = $d016
VicSpriteExpandY = $d017
VicMemCtrlReg = $d018
VicIrqStatusReg = $d019
VicIrqCtrlReg = $d01a
VicSpritePriority = $d01b
VicSpriteMulticolor = $d01c
VicSpriteExpandX = $d01d
VicSpriteToSpriteCol = $d01e
VicSpriteToBckgndCol = $d01f
VicBorderColor = $d020
VicBackgroundColor0 = $d021
VicBackgroundColor1 = $d022
VicBackgroundColor2 = $d023
VicBackgroundColor3 = $d024
VicSpriteMultiColor0 = $d025
VicSpriteMultiColor1 = $d026
VicSprite0Color = $d027
VicSprite1Color = $d028
VicSprite2Color = $d029
VicSprite3Color = $d02a
VicSprite4Color = $d02b
VicSprite5Color = $d02c
VicSprite6Color = $d02d
VicSprite7Color = $d02e
Cia1PortA = $dc00
Cia1PortB = $dc01
Cia1PortADataDir = $dc02
Cia1PortBDataDir = $dc03
Cia1TimerALb = $dc04
Cia1TimerAHb = $dc05
Cia1TimerBLb = $dc06
Cia1TimerBHb = $dc07
Cia1TimeOfDayDseconds = $dc08
Cia1TimeOfDaySeconds = $dc09
Cia1TimeOfDayMinutes = $dc0a
Cia1TimeOfDayHours = $dc0b
Cia1SerialShiftReg = $dc0c
Cia1IrqCtrlReg = $dc0d
Cia1TimerACtrlReg = $dc0e
Cia1TimerBCtrlReg = $dc0f
Cia2PortA = $dd00
Cia2PortB = $dd01
Cia2PortADataDir = $dd02
Cia2PortBDataDir = $dd03
Cia2TimerALb = $dd04
Cia2TimerAHb = $dd05
Cia2TimerBLb = $dd06
Cia2TimerBHb = $dd07
Cia2TimeOfDayDseconds = $dd08
Cia2TimeOfDaySeconds = $dd09
Cia2TimeOfDayMinutes = $dd0a
Cia2TimeOfDayHours = $dd0b
Cia2SerialShiftReg = $dd0c
Cia2IrqCtrlReg = $dd0d
Cia2TimerACtrlReg = $dd0e
Cia2TimerBCtrlReg = $dd0f
CINT = $ff81
IOINIT = $ff84
RAMTAS = $ff87
RESTOR = $ff8a
VECTOR = $ff8d
SETMSG = $ff90
SECOND = $ff93
TKSA = $ff96
MEMTOP = $ff99
MEMBOT = $ff9c
SCNKEY = $ff9f
SETTMO = $ffa2
ACPTR = $ffa5
CIOUT = $ffa8
UNTLK = $ffab
UNLSN = $ffae
LISTEN = $ffb1
TALK = $ffb4
READST = $ffb7
SETLFS = $ffba
SETNAM = $ffbd
OPEN = $ffc0
CLOSE = $ffc3
CHKIN = $ffc6
CHKOUT = $ffc9
CLRCHN = $ffcc
CHRIN = $ffcf
CHROUT = $ffd2
LOAD = $ffd5
SAVE = $ffd8
SETTIM = $ffdb
RDTIM = $ffde
STOP = $ffe1
GETIN = $ffe4
CLALL = $ffe7
UDTIM = $ffea
SCREEN = $ffed
PLOT = $fff0
IOBASE = $fff3
