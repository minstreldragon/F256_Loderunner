; F256 specific

; kernel structures

event   .dstruct    kernel.event.event_t

zpSrc   .word ?                         ; a pointer to data to read
zpDst   .word ?                         ; a pointer to data to write

zpBufferPtr                             ; read / write buffer
        .word ?

zpFileSize                              ; File size (to save)
        .word ?

; Lode Runner (game)

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
zpDstPtr
        .word ?                         ; $0f/$10

zpBmpPtr1                               ; pointer into bitmap 1
zpEcx                                   ; used for zx7 uncompressor
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

zpBmpLinePtrLb                          ; bitmap line pointer
zpCombinedNibbles
zpJoystickPort
zpNibbleSelect                          ; even: extract low nibble, odd: high nibble
zpSrcCol
zpTmpProbeX                             ; temporary variable enemy probe X
zpTmpProbeY                             ; temporary variable enemy probe Y
zpRleCounter                            ; RLE uncompressor counter
zpDriveStatus                           ; combined drive status: '0': OK
zpCopyProtByteOffset                    ; copy protection check byte offset
zpLastZx7
        .byte ?                         ; $1e

zpBmpLinePtrHb                          ; bitmap line pointer (hb)
zpRleValue                              ; current RLE run value
zpTmpZx7
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

        .byte ?                         ; $40

zpShapeDataOffset                       ; current offset in sprite copy routine
zpShapeDblPixelShift                    ; double-pixel shift applied to shape for printing
        .byte ?                         ; $41

zpIrisAnimDirection                     ; 0: closing, 1: opening
zpIrisPixelColor                        ; COL_TRANSPARENT or COL_SOLIDBLACK
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
        .word ?                         ; $55/$56

zpDemoCommandInput                      ; direction code during game demo
        .byte ?                         ; $57

zpDemoRepeatCounter                     ; command repeat counter during game demo
        .byte ?                         ; $58

zpDemoUnused                            ; variable unused (initialized to $01)
        .byte ?                         ; $59

zpEditorBoardUnchanged                  ; 0: board unchanged, 1: changes pending
        .byte ?                         ; $5a

zpShiftedShapeHopper                    ; 22 bytes of shifted shape data
        .fill 22                        ; $5b

zpSoundEffectPitch                      ; current pitch of sound effect
        .byte ?                         ; $7c

zpJingleDataPtr
        .word ?                         ; $7d
