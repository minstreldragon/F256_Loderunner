
.include "archdep.h"
.include "constants.h"

        * = $6000

        lda #$00
        sta volume                      ; sound playback volume - 0: off, $ff: on
        jsr initHwResources             ; initialize HW resources
        jsr runCopyProtection
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores

resetGame
        lda #$ff
        sta volume                      ; sound playback volume - 0: off, $ff: on
        sta playerNotFalling            ; 0: player is falling, >0: not falling
        sta soundFxPlayerDying          ; sound effect player dying, current pitch
        sta digDirection                ; $ff: dig forward, $00: dig behind runner
        sta irisAnimationOn             ; $ff: iris animation on, $00: turned off
        lda #$00
        sta tuneDataEnd                 ; offset: end of current tune
        sta tunePlayOffset              ; offset: playback position in current tune
        sta playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        sta soundFxPlayerDying          ; sound effect player dying, current pitch
        sta keyboardCode                ; keyboard matrix code (no key: 0)
        sta previousKeyboardCode        ; last registered keyboard code (prevent repeat)
        sta skipShapeConversion         ; 0: don't skip shape conversion, >0: skip shape conversion
        lda #>boardPacked               ; ($10)
        sta boardLoadPage
        sta boardSavePage
        sta jingleGoldBeamPosPrev       ; previous raster beam position for gold jingle
        lda LSTX                        ; Matrix code of last key pressed ($40: none pressed)
        sta previousKeyboardCode        ; last registered keyboard code (prevent repeat)
        lda #KEY_CODE_NONE
        sta LSTX                        ; Matrix code of last key pressed ($40: none pressed)
        lda #$05
        sta gameDelay                   ; 3: fastest, 8: slowest
        lda #PETSCII_J
        sta controllerMode              ; 'J': Joystick, 'K': Keyboard
        lda #GAME_MODE_TITLE_SCREEN     ; set game mode to title screen
        sta gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        jmp displayTitleScreen

enterPlayState
        lda #$00
        sta score+0
        sta score+1
        sta score+2
        sta score+3
        sta enemySpeed                  ; enemy speed (0-10)
        sta irisPhase                   ; iris animation phase: 0: animate open only, $ff: animate both
        sta enemyRespawnCol             ; column for respawning enem
        sta zpDemoRepeatCounter
        sta zpDemoPtr+0
        lda #>demoGameSequence
        sta zpDemoPtr+1
        sta reloadBoard                 ; force loading board if != currentLevel
        lda #$05
        sta lives
        lda #$20
        sta zpBitmapPage2               ; active bitmap page for printing
        jsr resetJingleGoldComplete
        jsr clearBitmapsPrintFooter     ; clear both bitmaps, then print the display footer
        lda #(COL_CYAN << 4 | COL_RED)
        jsr setColorMemory

initBoard
        ldx #$01                        ; X != 0: board requires full init
        jsr setupBoard
        sta tunePlayOffset              ; offset: playback position in current tune
        sta tuneDataEnd                 ; offset: end of current tune
        lda #$ff                        ; animate iris: both open and close
        sta irisPhase                   ; iris animation phase: 0: animate open only, $ff: animate both
        ldy numEnemies
        lda SpriteEnableTable,y         ; bitfield: enabled sprites per number of enemies
        sta VicSpriteEnable
        lda #$00
        sta inputVertical
        sta inputHorizontal
        lda gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        lsr                             ; initiating a game in play mode?
        beq _initEnemyMovement          ; no (demo mode) -> start immediately

_playerFlashingL
        lda VicSpriteEnable
        and #$fe                        ; disable player sprite
        sta VicSpriteEnable

        ldy #$90                        ; delay loop: $9000
        ldx #$00                        ; (note: this is shorter than a blink cycle, to do this
                                        ;        properly it would require a third variable)
_waitForInputL1
        dex
        jsr detectUserInput             ; check for user input
        bcs _enablePlayerSprite         ; joystick or keyboard event detected ->
        bne _waitForInputL1
        dey
        bne _waitForInputL1

        lda VicSpriteEnable
        ora #$01                        ; enable player sprite
        sta VicSpriteEnable

        ldy #$90                        ; delay loop: $9000
        ldx #$00                        ; (note: this is shorter than a blink cycle, to do this
_waitForInputL2
        dex
        jsr detectUserInput             ; check for user input
        bcs _enablePlayerSprite         ; joystick or keyboard event detected ->
        bne _waitForInputL2
        dey
        bne _waitForInputL2
        jmp _playerFlashingL

_enablePlayerSprite
        lda VicSpriteEnable
        ora #$01                        ; enable player sprite
        sta VicSpriteEnable

_initEnemyMovement
        ldx #$00
        stx playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        lda enemySpeed                  ; enemy speed (0-10)
        clc
        adc numEnemies
        tay
        ldx tabDefEnemyMoveCycleOff,y
        ; copy enemy move cycle definition entries into zpEnemyMoveCycleTbl,
        ; starting at zpEnemyMoveCycle0
        lda tabDefEnemyMoveCycle0,x
        sta zpEnemyMoveCycle0           ; enemy move cycle 0
        lda tabDefEnemyMoveCycle1,x
        sta zpEnemyMoveCycle1           ; enemy move cycle 1
        lda tabDefEnemyMoveCycle2,x
        sta zpEnemyMoveCycle2           ; enemy move cycle 2
        ldy enemySpeed                  ; enemy speed (0-10)
        lda tabEnemyTrappedDuration,y   ; time an enemy is trapped in a hole
        sta enemyTrappedDuration

playStateLoop
        jsr handlePlayer                ; handle player movement
        lda playerAlive                 ; 0: player dead, 1: player alive
        beq playerDies                  ; another one bites the dust ->
        lda goldLeft                    ; all gold collected?
        bne checkLevelFinished          ; not yet, or exit ladders already displayed ->
        jsr initJingleGoldComplete      ; play a jingle (all gold collected)
        jsr activateExitLadders         ; activate the exit ladders
checkLevelFinished
        lda zpPlayerY                   ; player, Y position on board
        bne _levelNotFinished           ; player not on top of screen ->
        lda zpPlayerStepY               ; player, Y position (fine)
        cmp #STEP_MIDDLE                ; player in middle position vertically?
        bne _levelNotFinished           ; no ->
        lda goldLeft
        beq _finishLevel
        cmp #$ff                        ; are all exit ladders drawn?
        beq _finishLevel                ; yes ->

_levelNotFinished
        jsr respawnEnemiesHandleHoles   ; respawn enemies, handle holes
        lda playerAlive                 ; 0: player dead, 1: player alive
        beq playerDies
        jsr handleEnemies
        lda playerAlive                 ; 0: player dead, 1: player alive
        beq playerDies
_delayGameLoop
        lda irqFrameCounter             ; free running counter increased with each IRQ
        cmp gameDelay                   ; 3: fastest, 8: slowest
        bcc _delayGameLoop
        lda #$03
        sta irqFrameCounter             ; free running counter increased with each IRQ
        jmp playStateLoop

_finishLevel
        inc displayedLevel              ; current level (displayed)
        inc currentLevel                ; current level (0-based)
_waitFinishTune
        lda tuneDataEnd                 ; offset: end of current tune
        cmp tunePlayOffset              ; offset: playback position in current tune
        bne _waitFinishTune             ; wait until tune has finished playing

        jsr selectNextJingle            ; select next "gold complete" jingle to play
        inc lives                       ; add bonus live
        bne _noOverflow
        dec lives                       ; prevent overflow (lives)
_noOverflow
        lda #$0e                        ; repeat 15 times
        sta zpLevelCompleteScoreCounter ; iterator for increasing score (total: 1500 points)
        sei                             ; prevent interrupts during busy wait loop
_addLevelCompleteScoreL
        lda #$40                        ; Frequency end value (hb)
        sec
        sbc zpLevelCompleteScoreCounter ; lower frequency by counter
        sta SidVoice1FreqHb

        ldy #$2c                        ; delay counter: $2c00
_delayL1
        ldx #$00
_delayL2
        dex
        bne _delayL2
        dey
        bne _delayL1

        sty SidVoice1FreqHb             ; frequency hb = 0 (?)
        tya                             ; score: lb ($00)
        iny                             ; score: hb ($01)
        jsr addPrintScore               ; add 100 points to score, print it
        dec zpLevelCompleteScoreCounter
        bpl _addLevelCompleteScoreL

        cli                             ; re-enable interrupts
playerDiesInitBoard
        jmp initBoard

playerDies
        dec lives
        lda gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        lsr                             ; running in demo mode?
        bne _playerDiesInPlayMode       ; no -> do the full player death sequence
        jmp returnFromDemoMode          ; player dies in demo mode -> return to title screen loop
_playerDiesInPlayMode
        lda #$38
        sta soundFxPlayerDying          ; sound effect player dying, current pitch
_playTuneL
        lda tuneDataEnd                 ; offset: end of current tune
        cmp tunePlayOffset              ; offset: playback position in current tune
        bne _playTuneL
        lda #$00
        sta playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        lda #$20
        sta playerNotFalling            ; 0: player is falling, >0: not falling
        jsr printLives
_waitSoundFxFinishedL
        lda soundFxPlayerDying          ; sound effect player dying, current pitch
        bne _waitSoundFxFinishedL       ; wait for sound effect to finish (IRQ)
        lda lives
        bne playerDiesInitBoard         ; (re)initialize board after player died

terminateGame
        jsr handleHighScoresEntry       ; check/handle entry into list of high scores
        jsr displayGameOver

attractStateLoop
        ldx #$ff                        ; initialize long delay counter
        ldy #$ff
        bcs _attractStateLoopJ1         ; carry is clear after displayGameOver, set after demo
        lda #$00
        sta keyboardCode                ; keyboard matrix code (no key: 0)
_attractStateLoopJ1
        lda #$03                        ; long delay counter: $030000
        sta zpLongDelayCtr

_longDelayLoop
        lda Cia1PortA                   ; read joystick port 2
        and #$10                        ; fire button pressed?
        beq enterPlayGameMode           ; yes -> play the game
        lda keyboardCode                ; keyboard matrix code (no key: 0)
        cmp #KEY_CODE_CTRL+KEY_CODE_E   ; <Ctrl>-E: Enter Edit mode
        bne _checkKeyPressed            ; no ->
        jmp enterEditMode
_checkKeyPressed
        cmp #KEY_CODE_RETURN
        beq attractShowHighScores
        lda keyboardCode                ; keyboard matrix code (no key: 0)
        bne enterPlayGameMode           ; yes -> play the game
        dex
        bne _longDelayLoop
        dey
        bne _longDelayLoop
        dec zpLongDelayCtr              ; long delay counter (highest byte)
        bne _longDelayLoop

        lda gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        bne switchToStartScreen         ; currently not displaying start screen ->
        ; displaying Start Screen -> enter Demo Mode
        ldx #$01                        ; initiate demo mode
        stx gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        stx displayedLevel              ; set level = 1
        stx zpDemoUnused                ; variable unused (initialized to $01)
        stx allowHighScoreEntry         ; 0: player cheated, no high score entry, 1: did not cheat
        ldx volume                      ; sound playback volume - 0: off, $ff: on
        stx returnFromDemoMode+1
        lda #$00
        sta volume                      ; sound playback volume - 0: off, $ff: on
        sta currentLevel                ; current level (0-based)
        jmp enterPlayState              ; initialize Play State and enter playStateLoop

returnFromDemoMode
        lda #$00                        ; restore volume from before demo
        sta volume                      ; sound playback volume - 0: off, $ff: on
        jmp attractStateLoop

switchToStartScreen
        cmp #$01                        ; are we running in demo mode?
        bne switchToStartScreenJ1       ; no -> go back to title screen

attractShowHighScores
        jsr displayHighScores           ; leaving demo mode: display high scores screen
        lda #$02
        sta gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        clc
        jmp attractStateLoop

switchToStartScreenJ1
        lda #$00                        ; start at first level
        sta currentLevel                ; current level (0-based)
        sta gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        jmp displayTitleScreen          ; display the title screen

enterPlayGameMode
        ldx #$00
        stx currentLevel                ; current level (0-based)
        inx
        stx displayedLevel              ; current level (displayed)
        stx allowHighScoreEntry         ; 0: player cheated, no high score entry, 1: did not cheat
        lda #GAME_MODE_PLAY_GAME
        sta gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        lda #$ff
        sta volume                      ; sound playback volume - 0: off, $ff: on
        jmp enterPlayState              ; initialize Play State and enter playStateLoop

tabDefEnemyMoveCycleOff
        .byte tabDefEnemyMoveCyclesBlock00 - tabDefEnemyMoveCycles      ; $00
        .byte tabDefEnemyMoveCyclesBlock01 - tabDefEnemyMoveCycles      ; $03
        .byte tabDefEnemyMoveCyclesBlock02 - tabDefEnemyMoveCycles      ; $06
        .byte tabDefEnemyMoveCyclesBlock03 - tabDefEnemyMoveCycles      ; $09
        .byte tabDefEnemyMoveCyclesBlock04 - tabDefEnemyMoveCycles      ; $0c
        .byte tabDefEnemyMoveCyclesBlock05 - tabDefEnemyMoveCycles      ; $0f
        .byte tabDefEnemyMoveCyclesBlock06 - tabDefEnemyMoveCycles      ; $12
        .byte tabDefEnemyMoveCyclesBlock07 - tabDefEnemyMoveCycles      ; $15
        .byte tabDefEnemyMoveCyclesBlock08 - tabDefEnemyMoveCycles      ; $18
        .byte tabDefEnemyMoveCyclesBlock09 - tabDefEnemyMoveCycles      ; $1b
        .byte tabDefEnemyMoveCyclesBlock0a - tabDefEnemyMoveCycles      ; $1e

        ; BUG: the table is not big enough for all combinations of
        ;      zpNumEnemies and enemySpeed
.if BUGFIX == true
        .byte tabDefEnemyMoveCyclesBlock0b - tabDefEnemyMoveCycles      ; $21
        .byte tabDefEnemyMoveCyclesBlock0c - tabDefEnemyMoveCycles      ; $24
        .byte tabDefEnemyMoveCyclesBlock0d - tabDefEnemyMoveCycles      ; $27
        .byte tabDefEnemyMoveCyclesBlock0e - tabDefEnemyMoveCycles      ; $2a
        .byte tabDefEnemyMoveCyclesBlock0f - tabDefEnemyMoveCycles      ; $2d
        .byte tabDefEnemyMoveCyclesBlock10 - tabDefEnemyMoveCycles      ; $30
        .byte tabDefEnemyMoveCyclesBlock11 - tabDefEnemyMoveCycles      ; $33
        .byte tabDefEnemyMoveCyclesBlock12 - tabDefEnemyMoveCycles      ; $36
        .byte tabDefEnemyMoveCyclesBlock13 - tabDefEnemyMoveCycles      ; $39
        .byte tabDefEnemyMoveCyclesBlock14 - tabDefEnemyMoveCycles      ; $3c
.endif

tabEnemyTrappedDuration                 ; # of moves an enemy is trapped in a hole
        ; index: enemySpeed (1-10)
        ; value: # of moves an enemy is trapped in a hole.
        ; This table compensates for the increased number of moves enemies
        ; have at higher enemySpeed values.
        .byte $26,$26,$2e,$44,$47,$49,$4a,$4b
        .byte $4c,$4d,$4e,$4f,$50,$51,$52,$53
        .byte $54,$55,$56,$57,$58


displayTitleScreen                      ; display the title screen
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda #(COL_BLACK << 4 | COL_BLACK)
        jsr setColorMemory
        lda VicScreenCtrlReg1           ; ($d011)
        ora #$20                        ; enable bitmap mode
        sta VicScreenCtrlReg1
        lda VicMemCtrlReg               ; ($d018)
        and #$f0                        ; preserve screen ram: $0400
        ora #$08                        ; bitmap start: $2000
        sta VicMemCtrlReg
        ldy #$00

_setTitleColorL1
        lda #(COL_BLUE << 4 | COL_BLACK)
        sta $0400,y                     ; rows 0-6: blue
        sta $0418,y                     ; (Broderbund Software)
        lda #(COL_WHITE << 4 | COL_BLACK)
        sta $0568,y                     ; rows 9-14: white (Lode Runner)
        dey
        bne _setTitleColorL1

        ldy #$50
_setTitleColorL2
        lda #(COL_RED << 4 | COL_BLACK)
        sta $0518,y                     ; rows 7,8: red (Presents)
        sta $0658,y                     ; rows 15,16: red (By)
        lda #(COL_WHITE << 4 | COL_BLACK)
        sta $06d0,y                     ; rows 18,19: white (DOUG SMITH...)
        lda #(COL_BLUE << 4 | COL_BLACK)
        sta $0748,y                     ; rows 21,22: blue (Copyright)
        dey
        bpl _setTitleColorL2

        lda VicScreenCtrlReg2           ; ($d016)
        and #$ef                        ; disable multi color mode
        sta VicScreenCtrlReg2

        ; unpack title screen from $1400 to $2000

        lda #$00
        sta zpRleDataPtr+0              ; pointer to RLE packed data
        lda #$14
        sta zpRleDataPtr+1
        lda #$ff
        sta zpUnpackedDataPtr+0         ; pointer to unpacked RLE data
        lda #$1f
        sta zpUnpackedDataPtr+1
_unpackRleDataL1
        ldy #$00
        lda (zpRleDataPtr),y            ; read byte from RLE packed data
        sta zpRleCounter                ; RLE repeat counter
        iny
        lda (zpRleDataPtr),y            ; read byte from RLE packed data
        sta zpRleValue                  ; current RLE run valeu
        lda zpRleDataPtr+0
        clc
        adc #$02                        ; source ptr += 2
        sta zpRleDataPtr+0
        bcc _skipIncSrc
        inc zpRleDataPtr+1
_skipIncSrc
        lda zpRleCounter                ; RLE repeat counter
        tay
        lda zpRleValue                  ; current RLE run valeu
_unpackRleDataL2
        sta (zpUnpackedDataPtr),y       ; write byte to destination
        dey
        bne _unpackRleDataL2
        lda zpUnpackedDataPtr+0
        clc
        adc zpRleCounter                ; RLE repeat counter
        sta zpUnpackedDataPtr+0
        bcc _skipIncDst
        inc zpUnpackedDataPtr+1
_skipIncDst
        lda zpUnpackedDataPtr+1
        cmp #$3f
        bcc _unpackRleDataL1
        jmp attractStateLoop

handleJinglePlayback
        ldx tunePlayOffset              ; offset: playback position in current tune
        cpx tuneDataEnd                 ; offset: end of current tune
        beq finishTunePlayback
        lda #$11                        ; wave form: triangle, enable voice
        sta ctrlRegVoice2
        sta ctrlRegVoice3
        lda tuneDataVolume,x            ; read volume information from tune data (ignored)
        lda #$f0                        ; (release = 0)
        and volume                      ; sound playback volume - 0: off, $ff: on
        sta SidVoice2SustainRelease     ; (sustain = off or maxiumum, release = 0)
        sta SidVoice3SustainRelease     ; (sustain = off or maxiumum, release = 0)

        lda tuneDataVoice0,x
        bne _noPauseVoice2              ; pause voice? no ->
        sta ctrlRegVoice2               ; turn off voice 0
_noPauseVoice2
        tay
        lda frequencyTableLow,y
        sta SidVoice2FreqLb
        lda frequencyTableHigh,y
        sta SidVoice2FreqHb

        lda tuneDataVoice1,x
        bpl _distinctVoice1             ; if >= 0: voice 3 polyphony ->
        lda tuneDataVoice0,x            ; else, play same pitch as voice 0, but slightly lower
        tay
        sec
        lda frequencyTableLow,y
        sbc #$80                        ; subtract a delta of $0080 from voice 0 frequency data
        sta SidVoice3FreqLb
        lda frequencyTableHigh,y
        sbc #$00
        sta SidVoice3FreqHb
        jmp _startStopVoices

_distinctVoice1
        bne _noPauseVoice3              ; pause voice? no ->
        sta ctrlRegVoice3               ; turn off voice 1
_noPauseVoice3
        tay
        lda frequencyTableLow,y
        sta SidVoice3FreqLb
        lda frequencyTableHigh,y
        sta SidVoice3FreqHb
_startStopVoices
        lda ctrlRegVoice2               ; (start/stop Voice)
        sta SidVoice2CtrlReg
        lda ctrlRegVoice3               ; (start/stop Voice)
        sta SidVoice3CtrlReg
        dec tuneDataNoteLength,x
        bne _exit
        inc tunePlayOffset              ; offset: playback position in current tune
_exit
        rts

finishTunePlayback
        lda #$00
        sta SidVoice2CtrlReg
        sta SidVoice3CtrlReg
        rts

initJingleCollectGold
        pla                             ; pull address for jingle data from stack
        sta zpJingleDataPtr+0           ; (jingle data starts right after JSR command)
        pla
        sta zpJingleDataPtr+1
        bne _increaseJinglePtr          ; increase jingle pointer

_initJingleDataLoop
        sei                             ; prevent music playback by interrupts during initialization
        ldy #$00                        ; end of sequence?
        lda (zpJingleDataPtr),y         ; jingle event byte #0: note length
        beq _initJingleDataExit
        ldx tuneDataEnd                 ; offset: end of current tune
        sta tuneDataNoteLength,x
        iny
        lda (zpJingleDataPtr),y         ; jingle event byte #1: pitch voice 2
        sta tuneDataVoice0,x
        iny
        lda (zpJingleDataPtr),y         ; jingle event byte #2: pitch voice 3
        sta tuneDataVoice1,x
        iny
        lda (zpJingleDataPtr),y         ; jingle event byte #3: volume / sustain / release
        sta tuneDataVolume,x
        inc tuneDataEnd                 ; offset: end of current tune
        lda zpJingleDataPtr+0           ; data pointer += 4
        clc
        adc #$04
        sta zpJingleDataPtr+0
        lda zpJingleDataPtr+1
        adc #$00
        sta zpJingleDataPtr+1
        bne _initJingleDataLoop

_increaseJinglePtr
        inc zpJingleDataPtr+0
        bne _initJingleDataLoop
        inc zpJingleDataPtr+1
        bne _initJingleDataLoop

_initJingleDataExit
        lda zpJingleDataPtr+1           ; point stack to position after jingle data
        pha
        lda zpJingleDataPtr+0
        pha
        cli                             ; allow music playback by interrupts
        rts


randomizeGoldJinglePitchVal
        sta jingleGoldMinimumPitchDelta ; minimum delta between consecutive pitch values ($04)
_waitBeamPosition
        lda VicRasterValue
        sta jingleGoldBeamPosCur        ; current raster beam position for gold jingle

        ; condition 1: beam position >= minimum ($10)
        cpx jingleGoldBeamPosCur        ; compare minimum beam position with current value
        bcc _waitForCondition2          ; beam pos  > minimum? -> continue
        beq _waitForCondition2          ; beam pos == minimum? -> continue
        jmp _waitBeamPosition           ; else: beam pos < minimum -> wait

        ; condition 2: beam position <= maximum ($20)
_waitForCondition2
        cpy jingleGoldBeamPosCur        ; compare maximum beam position with current value
        bcc _waitBeamPosition           ; beam pos  > maximum? -> wait

        ; calculate delta between current beam pos and previous one
        ; used for jingle
        sec
        sbc jingleGoldBeamPosPrev       ; subtract previously used beam pos

        bcs _checkCondition3            ; delta >= 0 ->
        eor #$ff                        ; invert delta value
        adc #$01                        ; (calculate two's complement)

        ; condition 3: absolute delta between current and previous value >= min delta ($04)
_checkCondition3
        cmp jingleGoldMinimumPitchDelta ; minimum delta between consecutive pitch values
        bcc _waitBeamPosition
        lda jingleGoldBeamPosCur
        sta jingleGoldBeamPosPrev       ; previous raster beam position for gold jingle
        rts


resetJingleGoldComplete
        lda jingleGoldCompleteMax
        sta jingleGoldComplete          ; jingle ID: player has collected all gold
        lda jingleTransposeMin
        sta jingleGoldCompleteTranspose ; transpose value for jingle
        rts


selectNextJingle
        dec jingleGoldComplete          ; jingle ID: player has collected all gold
        bpl _exit
        inc jingleGoldCompleteTranspose ; transpose value for jingle
        lda jingleGoldCompleteTranspose
        cmp jingleTransposeMax          ; maximum transpose value reached?
        bcc _wrapToMaxJingle            ; no ->
        lda jingleTransposeMin          ; wrap to minimum transpose value
        sta jingleGoldCompleteTranspose ; transpose value for jingle
_wrapToMaxJingle
        lda jingleGoldCompleteMax
        sta jingleGoldComplete          ; jingle ID: player has collected all gold
_exit
        rts


initJingleGoldComplete
        lda jingleGoldComplete          ; jingle ID: player has collected all gold
        asl                             ; as 16 bit offset
        tax
        lda jingleDataPtrTbl+0,x        ; pointer: start of jingle data
        sta zpJingleDataPtr+0           ; as zero page pointer
        lda jingleDataPtrTbl+1,x
        sta zpJingleDataPtr+1

_initJingleDataLoop
        sei                             ; prevent music playback by interrupts during initialization
        ldy #$00
        lda (zpJingleDataPtr),y         ; jingle event byte #0: note length
        beq _initJingleDataExit
        ldx tuneDataEnd                 ; offset: end of current tune
        sta tuneDataNoteLength,x
        iny
        lda (zpJingleDataPtr),y         ; jingle event byte #1: pitch voice 2
        beq _initJingleVoice3
        clc
        adc jingleGoldCompleteTranspose ; transpose value for jingle
_initJingleVoice3
        sta tuneDataVoice0,x
        iny
        lda (zpJingleDataPtr),y         ; jingle event byte #2: pitch voice 3
        beq _initJingleVolume
        bmi _initJingleVolume
        clc
        adc jingleGoldCompleteTranspose ; transpose value for jingle
_initJingleVolume
        sta tuneDataVoice1,x
        iny
        lda (zpJingleDataPtr),y         ; jingle event byte #3: volume / sustain / release
        sta tuneDataVolume,x
        inc tuneDataEnd                 ; offset: end of current tune
        lda zpJingleDataPtr+0           ; data pointer += 4
        clc
        adc #$04
        sta zpJingleDataPtr+0
        lda zpJingleDataPtr+1
        adc #$00
        sta zpJingleDataPtr+1
        bne _initJingleDataLoop
_initJingleDataExit
        cli                             ; allow music playback by interrupts
        rts


irqHandler
        inc irqFrameCounter             ; increase the IRQ/Frame counter
        lda #$00
        sta zpSoundEffectPitch          ; current pitch of sound effect
        lda volume                      ; sound playback volume - 0: off, $ff: on
        and #$0f                        ; disable filters and voice 3, only set volume
        sta SidFilterModeVolume         ; set volume and filters

        ; handle sound effect: player falling
        lda playerNotFalling            ; 0: player is falling, >0: not falling
        bne _playerNotFalling           ; player not falling ->
        ldx soundFxFallingPitch         ; sound effect player falling, current pitch
        stx zpSoundEffectPitch          ; current pitch of sound effect
        jmp _irqJ1
_playerNotFalling
        lda #$53                        ; reset sound effect (prepare for next use)
        sta soundFxFallingPitch         ; sound effect player falling, current pitch

_irqJ1
        ; handle sound effect: player digging
        lda playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        beq _playerNotDigging           ; player not digging ->
        lda soundFxPlayerDigging        ; sound effect player digging, current pitch
        sec
        sbc #$20                        ; decrease pitch
        sta soundFxPlayerDigging        ; sound effect player digging, current pitch
        sta zpSoundEffectPitch          ; current pitch of sound effect
        beq _playerNotDigging
        jmp _irqJ2
_playerNotDigging
        lda #$00                        ; reset sound effect (prepare for next use)
        sta soundFxPlayerDigging        ; sound effect player digging, current pitch

_irqJ2
        lda soundFxPlayerDying          ; sound effect player dying, current pitch
        beq _irqJ3
        sec
        sbc #$04                        ; decrease pitch
        sta zpSoundEffectPitch          ; current pitch of sound effect
        sta soundFxPlayerDying          ; sound effect player dying, current pitch

_irqJ3
        lda zpSoundEffectPitch          ; current pitch of sound effect
        sta SidVoice1FreqHb             ; store in SID voice 1

        jsr handleJinglePlayback        ; play back jingle if active

        lda LSTX                        ; Matrix code of last key pressed ($40: none pressed)
        bne _handleKeyboardJ1           ; KEY_CODE_INSERT_DELETE = 0?
        lda #KEY_CODE_UP_DOWN           ; replace insert/delete by cursor up/down
_handleKeyboardJ1
        cmp #KEY_CODE_NONE              ; no key pressed? ($40)
        bne _handleKeyModifiers         ; key press detected -> handle modifiers
        lda #$00
        sta previousKeyboardCode        ; last registered keyboard code (prevent repeat)
        jmp c64KernelStandardIrq        ; KERNEL: standard IRQ routine (reads keyboard)

_handleKeyModifiers
        ldx SHFLAG                      ; Shift Flag (bit 0: SHIFT, bit 1: C=, bit 2: CTRL)
        beq _preventKeyRepeat           ; no ->
        ora #KEY_CODE_CTRL              ; mark <CTRL> modifier in keyboard code

_preventKeyRepeat
        cmp previousKeyboardCode        ; last registered keyboard code (prevent repeat)
        beq _finishIrq
_storeKeyCode
        sta previousKeyboardCode        ; last registered keyboard code (prevent repeat)
        sta keyboardCode                ; keyboard matrix code (no key: 0)
_finishIrq
        jmp c64KernelStandardIrq        ; KERNEL: standard IRQ routine (reads keyboard)

initHwResources
        lda R6510                       ; processor port ($01)
        and #$fe                        ; disable BASIC, $a000-$bfff RAM
        sta R6510

        lda #$00
        sta zpBmpLinePtrLb
        lda #$00
        sta zpBmpLinePtrHb
        ldy #$00                        ; table index
        lda #25
        sta rowIterator                 ; row iterator for table init routine
_initTblsBmpLinePtrL1
        ldx #$08                        ; iterator: loop over 8 lines
_initTblsBmpLinePtrL2
        lda zpBmpLinePtrHb              ; write current line pointer to table (hb)
        sta TblBmpLinePtrHb,y
        lda zpBmpLinePtrLb              ; write current line pointer to table (lb)
        sta TblBmpLinePtrLb,y
        inc zpBmpLinePtrLb              ; next line: increment line pointer
        iny                             ; inc. table index
        dex                             ; dec. loop counter
        bne _initTblsBmpLinePtrL2       ; iterate over 8 lines (inner loop)

        clc
        adc #<(320-7)                   ; next line: increment line pointer (lb)
        sta zpBmpLinePtrLb
        lda zpBmpLinePtrHb
        adc #>(320-7)                   ; next line: increment line pointer (hb)
        sta zpBmpLinePtrHb
        dec rowIterator                 ; row iterator for table init routine
        bne _initTblsBmpLinePtrL1       ; iterate over 25 rows

        ; init hardware resources

        lda #<nmiHandler
        sta NMINV+0                     ; set NMI vector
        lda #>nmiHandler
        sta NMINV+1
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda #COL_BLACK
        sta VicBackgroundColor0         ; background color: black
        sta VicBorderColor              ; border color: black
        sei
        lda #<irqHandler
        sta CINV+0                      ; set IRQ vector
        lda #>irqHandler
        sta CINV+1
        cli
        lda #$b0
        sta SidVoice1SustainRelease
        lda #$11
        sta SidVoice1CtrlReg
        ldx #$30                        ; sprite 0: player ($30)
        stx SpritePtrs+0
        inx                             ; sprite 2: enemy 0 ($31)
        stx SpritePtrs+2
        inx                             ; sprite 3: enemy 1 ($32)
        stx SpritePtrs+3
        inx                             ; sprite 4: enemy 2 ($33)
        stx SpritePtrs+4
        inx                             ; sprite 6: enemy 3 ($34)
        stx SpritePtrs+6
        inx                             ; sprite 7: enemy 4 ($35)
        stx SpritePtrs+7

        lda #COL_CYAN                   ; color for enemies
        ldy #$07
_setSpriteColorL
        sta VicSprite0Color,y           ; set sprite color for sprites 1..7 (enemies)
        dey
        bne _setSpriteColorL

        iny                             ; COL_WHITE (player)
        sty VicSprite0Color             ; set sprite color for sprite 0 (player)

        lda #$00
        sta VicSpritePriority

        tay
_clearSpriteDefsL                       ; clear sprite definitions
        sta @w $0002,y                  ; clear zero page variables
        sta spriteDefinitions+$0000,y
        sta spriteDefinitions+$0100,y
        iny
        bne _clearSpriteDefsL

        lda #$ff                        ; enable all sprites
        sta VicSpriteEnable
        rts

setColorMemory
        sta multiColorValues            ; store multi color values
        lda VicScreenCtrlReg2
        ora #$10                        ; enable multi color mode
        sta VicScreenCtrlReg2

        ldy #$00
multiColorValues = * + 1
_initColorRamL
        lda #$86                        ; restore multi color values
        sta ScreenRAM+$0000,y           ; set colors 1/2
        sta ScreenRAM+$0100,y
        sta ScreenRAM+$0200,y
        sta ScreenRAM+$02f8,y           ; don't overwrite sprite pointers
        lda #COL_WHITE
        sta ColorRAM+$0000,y            ; set color RAM to WHITE
        sta ColorRAM+$0100,y
        sta ColorRAM+$0200,y
        sta ColorRAM+$0300,y
        dey
        bne _initColorRamL
        rts

nmiHandler
        rti                             ; NMI handler (do nothing)

SpriteEnableTable
        .byte %00000001
        .byte %00000101
        .byte %00001101
        .byte %00011101
        .byte %01011101
        .byte %11011101

enterEditMode
        lda #$00
        sta score+0                     ; score = 0
        sta score+1
        sta score+2
        sta score+3
        lda #$ff
        sta reloadBoard                 ; force loading board if != currentLevel
        lda #$05                        ; GAME_MODE_EDITOR = 5
        sta lives                       ; lives = 5
        sta gameMode                    ; set game mode: editor (0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor)
        sta skipShapeConversion         ; 0: don't skip shape conversion, >0: skip shape conversion
        lda controllerMode              ; 'J': Joystick, 'K': Keyboard
        sta ldaControllerMode+1         ; store in self-modified variable (controller mode for play mode)
        lda #PETSCII_K                  ; force keyboard mode for editor
        sta controllerMode              ; 'J': Joystick, 'K': Keyboard
        lda currentLevel                ; current level (0-based)
        cmp #BOARD_MAXIMUM+1
        bcc editorMenuLoop
        lda #$00                        ; set level to 0
        sta currentLevel                ; current level (0-based)

editorMenuLoop
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda #(COL_YELLOW << 4 | COL_CYAN)
        jsr setColorMemory
        lda #>Bitmap0                   ; print to bitmap 0
        sta zpBitmapPage2               ; set active bitmap page for printing text
        lda #$00
        sta keyboardCode                ; keyboard matrix code (no key: 0)
        sta zpCursorCol
        sta zpCursorRow
        jsr printString
        .text "  LODE RUNNER BOARD EDITOR",$8d
        .text "----------------------------",$8d
        .text " R/S KEY ABORTS ANY COMMAND",$8d,$00

returnToEditorMenu                      ; continue here after last command
        lda zpCursorRow
        cmp #$09                        ; cursor row >= 9?
        bcs editorMenuLoop              ; yes ->

printCommandPrompt
        jsr printString
        .text $8d
        .text "COMMAND>",$00
        jsr waitForKeyboardInput
_checkEditorMenuCmdKeyL
        ldy editorMenuCmdKeyTbl,x       ; end of command table reached?
        beq abortEditorCommandWithBell  ; yes -> abort
        cmp editorMenuCmdKeyTbl,x       ; command key matches?
        beq executeEditorCommand        ; yes -> execute command X
        inx
        bne _checkEditorMenuCmdKeyL

abortEditorCommandWithBell
        jsr soundBell
        jmp returnToEditorMenu

executeEditorCommand
        txa                             ; command number
        asl                             ; as 16 bit offset
        tax
        lda editorMenuCmdJmpTbl+1,x     ; retrieve jump address for command X
        pha                             ; push it on the stack
        lda editorMenuCmdJmpTbl+0,x
        pha
        rts                             ; jump to editor command

editorMenuCmdKeyTbl
        .byte KEY_CODE_P                ; (P)lay level
        .byte KEY_CODE_C                ; (C)lear level
        .byte KEY_CODE_E                ; (E)dit level
        .byte KEY_CODE_M                ; (M)ove level
        .byte KEY_CODE_I                ; (I)nitialize user disk
        .byte KEY_CODE_S                ; (S)core - clear high scores from disk
        .byte $00

editorMenuCmdJmpTbl
        .word playLevel-1
        .word clearLevel-1
        .word editLevel-1
        .word moveLevel-1
        .word initializeUserDisk-1
        .word clearHighScores-1

playLevel
        jsr printString
        .text $8d
        .text ">>PLAY LEVEL",$00
        jsr inputLevelChecked
        bcs playLevelAbort              ; level out of range ->
ldaControllerMode                       ; label for self-modified variable:
        lda #$00                        ; restore controller mode for playing game (instead editing)
        sta controllerMode              ; 'J': Joystick, 'K': Keyboard
        lda #GAME_MODE_PLAY_FROM_EDITOR
        sta gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        lda #$01
        sta allowHighScoreEntry         ; 0: player cheated, no high score entry, 1: did not cheat
        lda #$00
        sta skipShapeConversion         ; 0: don't skip shape conversion, >0: skip shape conversion
        lda currentLevel                ; current level (0-based)
        beq _playLevelStart             ; start from level 0  -> not cheating
        lsr allowHighScoreEntry         ; 0: player cheated, no high score entry, 1: did not cheat
_playLevelStart
        jmp enterPlayState              ; initialize Play State and enter playStateLoop
playLevelAbort
        jmp abortEditorCommandWithBell

clearLevel
        jsr printString
        .text $8d
        .text ">>CLEAR LEVEL",$00
        jsr inputLevelChecked
        bcs _abort
        jsr assertUserDisk              ; enforce drive contains a user disk
        ldy #$00                        ; tile index within board
        tya
_clearBoardL
        sta boardPacked,y               ; clear current board (packed by combining two shapes in one byte)
        iny
        bne _clearBoardL
        lda #$02                        ; operation: store cleared board in board set
        jsr loadStoreBoard              ; load from (1) or store (2) board in board set
        jmp returnToEditorMenu
_abort
        jmp abortEditorCommandWithBell

editLevel
        jsr printString
        .text $8d
        .text ">>EDIT LEVEL",$00
        jsr inputLevelChecked
        bcs _abort
        jmp startBoardEditor
_abort
        jmp abortEditorCommandWithBell

moveLevel
        jsr printString
        .byte $8d
        .text ">>MOVE LEVEL",$00
        jsr inputLevelChecked           ; let user input 1st level number
        bcs _abort                      ; input error -> abort
        sty selectedLevel1
        jsr printString
        .text " TO LEVEL",$00
        jsr inputLevelChecked           ; let user input 1st level number
        bcs _abort                      ; input error -> abort
        sty selectedLevel2
        jsr printString
        .byte $8d
        .text "  SOURCE DISKETTE",$00
        jsr waitForKeyboardInput
        jsr assertUserDisk              ; enforce drive contains a user disk
        lda selectedLevel1
        sta currentLevel                ; current level (0-based)
        lda #$01
        jsr loadStoreBoard              ; load from (1) or store (2) board in board set
        jsr printString
        .byte $8d
        .text "  DESTINATION DISKETTE",$00
        jsr waitForKeyboardInput
        jsr assertUserDisk              ; enforce drive contains a user disk
        lda selectedLevel2
        sta currentLevel                ; current level (0-based)
        lda #$02
        jsr loadStoreBoard              ; load from (1) or store (2) board in board set
        jmp returnToEditorMenu
_abort
        jmp abortEditorCommandWithBell

initializeUserDisk
        jsr printString
        .byte $8d
        .text ">>INITIALIZE",$8d
        .text "  THIS PREPARES AN ALREADY",$8d
        .text "  FORMATTED DISK FOR USER",$8d
        .text "  CREATED LEVELS.",$8d
        .text "  (WILL DESTROY OLD DATA).",$8d
        .text $8d
        .text "  ARE YOU SURE (Y/N) ",$00
        jsr waitForKeyboardInput
        cmp #KEY_CODE_Y                 ; 'Y'?
        bne _abort                      ; no -> abort
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores
        cmp #DISK_MASTER                ; master disk inserted?
        bne _initUserDiskJ1             ; no ->
        jsr warningMasterDiskNotAllowed ; message: not allowed to manipulate master disk
        jmp editorMenuLoop
_initUserDiskJ1
        lda currentLevel                ; current level (0-based)
        pha                             ; store current level
        lda #DISK_CMD_INIT              ; initialize the disk
        jsr loadStoreBoard              ; load from (1) or store (2) board in board set
        pla
        sta currentLevel                ; restore current level (0-based)
_abort
        jmp returnToEditorMenu


clearHighScores
        jsr printString
        .text $8d
        .text ">>CLEAR SCORE FILE",$8d
        .text "  THIS CLEARS THE HIGH",$8d
        .text "  SCORE FILE OF ALL",$8d
        .text "  ENTRIES.",$8d
        .text $8d
        .text "  ARE YOU SURE (Y/N) ",$00
        jsr waitForKeyboardInput
        cmp #KEY_CODE_Y                 ; 'Y'?
        bne _abort
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores
        cmp #DISK_UNKNOWN
        beq _abortNoDataDisk

        lda #$00                        ; high score record: init value
        ldy #$7f                        ; iterator over high score records
_clearHighScoresL
        sta highScoreData,y             ; clear byte in high score record
        dey
        bpl _clearHighScoresL

        lda #DISK_CMD_SAVE              ; command: save file
        jsr loadSaveHighScores
        jmp returnToEditorMenu

_abortNoDataDisk
        jsr warningUnknownDisk          ; message: disk is not a lode runner data disk
_abort
        jmp returnToEditorMenu

startBoardEditor
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        jsr assertUserDisk              ; enforce drive contains a user disk
        lda #(COL_CYAN << 4 | COL_RED)
        jsr setColorMemory
        lda #>Bitmap1                   ; print to bitmap 1
        sta zpBitmapPage2               ; set active bitmap page for printing text
        jsr printBoardFooter            ; draw solid ground and print status line
        ldx #$01
        stx zpEditorBoardUnchanged      ; 1: board unchanged, 0: changes pending
        dex                             ; 0: board has been initialized before
        jsr setupBoard
        bcc _noError
        jmp abortEditorCommandWithBell

_noError
        lda #$00                        ; set editor cursor to home (0,0)
        sta zpCursorCol
        sta zpCursorRow

boardEditorMainLoop
        jsr boardEditorWaitForUserKey   ; wait for user pressing a key while flashing cursor
        jsr checkKeyDigit               ; check whether user pressed a digit key
        bcs _checkEditorCmdKey          ; not a digit -> check for command keys
        sta zpEditorNewShape            ; user selected shape (0-9)
        ldy zpCursorRow
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpCursorCol
        lda zpEditorNewShape            ; user selected shape (0-9)
        eor (zpBoardActionPtr),y        ; compare with current board shape at cursor
        beq _noBoardChange
        lsr zpEditorBoardUnchanged      ; mark board as changed (change a '1' into '0')
_noBoardChange
        lda zpEditorNewShape            ; user selected shape (0-9)
        sta (zpBoardActionPtr),y        ; change the tile in the board
        jsr replaceTileBitmap0
        jmp boardEditorMainLoop

_checkEditorCmdKey
        sta zpEditorUserKey             ; key pressed by user
        ldy #$ff                        ; start with index 0
_checkEditorCmdKeyL
        iny
        lda boardEditorCmdKeyTbl,y      ; compare with command key codes
        beq abortBoardEditorCmdWithBell ; unsupported key? ->
        cmp zpEditorUserKey             ; key pressed by user
        bne _checkEditorCmdKeyL
        tya
        asl
        tay
        lda boardEditorCmdJmpTbl+1,y
        pha                             ; as return address on stack
        lda boardEditorCmdJmpTbl+0,y
        pha                             ; as return address on stack
        rts                             ; jump to board editor function

abortBoardEditorCmdWithBell
        jsr soundBell
        jmp boardEditorMainLoop

editorCursorUp
        lda zpCursorRow
        beq abortBoardEditorCmdWithBell
        dec zpCursorRow
        bpl boardEditorMainLoop

editorCursorLeft
        lda zpCursorCol
        beq abortBoardEditorCmdWithBell
        dec zpCursorCol
        bpl boardEditorMainLoop

editorCursorRight
        lda zpCursorCol
        cmp #BOARD_WIDTH-1
        bcs abortBoardEditorCmdWithBell
        inc zpCursorCol
        bne boardEditorMainLoop

editorCursorDown
        lda zpCursorRow
        cmp #BOARD_HEIGHT-1
        bcs abortBoardEditorCmdWithBell
        inc zpCursorRow
        bne boardEditorMainLoop

saveCurrentBoard                        ; save the current board
        lda zpCursorRow                 ; store cursor row and column
        pha
        lda zpCursorCol
        pha
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores
        cmp #DISK_UNKNOWN
        bne _saveCurrentBoardJ1
        jsr warningUnknownDisk          ; message: disk is not a lode runner data disk
        jmp saveCurrentBoardAbort
_saveCurrentBoardJ1
        cmp #DISK_MASTER
        bne _saveCurrentBoardJ2
        jsr warningMasterDiskNotAllowed ; message: not allowed to manipulate master disk
        jmp saveCurrentBoardAbort
_saveCurrentBoardJ2
        jsr packBoard                   ; pack and store the board in the level set
        pla
        sta zpCursorCol                 ; restore cursor row and column
        pla
        sta zpCursorRow
        lda #$01
        sta zpEditorBoardUnchanged      ; 1: board unchanged, 0: changes pending
        rts

saveCurrentBoardAbort
        lda #$00                        ; set editor cursor to home (0,0)
        sta zpCursorCol
        sta zpCursorRow
        jmp boardEditorMainLoop         ; BUG: leaves four bytes dead on the stack

editorSaveBoard
        jsr saveCurrentBoard            ; save the current board
        jmp boardEditorMainLoop

editorBoardForward
        lda currentLevel                ; current level (0-based)
        cmp #BOARD_MAXIMUM              ; 149
editorBoardBackAbort
        beq abortBoardEditorCmdWithBell
        jsr warningUnsavedLevelChanges  ; warn about unsaved level changes
        inc currentLevel                ; current level (0-based)
        inc displayedLevel              ; current level (displayed)
        jmp startBoardEditor

editorBoardBack
        lda currentLevel                ; current level (0-based)
        beq editorBoardBackAbort
        jsr warningUnsavedLevelChanges  ; warn about unsaved level changes
        dec displayedLevel              ; current level (displayed)
        dec currentLevel                ; current level (0-based)
        jmp startBoardEditor

editorQuit
        jsr warningUnsavedLevelChanges  ; warn about unsaved level changes
        jmp editorMenuLoop

warningUnsavedLevelChanges              ; warn about unsaved level changes
        lda zpEditorBoardUnchanged      ; 1: board unchanged, 0: changes pending
        beq _unsavedChanges
        rts
_unsavedChanges
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda multiColorValues            ; store current multi color values
        pha
        lda #(COL_YELLOW << 4 | COL_CYAN)
        jsr setColorMemory
        lda #>Bitmap0                   ; print to bitmap 0
        sta zpBitmapPage2               ; set active bitmap page for printing text
        lda #$00
        sta zpCursorCol                 ; set cursor to (0,0)
        sta zpCursorRow
        jsr printString
        .text "LEVEL HAS BEEN CHANGED BUT",$8d
        .text "NOT SAVED. DO YOU WISH TO",$8d
        .text "SAVE MODIFIED LEVEL (Y/N) ",$00
        jsr soundBell
_promptYesNoL
        lda #$00
        jsr waitForUserKey              ; flash cursor while waiting for input
        ldx #$00
        stx keyboardCode                ; clear keyboard matrix code
        cmp #KEY_CODE_N                 ; 'N'?
        beq _continueAfterSave
        cmp #KEY_CODE_Y                 ; 'Y'?
        bne _promptYesNoL
        jsr saveCurrentBoard            ; save the current board
_continueAfterSave
        pla                             ; restore current multi color values
        jmp setColorMemory

assertUserDisk
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores
        cmp #DISK_UNKNOWN               ; check for signature
        bne _assertUserDiskJ1           ; signature found ->
        jsr warningUnknownDisk          ; message: disk is not a lode runner data disk
        jmp editorMenuLoop              ; BUG: leaves two bytes dead on the stack
_assertUserDiskJ1
        cmp #$01                        ; master disk?
        bne _assertUserDiskJ2           ; no -> user disk
        jsr warningMasterDiskNotAllowed ; message: not allowed to manipulate master disk
        jmp editorMenuLoop              ; BUG: leaves two bytes dead on the stack
_assertUserDiskJ2
        rts

warningMasterDiskNotAllowed             ; message: not allowed to manipulate master disk
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda multiColorValues            ; store current multi color values
        pha
        lda #>Bitmap0                   ; print to bitmap 0
        sta zpBitmapPage2               ; set active bitmap page for printing text
        lda #(COL_YELLOW << 4 | COL_CYAN)
        jsr setColorMemory
        lda #$00                        ; set cursor to home (0,0)
        sta zpCursorCol
        sta zpCursorRow
        jsr printString
        .text "USER NOT ALLOWED TO",$8d
        .text "MANIPULATE MASTER DISKETTE.",$00

dialogHitKeyToContinue
        jsr printString
        .text $8d
        .text $8d
        .text "HIT A KEY TO CONTINUE ",$00
        jsr soundBell
        lda #SHAPE_BLANK                ; use b/w cursor
        jsr waitForUserKey              ; flash cursor while waiting for input
        lda #$00
        sta keyboardCode                ; keyboard matrix code (no key: 0)
        lda #>Bitmap1                   ; print to bitmap 1
        sta zpBitmapPage2               ; set active bitmap page for printing text
        jsr printBoardFooter            ; draw solid ground and print status line
        lda #$00
        sta boardRequiresFullInit       ; 0: board has been initialized before, != 0: board requires full init
        jsr initBoardState              ; initialize the board state (player, #enemies, #ladders etc)
        pla                             ; restore current multi color values
        jmp setColorMemory              ; set color memory and return

warningUnknownDisk                      ; message: disk is not a lode runner data disk
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda multiColorValues            ; store current multi color values
        pha
        lda #(COL_YELLOW << 4 | COL_CYAN)
        jsr setColorMemory
        lda #>Bitmap0                   ; print to bitmap 0
        sta zpBitmapPage2               ; set active bitmap page for printing text
        lda #$00                        ; set cursor to home (0,0)
        sta zpCursorCol
        sta zpCursorRow
        jsr printString
        .text "DISKETTE IN DRIVE IS NOT A",$8d
        .text "LODE RUNNER DATA DISK.",$00
        jmp dialogHitKeyToContinue

boardEditorWaitForUserKey
        ldy zpCursorRow
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpCursorCol
        lda (zpBoardActionPtr),y        ; shape under cursor
        jsr waitForUserKey              ; flash cursor while waiting for input
        rts

boardEditorCmdKeyTbl
        .byte KEY_CODE_J                ; J: cursor left
        .byte KEY_CODE_I                ; I: cursor up
        .byte KEY_CODE_K                ; K: cursor right
        .byte KEY_CODE_M                ; M: cursor down
        .byte KEY_CODE_S+$80            ; <Ctrl>-S: save level
        .byte KEY_CODE_F+$80            ; <Ctrl>-F: board forward
        .byte KEY_CODE_B+$80            ; <Ctrl>-B: board back
        .byte KEY_CODE_Q+$80            ; <Ctrl>-Q: editor quit
        .byte $00                       ; end of list

boardEditorCmdJmpTbl
        .word editorCursorLeft-1
        .word editorCursorUp-1
        .word editorCursorRight-1
        .word editorCursorDown-1
        .word editorSaveBoard-1
        .word editorBoardForward-1
        .word editorBoardBack-1
        .word editorQuit-1

inputLevelChecked                       ; let the user enter a three digit level number
                                        ; Return: Y = level number (0-based), Carry set: input error
        ldy currentLevel                ; current level (0-based)
        iny                             ; inc level for display
        tya
        jsr convertHexToDecimal         ; convert hex number to 3 digit decimal
        lda zpCursorCol
        sta cursorColDigit0             ; store cursor pos at first of level digit

        ldy #$00                        ; index: digit to print
_printLevelL
        lda digitHigh,y
        sty digitEntryPos               ; position of digit to be entered
        jsr printDigit
        ldy digitEntryPos
        iny
        cpy #$03                        ; print three digits
        bcc _printLevelL

        lda cursorColDigit0             ; restore cursor pos
        sta zpCursorCol
        ldy #$00                        ; next entry: first digit
        sty digitEntryPos

_readDigitLoop
        ldx digitEntryPos
        lda digitHigh,x
        clc
        adc #$3b                        ; convert to "lr" encoded digit

        jsr waitForUserKey
        jsr checkKeyDigit               ; check whether user pressed a digit key
        bcc _keyIsDigit                 ; yes -> store it, print it, continue
        cmp #KEY_CODE_RETURN            ; keyboard code = <Return> ?
        beq _keyIsReturn                ; yes ->
        cmp #KEY_CODE_UP_DOWN           ; cursor key up/down: handle as backspace
        beq _readDigitBackspace
        cmp #KEY_CODE_CTRL+KEY_CODE_LEFT_RIGHT  ; <ctrl>-cursor left
        bne _checkCursorRight
_readDigitBackspace
        ldx digitEntryPos
        beq _unexpectedKey              ; can't move cursor left ->
        dec digitEntryPos
        dec zpCursorCol
        jmp _readDigitLoop

_checkCursorRight
        cmp #KEY_CODE_LEFT_RIGHT        ; cursor key left/right? (acts as cursor right)
        bne _checkKeyEscape
        ldx digitEntryPos
        cpx #$02                        ; max digit entry position: 2
        beq _unexpectedKey              ; can't move cursor right ->
        inc zpCursorCol                 ; increase digit entry position (move cursor to right)
        inc digitEntryPos
        jmp _readDigitLoop
_checkKeyEscape
        cmp #KEY_CODE_RUN_STOP
        bne _checkKeyDigit
        jmp returnToEditorMenu
_checkKeyDigit
        jsr checkKeyDigit               ; check whether user pressed a digit key
        bcs _unexpectedKey              ; no ->
_keyIsDigit
        ldy digitEntryPos
        sta digitHigh,y
        jsr printDigit
        inc digitEntryPos
        lda digitEntryPos
        cmp #$03                        ; digit entry position < 3?
        bcc _readDigitLoop              ; yes -> enter next digit

        dec digitEntryPos               ; step back, in order to read digit again
        dec zpCursorCol
        jmp _readDigitLoop              ; reread this digit

_unexpectedKey
        jsr soundBell
        jmp _readDigitLoop

_keyIsReturn
        lda cursorColDigit0
        clc
        adc #$03                        ; position cursor after 3 digit level number
        sta zpCursorCol
        lda #$00                        ; init converted level number
        ldx digitHigh                   ; level number * 100
        beq _convertMidDigit
        clc
_decToBinLoopHigh
        adc #100                        ; convert to binary representation
        bcs _returnBadLevelNumber       ; overflow -> level number is too big
        dex                             ; dec level number * 100
        bne _decToBinLoopHigh

_convertMidDigit
        ldx digitMid
        beq _convertLowDigit
        clc
_decToBinLoopMid
        adc #10                         ; convert to binary representation
        bcs _returnBadLevelNumber       ; overflow -> level number is too big
        dex                             ; dec level number * 10
        bne _decToBinLoopMid

_convertLowDigit
        clc
        adc digitLow                    ; add level number * 1 digit
        bcs _returnBadLevelNumber       ; overflow -> level number is too big
        sta displayedLevel              ; current level (displayed)
        tay
        dey
        sty currentLevel                ; current level (0-based)
        cpy #BOARD_MAXIMUM+1            ; 150 / $95+1
_returnBadLevelNumber
        rts

waitForKeyboardInput
        lda #$00                        ; shape under cursor (blank)
        jsr waitForUserKey              ; flash cursor while waiting for input
        ldx #$00
        stx keyboardCode                ; keyboard matrix code (no key: 0)
        cmp #KEY_CODE_RUN_STOP
        bne _finish
        jmp returnToEditorMenu
_finish
        rts

digitEntryPos
initialsEntryPos
        .byte $00                       ; position of digit to be entered

cursorColDigit0
        .byte $00                       ; cursor column of first digit to be entered

selectedLevel1
        .byte $00

selectedLevel2
        .byte $00

checkKeyDigit                           ; check whether user pressed a digit key
        lda keyboardCode                ; keyboard matrix code (no key: 0)
        ldy #$00                        ; clear last key stroke
        sty keyboardCode
        ldy #$09                        ; iterate over all digits 9..0
_checkDigitsLoop
        cmp keyCodesDigitsTab,y         ; compare with key code for digit y
        beq _digitKeyPressed
        dey
        bpl _checkDigitsLoop
        sec                             ; mark digit not found
        rts
_digitKeyPressed
        tya                             ; return digit key that has been pressed
        clc                             ; mark as found
        rts


keyCodesDigitsTab
        .byte KEY_CODE_0, KEY_CODE_1
        .byte KEY_CODE_2, KEY_CODE_3
        .byte KEY_CODE_4, KEY_CODE_5
        .byte KEY_CODE_6, KEY_CODE_7
        .byte KEY_CODE_8, KEY_CODE_9

strDaneBigham
        .text "DANE BIGHAM"

handleHighScoresEntry
        lda allowHighScoreEntry         ; 0: player cheated, no high score entry, 1: did not cheat
        beq _exit
        lda score+0
        ora score+1
        ora score+2
        ora score+3
        beq _exit
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores          ; load high score from disk
        beq _exit                       ; DISK_UNKNOWN? ->

        ldy #$00
_copyHighScoreDataL
        lda highScoreData,y
        sta highScoreDataBackup,y
        iny
        bne _copyHighScoreDataL

        ldy #$01
_findHighScoreEntryL
        ldx tabHighScoreOffsets,y       ; offset of high score Y
        lda displayedLevel              ; current level (displayed)
        cmp highScoreData+3,x           ; entry: level
        bcc _continueNextEntry          ; level < level in entry? ->
        bne newHighScoreIndexFound      ; level > level in entry? ->
        ; same level as current entry
        lda score+3                     ; current score (highest)
        cmp highScoreData+4,x           ; entry: score
        bcc _continueNextEntry          ; score < score in entry? ->
        bne newHighScoreIndexFound      ; score > score in entry? ->
        lda score+2                     ; current score (next to highest)
        cmp highScoreData+5,x           ; entry: score
        bcc _continueNextEntry
        bne newHighScoreIndexFound
        lda score+1                     ; current score (next to lowest)
        cmp highScoreData+6,x           ; entry: score
        bcc _continueNextEntry
        bne newHighScoreIndexFound
        lda score+0                     ; current score (lowest)
        cmp highScoreData+7,x           ; entry: score
        bcc _continueNextEntry
        bne newHighScoreIndexFound
_continueNextEntry
        iny
        cpy #11
        bcc _findHighScoreEntryL
_exit
        rts

newHighScoreIndexFound
        cpy #10                         ; is new entry last entry in high score list?
        beq _updateHighScoreRecord      ; yes ->
        sty zpHighScoreEntryOffset      ; offset of current high score entry
        ldy #$09
_moveHighScoreRecordsL
        ldx tabHighScoreOffsets,y       ; get offset of high score record y
        lda #$08
        sta zpHighScoreIter             ; iterator for moving high scores
_copyRecordL
        lda highScoreData,x             ; copy high score record y
        sta highScoreData+8,x           ; to record y+1
        inx
        dec zpHighScoreIter
        bne _copyRecordL
        cpy zpHighScoreEntryOffset      ; offset of current high score entry
        beq _updateHighScoreRecord
        dey
        bne _moveHighScoreRecordsL

_updateHighScoreRecord
        ldx tabHighScoreOffsets,y       ; offset of high score record
        lda #$a0
        sta highScoreData+0,x           ; clear initials (3 bytes)
        sta highScoreData+1,x
        sta highScoreData+2,x

        lda displayedLevel              ; current level (displayed)
        sta highScoreData+3,x           ; copy to record

        lda score+3                     ; current score
        sta highScoreData+4,x           ; copy to record (4 bytes)
        lda score+2
        sta highScoreData+5,x
        lda score+1
        sta highScoreData+6,x
        lda score+0
        sta highScoreData+7,x

        sty zpNewHighScoreEntryId
        lda tabHighScoreOffsets,y       ; offset of high score record
        sta _ldaHighScoreData+1         ; self modifying code: read from current high score record
        sta _staHighScoreData+1         ; self modifying code: write to current high score record
        jsr displayHighScores           ; display current high score table

        lda zpNewHighScoreEntryId
        clc
        adc #$04                        ; set cursor position to initials within high score record
        sta zpCursorRow
        lda #$07
        sta zpCursorCol
        ldx #$00
        stx initialsEntryPos
_inputInitialsL
        ldx initialsEntryPos
_ldaHighScoreData
        lda highScoreData,x
        jsr convertAsciiToShapeId       ; convert to shape under cursor (blank)
        jsr waitForUserKey              ; flash cursor while waiting for input
        ldx #$00
        stx keyboardCode                ; keyboard matrix code (no key: 0)
        jsr convertKeyCodeToAscii       ; convert to high ascii
        cmp #$8d                        ; ascii high: CR
        beq _finishHighScoreEntry

        cmp #$88                        ; ascii high: backspace
        bne _inputInitialsJ1            ; no ->
        ldx initialsEntryPos
        beq _inputCharBad
        dec initialsEntryPos
        dec zpCursorCol
        jmp _inputInitialsL

_inputInitialsJ1
        cmp #$95                        ; cursor right
        bne _inputInitialsJ2
        ldx initialsEntryPos
        cpx #$02                        ; at last entry position?
        beq _inputCharBad               ; yes -> ignore input
        inc zpCursorCol
        inc initialsEntryPos
        jmp _inputInitialsL

_inputInitialsJ2
        cmp #'.'                        ; ascii high: $ae
        beq _inputCharOk
        cmp #' '                        ; ascii high: $a0
        beq _inputCharOk
        cmp #'A'                        ; ascii high: $c1
        bcc _inputCharBad               ; < 'A'? -> bad character
        cmp #'Z'+1                      ; ascii high: $db
        bcs _inputCharBad               ; > 'Z'? -> bad character
_inputCharOk
        ldy initialsEntryPos
_staHighScoreData
        sta highScoreData,y             ; store initials character in high score data
        jsr printChar                   ; print character
        inc initialsEntryPos
        lda initialsEntryPos
        cmp #$03                        ; all 3 initials entered?
        bcc _inputInitialsL             ; no ->

        dec initialsEntryPos            ; backup one position
        dec zpCursorCol                 ; (allow last initial to be overwritten)
        jmp _inputInitialsL             ; continue input loop

_inputCharBad
        jsr soundBell
        jmp _inputInitialsL

_finishHighScoreEntry
        ldy #$00
_copyHighScoreDataToUpdated
        lda highScoreData,y             ; copy updated high score data
        sta highScoreDataUpdated,y      ;   to dedicated buffer
        iny
        bne _copyHighScoreDataToUpdated

        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores

        ldy #$00
_ensureSafeUpdateL
        lda highScoreData,y             ; ensure disk has not changed
        cmp highScoreDataBackup,y       ; (high score data is the same as before)
        bne _exit                       ; changes detected -> abort saving high scores
        lda highScoreDataUpdated,y
        sta highScoreData,y             ; update list of high scores in memory
        iny
        bne _ensureSafeUpdateL

        lda #DISK_CMD_SAVE              ; command: save file
        jsr loadSaveHighScores
_exit
        jmp attractStateLoop

displayHighScores
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda #(COL_YELLOW << 4 | COL_CYAN)
        jsr setColorMemory
        lda #$00
        sta zpCursorCol                 ; set cursor to (0,0)
        sta zpCursorRow
        jsr printString
        .text "    LODE RUNNER HIGH SCORES",$8d
        .text $8d
        .text $8d
        .text "    INITIALS LEVEL  SCORE",$8d
        .text "    -------- ----- --------",$8d,$00

        lda #$01
        sta zpHighScoreEntryNumber      ; number of current high score entry (1-10)
_displayHighScoresL
        cmp #10                         ; last entry (10)?
        bne _displayHighScoresJ1        ; no ->
        lda #$01                        ; print "10" (high score entry 10)
        jsr printDigit
        lda #$00
        jsr printDigit
        jmp _displayHighScoresJ2

_displayHighScoresJ1                    ; high score entry < 10:
        lda #' '                        ; only one digit: print space
        jsr printChar                   ; print character
        lda zpHighScoreEntryNumber      ; number of current high score entry (1-10)
        jsr printDigit                  ; print the entry number

_displayHighScoresJ2
        jsr printString
        .text ".    ",$00
        ldx zpHighScoreEntryNumber      ; number of current high score entry (1-10)
        ldy tabHighScoreOffsets,x       ; fetch data offset for current entry
        sty zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+3,y           ; record: level
        bne _displayHighScoresJ3        ; level > 0 -> display record
        jmp _continueNextRecord         ; else, skip this record

_displayHighScoresJ3
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+0,y           ; record: initials, 1st character
        jsr printChar                   ; print character
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+1,y           ; record: initials, 2nd character
        jsr printChar                   ; print character
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+2,y
        jsr printChar                   ; print character

        jsr printString                 ; print spacer
        .text "    ",$00

        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+3,y           ; record: level
        jsr convertHexToDecimal         ; convert hex number to 3 digit decimal
        lda digitHigh
        jsr printDigit                  ; print three digits of level
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit

        jsr printString                 ; print spacer
        .text "  ",$00

        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+4,y
        jsr splitBcdIntoDigits          ; print first two digits of score
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+5,y
        jsr splitBcdIntoDigits          ; print second two digits of score
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+6,y
        jsr splitBcdIntoDigits          ; print third two digits of score
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+7,y
        jsr splitBcdIntoDigits          ; print last two digits of score
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit

_continueNextRecord
        jsr printNewline
        inc zpHighScoreEntryNumber      ; inc number of current high score entry
        lda zpHighScoreEntryNumber      ; number of current high score entry (1-10)
        cmp #11                         ; repeat for 10 high score entries
        bcs _exit                       ; finished ->
        jmp _displayHighScoresL         ; print next entry
_exit
        rts

tabHighScoreOffsets
        .byte $00,$00,$08,$10,$18,$20,$28,$30,$38,$40,$48

convertKeyCodeToAscii
        cmp #$82                        ; <SHIFT>-left (?)
        bne _convertJ1
        lda #$07                        ; cursor up -> converts to backspace
_convertJ1
        tay
        lda tabKeyCodeToAscii,y
        ora #$80                        ; convert to high ascii
        rts

tabKeyCodeToAscii
.enc "none"
        .text $00,$0d,$15,$ff,$ff,$ff,$ff,$08,"3WA4ZSE",$ff
        .text "5RD6CFTX7YG8BHUV"
        .text "9IJ0MKON+PL-.:",$ff,","
        .text "\*;",$ff,$ff,"=",$ff,"/1",$ff,$ff,"2 ",$ff,"Q",$ff

setupBoard
        stx boardRequiresFullInit       ; 0: board has been initialized before, != 0: board requires full init
        ldx #$ff                        ; init value: player not found yet
        stx zpPlayerX                   ; player, X position on board
        inx                             ; x = 0
        stx numExitLadders              ; number of exit ladders = 0
        stx goldLeft                    ; gold left = 0
        stx numEnemies                  ; enemies left = 0
        stx enemyIndex                  ; current enemy, index = 0
        stx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        stx zpPackedDataIdx             ; index into packed board data = 0
        stx zpNibbleSelect              ; next nibble to unpack
        stx zpCursorRow
        txa                             ; A = 0 (init value for array inits below)
        ldx #MAX_HOLES_OPEN
_initOpenHoleTimersL
        sta holesOpenCtr,x              ; all holes closed (timer = 0)
        dex
        bpl _initOpenHoleTimersL

        ldx #MAX_NUM_ENEMIES            ; iterate over max number of enemies (5)
_initEnemiesRespawnCtrsL
        sta enemiesRespawnCtr,x         ; time left until enemy respawns
        dex
        bpl _initEnemiesRespawnCtrsL

        lda #PLAYER_ALIVE               ; 1: player alive
                                        ;    operation: load current board from board set
        sta playerAlive                 ; 0: player dead, 1: player alive

        ; unpack board
        lda currentLevel                ; current level (0-based)
        cmp reloadBoard                 ; force loading board if != currentLevel
        beq _unpackBoard
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadStoreBoard              ; load from (1) or store (2) board in board set
_unpackBoard
        lda currentLevel                ; current level (0-based)
        sta reloadBoard                 ; force loading board if != currentLevel
        ldy zpCursorRow

_unpackBoardRowLoop
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        lda #$00
        sta zpCursorCol
_unpackBoardColumnLoop
        lda zpNibbleSelect
        lsr                             ; even/odd bit into carry -> handle low/high nibble
        ldy zpPackedDataIdx             ; index into packed board data
        lda boardPacked,y               ; current board, packed by combining two shapes in one byte
        bcs _handleHighNibble
_handleLowNibble
        and #$0f                        ; unpack shape from low nibble
        bpl _handleShape
_handleHighNibble
        lsr                             ; unpack shape from high nibble
        lsr
        lsr
        lsr
        inc zpPackedDataIdx             ; inc. index into packed board data

_handleShape
        inc zpNibbleSelect              ; toggle between low and high nibble
        ldy zpCursorCol
        cmp #SHAPE_INVALID              ; test for valid shape $00-$09
        bcc _writeShapeToBoards
        lda #SHAPE_BLANK                ; replace invalid shape with SHAPE_BLANK
_writeShapeToBoards
        sta (zpBoardActionPtr),y        ; store shape in unpacked buffer
        sta (zpBoardLayoutPtr),y
        inc zpCursorCol
        lda zpCursorCol
        cmp #BOARD_WIDTH
        bcc _unpackBoardColumnLoop

        inc zpCursorRow
        ldy zpCursorRow
        cpy #BOARD_HEIGHT
        bcc _unpackBoardRowLoop

        jsr initBoardState              ; initialize the board state (player, #enemies, #ladders etc)
        bcc _initBoardOk                ; initialization OK -> board is ready to be played
        lda currentLevel                ; current level (0-based)
        beq _noPlayableLevel            ; first level unplayable? -> FUBAR, reset the game!
        ldx #$00                        ; else, revert back to first level (0)
        stx currentLevel                ; current level (0-based)
        lda enemySpeed                  ; enemy speed (0-10) at maximum?
        cmp #10                         ; yes -> keep it there
        bcs _initBoardForceSetup
        inc enemySpeed                  ; enemy speed (0-10)
_initBoardForceSetup
        dex                             ; X != 0: board requires full init
        jmp setupBoard
_initBoardOk
        rts

_noPlayableLevel
        jmp resetGame

packBoard                               ; pack board in preparation for saving
        lda #$00
        sta zpPackedDataIdx             ; index into packed board data
        sta zpCombinedNibbles           ; start packing bytes with low nibble
        sta zpCursorRow

_packBoardRowsL
        ldy zpCursorRow
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy #$00
        sty zpCursorCol
_packBoardColumnsL
        lda zpCombinedNibbles           ; counted up from zero, bit 0 alternates
        lsr                             ; check whether bit 0 is set
        lda (zpBoardActionPtr),y        ; read a shape from current editor board
        bcs _packHighNibble             ; bit 0 of zpCombinedNibbles is 1 -> pack into high nibble
        sta zpLowNibble                 ; low nibble for combining packed byte
        bpl _packNextNibble
_packHighNibble
        asl
        asl
        asl
        asl
        ora zpLowNibble                 ; combine with low nibble from previous iteration
        ldy zpPackedDataIdx             ; index into packed board data
        sta boardPacked,y               ; current board, packed by combining two shapes in one byte
        inc zpPackedDataIdx             ; inc. index into packed board data
_packNextNibble
        inc zpCombinedNibbles           ; inc. destination nibble selector
        inc zpCursorCol
        ldy zpCursorCol
        cpy #BOARD_WIDTH
        bcc _packBoardColumnsL
        inc zpCursorRow
        lda zpCursorRow
        cmp #BOARD_HEIGHT
        bcc _packBoardRowsL

        lda #$02                        ; constant: store the board into board set
        jmp loadStoreBoard              ; load from (1) or store (2) board in board set


loadStoreBoard                          ; load from (1) or store (2) board in board set
        pha                             ; store disk operation request
        bpl _loadStoreBoardJ1           ; disk operation request > 0? ->
        pla                             ; restore disk operation request
        and #$7f                        ; make it positive
        pha                             ; push disk operation request
        jmp executeDiskCommand

_loadStoreBoardJ1
        lda gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        lsr                             ; starting up demo mode?
        beq loadDemoBoard               ; yes -> load a demo board
        jmp loadStoreBoardFromDisk      ; load or store board from disk

loadDemoBoard
        pla                             ; restore disk operation request
        lda currentLevel                ; current level (0-based)
        cmp #$03                        ; > maximum demo level?
        bcc _loadStoreBoardJ1           ; no ->
        lda #$00                        ; reset demo level to 0
        sta currentLevel                ; current level (0-based)
        inc enemySpeed                  ; increment enemy speed (0-10)
        ldx enemySpeed
        cpx #10                         ; enemy speed < 10 (maximum)?
        bcc _loadStoreBoardJ1           ; yes ->
        dec enemySpeed                  ; enemy speed (0-10)
_loadStoreBoardJ1
        clc
        adc #>demoGameBoards            ; page for first demo game board
        sta _ldaDemoGameBoards+2        ; self modifying code: adjust page
        ldy #$00                        ; index: board data
_copyDemoBoardL
_ldaDemoGameBoards
        lda demoGameBoards,y            ; copy demo game board
        sta boardPacked,y               ; into current board (packed)
        iny                             ; inc board data index
        bne _copyDemoBoardL
        rts

loadSaveHighScores
        tax                             ; disk command
        lda currentLevel                ; current level (0-based)
        pha                             ; store current level
        inc boardLoadPage               ; adjust data pointer page
        inc boardSavePage               ; adjust data pointer page
        lda #151                        ; store high scores in board entry 151
        sta currentLevel                ; current level (0-based)
        txa                             ; disk command
        jsr loadStoreBoard              ; load from (1) or store (2) board in board set
        pla                             ; restore current level
        sta currentLevel                ; current level (0-based)
        dec boardLoadPage               ; adjust data pointer page
        dec boardSavePage               ; adjust data pointer page

        ldy #$0a
        lda #$00
        sta zpSignatureChecksum         ; init signature checksum
_checkDiskSignatureL
        lda highScoreData+$f0,y         ; compare signature in high score data
        eor strDaneBigham,y             ; with string "DANE BIGHAM"
        ora zpSignatureChecksum         ; update signature checksum
        sta zpSignatureChecksum         ; 0: signature found, else: not found
        dey
        bpl _checkDiskSignatureL

        lda zpSignatureChecksum         ; 0: signature found, else: not found
        beq _signatureFound             ; signature found? ->
        lda #$00                        ; return: found unsupported disk
        rts

_signatureFound
        lda #$01                        ; return: found master disk
        ldx highScoreData+$fb           ; check known disk type (0: signature for user disk)
        bne _exit                       ; master disk -> return 1
        lda #$ff                        ; return: found user disk
_exit
        rts


loadStoreBoardFromDisk
        lda #$00
        sta VicSpriteEnable             ; turn off sprites
        jsr CLALL

        ; calculate disk sector from level to load
        ldx #'0'                        ; default: high sector = "0"
        lda currentLevel                ; current level (0-based)
        and #$0f                        ; isolate low nibble -> sector
        cmp #10                         ; sector < 10?
        bcc _setSector                  ; yes ->
        ldx #'1'                        ; else, high sector = 1
        sec
        sbc #10                         ; calculate low sector in range 0..9
_setSector
        stx sector+0                    ; set high sector ("0" or "1")
        clc
        adc #'0'                        ; convert low sector to ascii
        sta sector+1                    ; set low sector ("0".."9")

        ; calculate disk track from level to load
        lda currentLevel                ; current level (0-based)
        lsr                             ; isolate high nibble -> track
        lsr
        lsr
        lsr
        clc
        adc #$03                        ; board data starts at track 3
        ldx #'0'                        ; default: high track = "0"
        cmp #10                         ; track < 10?
        bcc _setTrack                   ; yes ->
        ldx #'1'                        ; else, high track = 1
        sec
        sbc #10                         ; calculate low track in range 0..9
_setTrack
        stx track+0                    ; set high track ("0" or "1")
        clc
        adc #'0'                        ; convert low track to ascii
        sta track+1                     ; set low track ("0".."9")


executeDiskCommand
        lda #$00
        jsr SETNAM                      ; setnam ""
        lda #$0f
        ldx #$08
        ldy #$0f
        jsr SETLFS                      ; setlfs
        jsr OPEN                        ; open 15,8,15,"" (command channel)
        bcs diskError
        lda #$01
        ldx #<filenameDataChannel
        ldy #>filenameDataChannel
        jsr SETNAM                      ; setnam "#"
        lda #$02
        ldx #$08
        ldy #$02
        jsr SETLFS                      ; setlfs 2,8,2
        jsr OPEN                        ; open 2,8,2,"#" (data channel)
        bcs diskError
        pla                             ; restore disk operation request
        lsr
        bcs loadBoardFromDisk           ; (1) load from disk? ->
        lsr
        bcc _initUserDisk               ; (4) initialize user disk? ->
        jmp saveBoardToDisk             ; (2) save to disk ->

_initUserDisk
        ldy #$00                        ; clear value: $00
        tya
_clearBoardL
        sta highScoreData,y             ; init high score list data
        sta boardPacked,y               ; init current board (packed)
        iny                             ; (clear a whole page each)
        bne _clearBoardL

        lda #BOARD_MAXIMUM              ; (149)
        sta currentLevel                ; iterator: current level (0-based)
        jsr CLALL
_initBoardsL
        lda #DISK_CMD_SAVE              ; command: save file
        jsr loadStoreBoard              ; load from (1) or store (2) board in board set
        dec currentLevel                ; current level (0-based)
        lda currentLevel                ; current level (0-based)
        cmp #$ff                        ; write boards 0..149
        bne _initBoardsL

        ldy #$0a
_setDiskSignatureL
        lda strDaneBigham,y
        sta highScoreData+$f0,y
        dey
        bpl _setDiskSignatureL

        iny                             ; inc y to 0 (signature for user disk)
        sty highScoreData+$fb           ; indicator: user data disk

        lda #DISK_CMD_SAVE              ; command: save file
        jsr loadSaveHighScores          ; save the high score file to the user disk
        rts

diskError
        jmp displayTitleScreen

loadBoardFromDisk
        lda #'1'                        ; prepare "U1" command
        sta userCommandStr+1
        jsr CLRCHN                      ; clrchn
        ldx #$0f
        jsr CHKOUT                      ; chkout 15
        bcs diskError
        ldy #$00
_sendU1CommandL
        lda userCommandStr,y            ; output "U1:02 0 03 00",$0d
        beq _loadBoardJ1
        jsr CHROUT
        iny
        jmp _sendU1CommandL

_loadBoardJ1
        jsr checkDriveCommandResult
        jsr CLRCHN                      ; clrchn
        ldx #$02
        jsr CHKIN                       ; chkin 2 (set data channel as input)
        bcs diskError
        jsr CHRIN                       ; read byte from disk buffer (first byte is discarded!)
        ldy #$00
_loadBoardL
        jsr CHRIN                       ; read byte from disk buffer
boardLoadPage = * + 2
        sta boardPacked,y               ; store it in packed board buffer
        iny
        bne _loadBoardL
        jmp reInitDrive


saveBoardToDisk
        lda #'2'                        ; prepare "U2" command
        sta userCommandStr+1
        jsr CLRCHN                      ; clrchn
        ldx #$02
        jsr CHKOUT                      ; chkout 2
        bcs diskError

        ldy #$00                        ; index
_writeBoardDataL
boardSavePage = * + 2                   ; modified by callers
        lda boardPacked,y               ; read byte from memory
        jsr CHROUT                      ; write byte to disk buffer
                                        ; NOTE: first byte written is $0d (why?)
        iny
        bne _writeBoardDataL            ; write one page of data

        jsr CLRCHN                      ; clrchn
        ldx #$0f
        jsr CHKOUT                      ; chkout 15

        ldy #$00
_sendU2CommandL
        lda userCommandStr,y            ; output "U2:02 0 03 00",$0d
        beq _saveBoardJ1
        jsr CHROUT
        iny
        jmp _sendU2CommandL

_saveBoardJ1
        jsr checkDriveCommandResult
        jmp reInitDrive

        .byte $00                       ; unused?

filenameDataChannel
        .text "#"

userCommandStr
        .text "U1:02 0 "                ; "U1:02 0 03 00",$0d,$00
track
        .text "03 "
sector
        .text"00",$0d,$00

uPlusCommandStr
        .text "U+",$0d,$00


checkDriveCommandResult
        jsr CLRCHN                      ; clrchn
        ldx #$0f
        jsr CHKIN                       ; chkin 15
        jsr CHRIN                       ; read status byte 0
        sta zpDriveStatus               ; combined drive status: '0': OK
        jsr CHRIN                       ; read status byte 1
        ora zpDriveStatus               ; combine drive status
        sta zpDriveStatus               ; combined drive status: '0': OK
_skipStatusMessageBytesL
        jsr CHRIN                       ; read status byte 1
        cmp #$0d                        ; end of message?
        bne _skipStatusMessageBytesL    ; no ->

        lda zpDriveStatus               ; combined drive status: '0': OK
        cmp #'0'
        beq _exitOk
        jmp resetGame
_exitOk
        rts


reInitDrive                             ; re-initialize disk drive
        jsr CLRCHN                      ; clrchn
        ldx #$0f
        jsr CHKOUT                      ; chkout 15

        ldy #$00
_sendUPlusCommandL
        lda uPlusCommandStr,y
        beq _exit
        jsr CHROUT                      ; chrout
        iny
        jmp _sendUPlusCommandL
_exit
        jmp CLALL                       ; clall


initBoardState                          ; initialize the board state (player, #enemies, #ladders etc)
        ; iterate over the board in order to find the player position,
        ; set up the enemies, initialize exit ladders, count gold etc.
        ldy #BOARD_HEIGHT-1             ; set up row iterator
        sty zpCursorRow
        lda #$00                        ; disable sprites
        sta VicSpriteEnable
_initBoardRowL
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy #BOARD_WIDTH-1              ; set up column iterator
        sty zpCursorCol
_initBoardColL
        lda (zpBoardActionPtr),y        ; get action tile at current board location
        ldx boardRequiresFullInit       ; 0: board has been initialized before, != 0: board requires full init
        beq _justDrawTile               ; board state already initialized -> just draw the tile
        cmp #SHAPE_EXIT_LADDER          ; exit ladder?
        bne _notExitLadder              ; no ->
        ldx numExitLadders              ; number of exit ladders
        cpx #MAX_NUM_EXIT_LADDERS       ; maximum reached?
        bcs _substituteBlank            ; yes -> skip this ladder
        inc numExitLadders              ; number of exit ladders
        inx
        lda zpCursorRow
        sta exitLaddersY,x              ; store X coordinate of exit ladder
        tya
        sta exitLaddersX,x              ; store Y coordinate of exit ladder
_substituteBlank
        lda #SHAPE_BLANK                ; substitute exit ladder with blank tile
        sta (zpBoardActionPtr),y
        sta (zpBoardLayoutPtr),y

_justDrawTile
        beq _printTileAndContinue

_notExitLadder
        cmp #SHAPE_GOLD_CHEST           ; gold chest?
        bne _notGold                    ; no ->
        inc goldLeft                    ; inc # of gold to collect
        bne _printTileAndContinue

_notGold
        cmp #SHAPE_ENEMY                ; enemy?
        bne _notEnemy                   ; no ->
        ldx numEnemies                  ; enemy limit reached? (max. 5 enemies)
        cpx #MAX_NUM_ENEMIES            ; (max. 5 enemies)
        bcs _substituteBlank            ; yes -> skip this enemy
        inc enemyIndex                  ; current enemy, index
        inc numEnemies                  ; add the new enemy object
        inx                             ; inc enemy index
        tya                             ; cursor column
        sta enemiesX,x                  ; x position (course)
        lda zpCursorRow
        sta enemiesY,x                  ; y position (course)
        lda #$00
        sta enemiesActionCtr,x          ; enemy does not carry gold, is not trapped
        sta enemiesAnimPhase,x          ; enemy animation phase (EN_ANIM_PHASE_RUN_LEFT_0)
        lda #STEP_MIDDLE
        sta enemiesStepX,x              ; x position (fine)
        sta enemiesStepY,x              ; y position (fine)
        lda #SHAPE_BLANK
        sta (zpBoardLayoutPtr),y        ; replace enemy by blank in the layout board
        lda #SHAPE_BLANK
        jsr replaceTileBitmap1
        lda #SHAPE_ENEMY
        bne _printTileAndContinue

_initBoardRowLBranch                    ; (branch extension row loop)
        bpl _initBoardRowL

_initBoardColLBranch                    ; (branch extension column loop)
        bpl _initBoardColL

_notEnemy
        cmp #SHAPE_PLAYER               ; player?
        bne _notPlayer                  ; no ->
        ldx zpPlayerX                   ; player, X position on board
        bpl _substituteBlank            ; if already set -> ignore
        sty zpPlayerX                   ; set initial player X position on board
        ldx zpCursorRow
        stx zpPlayerY                   ; set initial player Y position on board
        ldx #STEP_MIDDLE
        stx zpPlayerStepX               ; player, X position (fine)
        stx zpPlayerStepY               ; player, Y position (fine)
        ldx #ANIM_PHASE_RUN_RIGHT_0
        stx zpPlayerAnimPhase           ; player animation phase
        lda #SHAPE_BLANK
        sta (zpBoardLayoutPtr),y        ; replace player by blank in the layout board
        lda #SHAPE_BLANK
        jsr replaceTileBitmap1
        lda #SHAPE_PLAYER
        bne _printTileAndContinue

_notPlayer
        cmp #SHAPE_TRAP_DOOR            ; trap door?
        bne _printTileAndContinue       ; no ->
        lda #SHAPE_FLOOR_DIG            ; substitute trap door by diggable floor shape

_printTileAndContinue
        jsr replaceTileBitmap1          ; draw tile
        dec zpCursorCol                 ; iterate over columns
        ldy zpCursorCol
        bpl _initBoardColLBranch

        dec zpCursorRow                 ; iterate over rows
        ldy zpCursorRow
        bpl _initBoardRowLBranch

        lda boardRequiresFullInit       ; 0: board has been initialized before, != 0: board requires full init
        beq _copyBufferedBitmap         ; copy Bitmap1 (buffer) to Bitmap0
        lda zpPlayerX                   ; player, X position on board
        bpl _initBoardQuickRedraw
        sec
        rts

_copyBufferedBitmap
        lda #>Bitmap0
        sta zpBmpPtr1+1
        lda #>Bitmap1
        sta zpBmpPtr0+1
        lda #<Bitmap0
        sta zpBmpPtr1+0
        sta zpBmpPtr0+0
        tay
_copyBitmapL
        lda (zpBmpPtr0),y
        sta (zpBmpPtr1),y
        iny
        bne _copyBitmapL                ; copy complete page

        inc zpBmpPtr1+1
        inc zpBmpPtr0+1
        ldx zpBmpPtr0+1
        cpx #>Bitmap1+$2000             ; copied all pages?
        bcc _copyBitmapL                ; no ->

        clc
        rts

_skipIrisAnimation
        lda #>Bitmap1                   ; print to bitmap 1 (buffer, hidden)
        sta zpBitmapPage2               ; active bitmap page for printing
        jsr printBoardFooter            ; draw solid ground and print status line
        jsr _copyBufferedBitmap         ; copy Bitmap1 (buffer) to Bitmap0
        lda #>Bitmap0                   ; print to bitmap 0
        sta zpBitmapPage2               ; active bitmap page for printing
        bne _initBoardQuickRedrawJ1

_initBoardQuickRedraw
        lda irisAnimationOn             ; $ff: iris animation on, $00: turned off
        beq _skipIrisAnimation
        jsr doIrisAnimation             ; do the iris animation effect

_initBoardQuickRedrawJ1
        ldy #BOARD_HEIGHT-1
        sty zpCursorRow
_rowL
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy #BOARD_WIDTH-1
        sty zpCursorCol
_colL
        lda (zpBoardActionPtr),y
        cmp #SHAPE_PLAYER
        beq _replaceWithBlank
        cmp #SHAPE_ENEMY
        bne _continue
_replaceWithBlank
        lda #SHAPE_BLANK
        jsr replaceTileBitmap1
_continue
        dec zpCursorCol
        ldy zpCursorCol
        bpl _colL
        dec zpCursorRow
        ldy zpCursorRow
        bpl _rowL
        clc
        rts


handlePlayer
        lda #$01                        ; constant: no gold picked up
        sta playerNoGoldPickedUp        ; 0: gold pick up in process, 1: no gold picked up
        lda playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        beq _playerNotDigging
        bpl _playerDigRight
        jmp continuePlayerDigLeft       ; continue digging left
_playerDigRight
        jmp continuePlayerDigRight      ; continue digging right

_playerNotDigging
        ldy zpPlayerY                   ; player, Y position on board
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y        ; read board at player's current position
        cmp #SHAPE_LADDER               ; player on a ladder?
        beq handlePlayerControllable    ; yes -> (can walk)
        cmp #SHAPE_BAR                  ; player on a bar?
        bne _checkPlayerFalling
        lda zpPlayerStepY               ; player, Y position (fine)
        cmp #STEP_MIDDLE                ; player on level with the bar?
        beq handlePlayerControllable    ; yes -> player is controllable

_checkPlayerFalling
        lda zpPlayerStepY               ; player, Y position (fine)
        cmp #STEP_MIDDLE                ; Y position above center of tile?
        bcc handlePlayerFall            ; yes -> player falling
        ldy zpPlayerY                   ; player, Y position on board
        cpy #BOARD_HEIGHT - 1           ; player in last row of the board?
        beq handlePlayerControllable    ; yes -> player is controllable
        lda boardActionOffsetLb+1,y     ; get board pointers one pos. below
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb+1,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardActionPtr),y        ; read action data in tile below player
        cmp #SHAPE_BLANK                ; tile below player empty?
        beq handlePlayerFall            ; yes -> player falls
        cmp #SHAPE_ENEMY                ; tile below player is an enemy?
        beq handlePlayerControllable    ; yes -> player can walk horizontally
        lda (zpBoardLayoutPtr),y        ; read layout data below player
        cmp #SHAPE_FLOOR_DIG
        beq handlePlayerControllable
        cmp #SHAPE_FLOOR_UNDIG
        beq handlePlayerControllable
        cmp #SHAPE_LADDER
        bne handlePlayerFall

handlePlayerControllable                ; player is not free-falling
        jmp stopPlayerFalling

handlePlayerFall
        lda #$00
        sta playerNotFalling            ; 0: player is falling, >0: not falling
        dec soundFxFallingPitch         ; sound effect player falling, current pitch
        jsr prepareDisplayPlayer
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        lda #ANIM_PHASE_FALL_LEFT
        ldx zpPlayerOrientation         ; $ff: player facing left, $01: facing right
        bmi _handlePlayerFallAnimate
        lda #ANIM_PHASE_FALL_RIGHT
_handlePlayerFallAnimate
        sta zpPlayerAnimPhase           ; set animation phase: falling (left/right)
        jsr nudgePlayerToStepXMiddle    ; nudge player towards StepX middle position
        inc zpPlayerStepY               ; player, Y position (fine)
        lda zpPlayerStepY               ; player, Y position (fine)
        cmp #STEP_MAX+1                 ; ($05)
        bcs _handlePlayerFallNextBlock  ; player falls into block below
        jsr playerCheckCollectGold      ; check for and collect gold chests
        jmp displayPlayerCheckEnemy     ; display player, check for enemy collision

_handlePlayerFallNextBlock
        lda #STEP_MIN                   ; reset Y position (fine) for new block
        sta zpPlayerStepY               ; player, Y position (fine)
        ldy zpPlayerY                   ; player, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y        ; fetch tile below player
        cmp #SHAPE_FLOOR_DIG
        bne _handlePlayerFallClearOldPos
        lda #SHAPE_BLANK                ; replace diggable floor by blank (i.e. brick is dug up)
_handlePlayerFallClearOldPos
        sta (zpBoardActionPtr),y        ; clear player shape from action board (old position)
        inc zpPlayerY                   ; increment Y position on board (coarse)
        ldy zpPlayerY                   ; player, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda #SHAPE_PLAYER
        sta (zpBoardActionPtr),y        ; store player shape in action board (new position)
        jmp displayPlayerCheckEnemy     ; display player, check for enemy collision

stopPlayerFalling
        lda playerNotFalling            ; 0: player is falling, >0: not falling
        bne _stopPlayerFallingJ1
_stopPlayerFallingJ1
        lda #$20
        sta playerNotFalling            ; 0: player is falling, >0: not falling
        jsr getPlayerControllerInput

_testInputUp
        lda inputVertical
        cmp #KEY_CODE_I                 ; 'I' (key: up)
        bne _testInputDown
        jsr handlePlayerUp              ; try to move up
        bcs _testInputLeft              ; moving up failed -> check horizontal requests
        rts
_testInputDown
        cmp #KEY_CODE_K                 ; 'K' (key: down)
        bne _testInputDigLeft
        jsr handlePlayerDown            ; try to move down
        bcs _testInputLeft              ; moving down failed -> check horizontal requests
        rts
_testInputDigLeft
        cmp #KEY_CODE_U                 ; 'U' (key: dig left)
        bne _testInputDigRight
        jsr handlePlayerDigLeft         ; try to dig left
        bcs _testInputLeft              ; dig left failed -> check horizontal requests
        rts
_testInputDigRight
        cmp #KEY_CODE_O                 ; 'O' (key: dig right)
        bne _testInputLeft
        jsr handlePlayerDigRight        ; try to dig right
        bcs _testInputLeft              ; dig right failed -> check horizontal requests
        rts
_testInputLeft
        lda inputHorizontal
        cmp #KEY_CODE_J                 ; 'J' (key: left)
        bne _testInputRight
        jmp handlePlayerLeft            ; try to move left
_testInputRight
        cmp #KEY_CODE_L                 ; 'L' (key: right)
        bne _testInputExit
        jmp handlePlayerRight           ; try to move right
_testInputExit
        rts                             ; no input detected ->


handlePlayerLeft
        ldy zpPlayerY                   ; player, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldx zpPlayerStepX               ; player, X position (fine)
        cpx #STEP_MIDDLE+1              ; player right of tile middle?
        bcs _handlePlayerLeftOk         ; yes ->
        ldy zpPlayerX                   ; player, X position on board
        beq _exitNoMove                 ; already in leftmost position ->
        dey                             ; dec. x-pos probe position
        lda (zpBoardActionPtr),y        ; get board character left of player
        cmp #SHAPE_FLOOR_UNDIG          ; check for solid objects left of player
        beq _exitNoMove                 ; and deny movement if applicable
        cmp #SHAPE_FLOOR_DIG
        beq _exitNoMove
        cmp #SHAPE_TRAP_DOOR
        bne _handlePlayerLeftOk
_exitNoMove
        rts

_handlePlayerLeftOk
        jsr prepareDisplayPlayer
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        lda #PLAYER_FACING_LEFT         ; ($ff)
        sta zpPlayerOrientation         ; $ff: player facing left, $01: facing right
        jsr nudgePlayerToStepYMiddle    ; nudge player towards StepY middle position
        dec zpPlayerStepX               ; dec player X position (fine)
        bpl _handlePlayerLeftJ2         ; move left within same tile ->
        ; enter new position to the left
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y        ; read board layout tile at player's old position
        cmp #SHAPE_FLOOR_DIG
        bne _handlePlayerLeftJ1         ; preserve shape except diggable floor
        lda #SHAPE_BLANK                ; replace diggable floor by blank shape
_handlePlayerLeftJ1
        sta (zpBoardActionPtr),y        ; remove player from action board (old position)
        dec zpPlayerX                   ; dec player X position on board
        dey                             ; one position to the left
        lda #SHAPE_PLAYER
        sta (zpBoardActionPtr),y        ; set player in action board (new position)
        lda #STEP_MAX                   ; maximum step (right most step position) ($04)
        sta zpPlayerStepX               ; player, X position (fine)
        bne _handlePlayerLeftJ3

_handlePlayerLeftJ2
        jsr playerCheckCollectGold      ; check for and collect gold chests

_handlePlayerLeftJ3
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_BAR
        beq _animPlayerBarLeft
        lda #ANIM_PHASE_RUN_LEFT_0      ; animate player run left
        ldx #ANIM_PHASE_RUN_LEFT_2
        bne _handlePlayerLeftJ4
_animPlayerBarLeft
        lda #ANIM_PHASE_BAR_LEFT_0      ; animate player on bar left
        ldx #ANIM_PHASE_BAR_LEFT_2
_handlePlayerLeftJ4
        jsr updatePlayerAnimPhase       ; set/inc animation phase between min (A) and max (X)
        jmp displayPlayerCheckEnemy     ; display player, check for enemy collision


handlePlayerRight
        ldy zpPlayerY                   ; player, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldx zpPlayerStepX               ; player, X position (fine)
        cpx #STEP_MIDDLE                ; player left of tile middle
        bcc _handlePlayerRightOk        ; yes ->
        ldy zpPlayerX                   ; player, X position on board
        cpy #BOARD_WIDTH-1              ; already in rightmost position?
        beq _exitNoMove
        iny                             ; inc. x-pos probe position
        lda (zpBoardActionPtr),y        ; get board shape right of player
        cmp #SHAPE_FLOOR_UNDIG          ; check for solid objects left of player
        beq _exitNoMove                 ; and deny movement if applicable
        cmp #SHAPE_FLOOR_DIG
        beq _exitNoMove
        cmp #SHAPE_TRAP_DOOR
        bne _handlePlayerRightOk
_exitNoMove
        rts

_handlePlayerRightOk
        jsr prepareDisplayPlayer
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        lda #PLAYER_FACING_RIGHT        ; ($01)
        sta zpPlayerOrientation         ; $ff: player facing left, $01: facing right
        jsr nudgePlayerToStepYMiddle    ; nudge player towards StepY middle position
        inc zpPlayerStepX               ; inc player X position (fine)
        lda zpPlayerStepX               ; player, X position (fine)
        cmp #STEP_MAX+1                 ; ($05)
        bcc _handlePlayerRightJ2        ; move right within same tile ->
        ; enter new position to the right
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y        ; read board layout tile at player's old position
        cmp #SHAPE_FLOOR_DIG
        bne _handlePlayerRightJ1        ; preserve shape except diggable floor
        lda #SHAPE_BLANK                ; replace diggable floor by blank shape
_handlePlayerRightJ1
        sta (zpBoardActionPtr),y        ; remove player from action board (old position)
        inc zpPlayerX                   ; inc player X position on board
        iny                             ; one position to the right
        lda #SHAPE_PLAYER
        sta (zpBoardActionPtr),y        ; set player in action board (new position)
        lda #STEP_MIN                   ; minimum step (left most step position) ($00)
        sta zpPlayerStepX               ; player, X position (fine)
        beq _handlePlayerRightJ3

_handlePlayerRightJ2
        jsr playerCheckCollectGold      ; check for and collect gold chests

_handlePlayerRightJ3
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_BAR
        beq _animPlayerBarRight
        lda #ANIM_PHASE_RUN_RIGHT_0     ; animate player run right
        ldx #ANIM_PHASE_RUN_RIGHT_2
        bne _handlePlayerRightJ4
_animPlayerBarRight
        lda #ANIM_PHASE_BAR_RIGHT_0     ; animate player on bar left
        ldx #ANIM_PHASE_BAR_RIGHT_2
_handlePlayerRightJ4
        jsr updatePlayerAnimPhase       ; set/inc animation phase between min (A) and max (X)
        jmp displayPlayerCheckEnemy     ; display player, check for enemy collision


handlePlayerUp
        ldy zpPlayerY                   ; player, Y position on board
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y        ; read board layout tile at player pos
        cmp #SHAPE_LADDER               ; ladder?
        beq _handlePlayerUpCheckAbove   ; yes ->
        ldy zpPlayerStepY               ; player, Y position (fine)
        cpy #STEP_MIDDLE+1              ; <= middle position?
        bcc _exitNoMove                 ; yes -> can't move up
        ldy zpPlayerY                   ; player, Y position on board
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y        ; read board layout tile below player
        cmp #SHAPE_LADDER               ; ladder?
        beq _handlePlayerUpOk           ; yes ->

_exitNoMove
        sec                             ; return code: fail
        rts

_handlePlayerUpCheckAbove
        ldy zpPlayerStepY               ; player, Y position (fine)
        cpy #STEP_MIDDLE+1              ; <= middle position?
        bcs _handlePlayerUpOk           ; no ->
        ldy zpPlayerY                   ; player, Y position on board
        beq _exitNoMove                 ; player in top row? -> can't climb
        lda boardActionOffsetLb-1,y     ; get boardActionPtr above player
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb-1,y
        sta zpBoardActionPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardActionPtr),y        ; read action tile above player
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq _exitNoMove                 ; yes -> can't climb
        cmp #SHAPE_FLOOR_UNDIG          ; undiggable floor?
        beq _exitNoMove                 ; yes -> can't climb
        cmp #SHAPE_TRAP_DOOR            ; trap door?
        beq _exitNoMove                 ; yes -> can't climb

_handlePlayerUpOk
        jsr prepareDisplayPlayer
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        ldy zpPlayerY                   ; player, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        jsr nudgePlayerToStepXMiddle    ; nudge player towards StepX middle position
        dec zpPlayerStepY               ; player, Y position (fine)
        bpl handlePlayerUpDownJ1        ; move up within same tile ->
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y        ; read board layout tile at player's old position
        cmp #SHAPE_FLOOR_DIG
        bne _handlePlayerUpJ1           ; preserve shape except diggable floor
        lda #SHAPE_BLANK
_handlePlayerUpJ1
        sta (zpBoardActionPtr),y        ; remove player from action board (old position)
        dec zpPlayerY                   ; dec player Y position on board
        ldy zpPlayerY                   ; player, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda #SHAPE_PLAYER
        sta (zpBoardActionPtr),y        ; set player in action board (new position)
        lda #STEP_MAX                   ; maximum step position ($04)
        sta zpPlayerStepY               ; player, Y position (fine)
        bne handlePlayerUpDownJ2
handlePlayerUpDownJ1
        jsr playerCheckCollectGold      ; check for and collect gold chests

handlePlayerUpDownJ2
        lda #ANIM_PHASE_CLIMB_0
        ldx #ANIM_PHASE_CLIMB_1
        jsr updatePlayerAnimPhase       ; set/inc animation phase between min (A) and max (X)
        jsr displayPlayerCheckEnemy     ; display player, check for enemy collision
        clc
        rts

handlePlayerDown
        ldy zpPlayerStepY               ; player, Y position (fine)
        cpy #STEP_MIDDLE                ; < middle position?
        bcc _handlePlayerDownOk         ; yes ->
        ldy zpPlayerY                   ; player, Y position on board
        cpy #BOARD_HEIGHT-1             ; already in lowest position?
        bcs _exitNoMove                 ; yes -> can't move down
        lda boardActionOffsetLb+1,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb+1,y
        sta zpBoardActionPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardActionPtr),y        ; read board action tile at player pos
        cmp #SHAPE_FLOOR_UNDIG          ; undiggable floor=
        beq _exitNoMove                 ; yes -> can't move down
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        bne _handlePlayerDownOk         ; something else -> move ok
_exitNoMove
        sec
        rts

_handlePlayerDownOk
        jsr prepareDisplayPlayer
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        ldy zpPlayerY                   ; player, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        jsr nudgePlayerToStepXMiddle    ; nudge player towards StepX middle position
        inc zpPlayerStepY               ; player, Y position (fine)
        lda zpPlayerStepY               ; player, Y position (fine)
        cmp #STEP_MAX+1                 ; ($05)
        bcc _handlePlayerDownJ2         ; move down within same tile ->
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG
        bne _handlePlayerDownJ1         ; preserve shape except diggable floor
        lda #SHAPE_BLANK
_handlePlayerDownJ1
        sta (zpBoardActionPtr),y        ; remove player from action board (old position)
        inc zpPlayerY                   ; inc player Y position on board
        ldy zpPlayerY                   ; player, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda #SHAPE_PLAYER
        sta (zpBoardActionPtr),y        ; set player in action board (new position)
        lda #STEP_MIN                   ; minimum step position ($00)
        sta zpPlayerStepY               ; player, Y position (fine)
        jmp handlePlayerUpDownJ2
_handlePlayerDownJ2
        jmp handlePlayerUpDownJ1


failPlayerDigLeft
        jmp abortPlayerDigLeftJ1

handlePlayerDigLeft
        lda #$ff                        ; constant: dig left
        sta playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        sta inputVertical
        sta inputHorizontal
        lda #DIG_ANIM_PHASE_LEFT_MIN    ; start dig animation
        sta zpHoleDigAnimCtr            ; animation counter for a hole being drilled
continuePlayerDigLeft                   ; entry point: continue digging
        ldy zpPlayerY                   ; player, Y position on board
        cpy #BOARD_HEIGHT-1             ; player on bottom row?
        bcs failPlayerDigLeft           ; yes -> can't dig here
        iny
        jsr getLayoutAndActionBoardPtrs ; set board pointers below player
        ldy zpPlayerX                   ; player in leftmost position?
        beq failPlayerDigLeft           ; yes -> can't dig here
        dey
        lda (zpBoardActionPtr),y        ; fetch board action tile (dig location)
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        bne failPlayerDigLeft           ; no -> can't dig here

        ldy zpPlayerY                   ; player, Y position on board
        jsr getLayoutAndActionBoardPtrs
        ldy zpPlayerX                   ; player, X position on board
        dey
        lda (zpBoardActionPtr),y        ; fetch board action tile left of player
        cmp #SHAPE_BLANK                ; field above dig location empty?
        bne _abortPlayerDigLeft         ; no -> (enemy above dig position) -> abort dig

        jsr prepareDisplayPlayer
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        jsr nudgePlayerToStepXMiddle    ; nudge player towards StepX middle position
        jsr nudgePlayerToStepYMiddle    ; nudge player towards StepY middle position
        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        lda #ANIM_PHASE_RUN_LEFT_0      ; anim phase used when digging is finished
        cpx #DIG_ANIM_PHASE_LEFT_MID    ; digging finished?
        bcs _playerDigLeftJ1            ; yes -> switch player anim back to running stance
        lda #ANIM_PHASE_FIRE_LEFT       ; anim phase while digging a hole
_playerDigLeftJ1
        sta zpPlayerAnimPhase           ; player animation phase
        jsr displayPlayerCheckEnemy     ; display player, check for enemy collision

        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        cpx #DIG_ANIM_PHASE_LEFT_MAX+1  ; animation finished?
        beq finishPlayerDigLeft         ; yes -> digging the hole has finished
        cpx #DIG_ANIM_PHASE_LEFT_MIN    ; animation just started?
        beq _playerDigLeftJ2            ; yes -> skip erasing old animation
        lda animDigUpperLeft-1,x
        pha
        ldx zpPlayerX                   ; player, X position on board
        dex                             ; column left of player (above dig position)
        ldy zpPlayerY                   ; player, Y position on board
        jsr convertBoardPosToPixelPos
        pla
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1

        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
_playerDigLeftJ2
        lda animDigUpperLeft,x          ; (upper part, left)
        pha
        ldx zpPlayerX                   ; player, X position on board
        dex
        stx zpCursorCol
        ldy zpPlayerY                   ; player, Y position on board
        sty zpCursorRow
        jsr convertBoardPosToPixelPos
        pla
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)

        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        lda animDigLower,x              ; (lower part)
        inc zpCursorRow
        jsr replaceTileBitmap0

        inc zpHoleDigAnimCtr            ; inc. animation counter for a hole being drilled
        clc
        rts

_abortPlayerDigLeft
        ldy zpPlayerY                   ; set cursor to dig position
        iny
        sty zpCursorRow
        ldy zpPlayerX
        dey
        sty zpCursorCol
        lda #SHAPE_FLOOR_DIG
        jsr replaceTileBitmap0          ; replace dig position tile with SHAPE_FLOOR_DIG
        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        beq abortPlayerDigLeftJ1        ; DIG_ANIM_PHASE_LEFT_MIN? -> just started digging
        dex                             ; previous animation frame
        lda animDigUpperLeft,x
        pha
        ldy zpPlayerY                   ; player, Y position on board
        ldx zpPlayerX                   ; player, X position on board
        dex
        jsr convertBoardPosToPixelPos
        pla
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1

abortPlayerDigLeftJ1
        lda #$00
        sta playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        sec
        rts

finishPlayerDigLeft
        ldx zpPlayerX                   ; player, X position on board
        dex
        jmp initOpenHole                ; initialize open hole counter and position

failPlayerDigRight
        jmp abortPlayerDigRightJ2

handlePlayerDigRight
        lda #$01                        ; constant: dig right
        sta playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        sta inputVertical
        sta inputHorizontal
        lda #DIG_ANIM_PHASE_RIGHT_MIN   ; start dig animation
        sta zpHoleDigAnimCtr            ; animation counter for a hole being drilled
continuePlayerDigRight                  ; entry point: continue digging
        ldy zpPlayerY                   ; player, Y position on board
        cpy #BOARD_HEIGHT-1             ; player on bottom row?
        bcs failPlayerDigRight          ; yes -> can't dig here
        iny
        jsr getLayoutAndActionBoardPtrs ; set board pointers below player
        ldy zpPlayerX                   ; player, X position on board
        cpy #BOARD_WIDTH-1              ; player in rightmost position?
        bcs failPlayerDigRight          ; yes -> can't dig here
        iny
        lda (zpBoardActionPtr),y        ; fetch board action tile (dig location)
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        bne failPlayerDigRight          ; no -> can't dig here

        ldy zpPlayerY                   ; player, Y position on board
        jsr getLayoutAndActionBoardPtrs
        ldy zpPlayerX                   ; player, X position on board
        iny
        lda (zpBoardActionPtr),y        ; fetch board action tile right of player
        cmp #SHAPE_BLANK                ; field above dig location empty?
        bne _abortPlayerDigRight        ; no (enemy above dig position) -> abort dig

        jsr prepareDisplayPlayer
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        jsr nudgePlayerToStepXMiddle    ; nudge player towards StepX middle position
        jsr nudgePlayerToStepYMiddle    ; nudge player towards StepY middle position
        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        lda #ANIM_PHASE_RUN_RIGHT_0     ; anim phase used when digging is finished
        cpx #DIG_ANIM_PHASE_RIGHT_MID   ; digging almost finished?
        bcs _playerDigRightJ1           ; yes -> switch player anim back to running stance
        lda #ANIM_PHASE_FIRE_RIGHT      ; anim phase while digging a hole
_playerDigRightJ1
        sta zpPlayerAnimPhase           ; player animation phase
        jsr displayPlayerCheckEnemy     ; display player, check for enemy collision

        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        cpx #DIG_ANIM_PHASE_RIGHT_MAX+1 ; animation finished?
        beq finishPlayerDigRight        ; yes -> digging the hole has finished
        cpx #DIG_ANIM_PHASE_RIGHT_MIN   ; animation just started?
        beq _playerDigRightJ2           ; yes -> skip erasing old animation
        lda animDigUpperRight-DIG_ANIM_PHASE_RIGHT_MIN-1,x
        pha
        ldx zpPlayerX                   ; player, X position on board
        inx                             ; column right of player (above dig position)
        ldy zpPlayerY                   ; player, Y position on board
        jsr convertBoardPosToPixelPos
        pla
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1

        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
_playerDigRightJ2
        lda animDigUpperRight-DIG_ANIM_PHASE_RIGHT_MIN,x        ; (upper part)
        pha
        ldx zpPlayerX                   ; player, X position on board
        inx
        stx zpCursorCol
        ldy zpPlayerY                   ; player, Y position on board
        sty zpCursorRow
        jsr convertBoardPosToPixelPos
        pla
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)

        inc zpCursorRow
        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        lda animDigLower-DIG_ANIM_PHASE_RIGHT_MIN,x     ; (lower part)
        jsr replaceTileBitmap0

        inc zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        clc
        rts

_abortPlayerDigRight
        ldy zpPlayerY                   ; set cursor to dig position
        iny
        sty zpCursorRow
        ldy zpPlayerX
        iny
        sty zpCursorCol
        lda #SHAPE_FLOOR_DIG
        jsr replaceTileBitmap0          ; replace dig position tile with SHAPE_FLOOR_DIG
        ldx zpHoleDigAnimCtr            ; animation counter for a hole being drilled
        cpx #DIG_ANIM_PHASE_RIGHT_MIN
        beq abortPlayerDigRightJ2       ; just started digging? ->
        dex                             ; previous animation frame
        lda animDigUpperRight-DIG_ANIM_PHASE_RIGHT_MIN,x
        pha
        ldx zpPlayerX                   ; player, X position on board
        inx
        ldy zpPlayerY                   ; player, Y position on board
        jsr convertBoardPosToPixelPos
        pla
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1

abortPlayerDigRightJ2
        lda #$00
        sta playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        sec
        rts

finishPlayerDigRight
        ldx zpPlayerX                   ; player, X position on board
        inx
        jmp initOpenHole                ; initialize open hole counter and position


; table: convert a sprite phase number into a shape data offset (see shapeData)

tabPlayerAnimPhaseToShape
        .byte $0b                       ; $00: run left 0
        .byte $0c                       ; $01: run left 1
        .byte $0d                       ; $02: run left 2
        .byte $18                       ; $03: bar left 0
        .byte $19                       ; $04: bar left 1
        .byte $1a                       ; $05: bar left 2
        .byte $0f                       ; $06: fire left
        .byte $13                       ; $07: fall left
        .byte $09                       ; $08: run right 0
        .byte $10                       ; $09: run right 1
        .byte $11                       ; $0a: run right 2
        .byte $15                       ; $0b: bar right 0
        .byte $16                       ; $0c: bar right 1
        .byte $17                       ; $0d: bar right 2
        .byte $25                       ; $0e: fire right
        .byte $14                       ; $0f: fall right
        .byte $0e                       ; $10: climb ladder 0
        .byte $12                       ; $11: climb ladder 1

animDigUpperLeft
        .byte $1b
        .byte $1b
        .byte $1c
        .byte $1c
        .byte $1d
        .byte $1d
        .byte $1e
        .byte $1e
        .byte $00
        .byte $00
        .byte $00
        .byte $00

animDigUpperRight
        .byte $26
        .byte $26                       ; $20
        .byte $27
        .byte $27
        .byte $1d
        .byte $1d
        .byte $1e
        .byte $1e
        .byte $00
        .byte $00
        .byte $00
        .byte $00

animDigLower
        .byte $1f
        .byte $1f
        .byte $20
        .byte $20
        .byte $21
        .byte $21                       ; $30
        .byte $22
        .byte $22
        .byte $23
        .byte $23
        .byte $24
        .byte $24


getDemoControllerInput                  ; get simulated user input for demo
        lda keyboardCode                ; keyboard matrix code (no key: 0)
        bne _userEndsDemo               ; any key pressed -> end demo
        lda Cia1PortA                   ; read joystick port #2
        and #$10                        ; joystick fire button?
        bne _continueDemo               ; no ->
_userEndsDemo
        lsr zpDemoUnused                ; variable unused (initialized to $01)
        lsr playerAlive                 ; 0: player dead, 1: player alive
        lda #$01
        sta lives
        rts
_continueDemo
        lda zpDemoRepeatCounter         ; command repeat counter during game demo
        bne _skipInc
        ldy #$00
        lda (zpDemoPtr),y               ; fetch next byte from demoGameSequence
        sta zpDemoCommandInput          ; simulate user's game control input
        iny
        lda (zpDemoPtr),y               ; fetch next byte from demoGameSequence
        sta zpDemoRepeatCounter         ; command repeat counter during game demo
        lda zpDemoPtr+0
        clc
        adc #$02                        ; increase zpDemoPtr by 2
        sta zpDemoPtr+0
        lda zpDemoPtr+1
        adc #$00
        sta zpDemoPtr+1
_skipInc
        lda zpDemoCommandInput
        and #$0f                        ; low nibble: vertical input
        tax
        lda demoSeqCmdToKey,x
        sta inputVertical
        lda zpDemoCommandInput
        lsr                             ; high nibble: horizontal input
        lsr
        lsr
        lsr
        tax
        lda demoSeqCmdToKey,x
        sta inputHorizontal
        dec zpDemoRepeatCounter
        rts

demoSeqCmdToKey
                                        ; table: demo sequence byte to key code
        .byte KEY_CODE_I                ; up
        .byte KEY_CODE_J                ; left
        .byte KEY_CODE_K                ; down
        .byte KEY_CODE_L                ; right
        .byte KEY_CODE_O                ; dig right
        .byte KEY_CODE_U                ; dig left

getPlayerControllerInput
        lda gameMode                    ; 0: start screen, 1: demo, 2: play, 3: play from edit, 5: editor
        cmp #GAME_MODE_DEMO
        beq getDemoControllerInput      ; get simulated user input for demo
        ldx keyboardCode                ; keyboard matrix code (no key: 0)
        bne _handleKeyboardInput        ; detected keyboard input? -> handle it
        lda controllerMode              ; 'J': Joystick, 'K': Keyboard
        cmp #PETSCII_K
        beq _exit                       ; no keyboard input read ->

_handleJoystickInput
        jmp readJoystick                ; read and evaluate joystick

_handleKeyboardInput
        lda #$00
        sta keyboardCode                ; keyboard matrix code (no key: 0)
        stx zpGameInputKey              ; input key during game play

        ldy #$ff                        ; try to find a key-combo for commands
_checkGameCmdKeyL
        iny
        lda gameCmdKeyTbl,y             ; compare key code with game command keys
        beq _handleKbdInputJ1           ; no match found ->
        cmp zpGameInputKey              ; input key during game play
        bne _checkGameCmdKeyL
        tya                             ; command offset in table
        asl                             ; as 16 bit (address) offset
        tay
        lda gameCmdJmpTbl+1,y
        pha                             ; as return address on stack
        lda gameCmdJmpTbl+0,y
        pha
                rts                             ; jump to game command

_handleKbdInputJ1
        lda controllerMode              ; 'J': Joystick, 'K': Keyboard
        cmp #PETSCII_J                  ; joystick input mode?
        beq _handleJoystickInput        ; yes ->
        ldx zpGameInputKey              ; input key during game play
        stx inputVertical               ; store in vertical input variable
        stx inputHorizontal             ; store in horizontal input variable
_exit
        rts


advanceOneLevel                         ; advance a level
        inc lives                       ; add a life
        inc displayedLevel              ; current level (displayed)
        inc currentLevel                ; current level (0-based)
        lsr playerAlive                 ; 0: player dead, 1: player alive
        lsr allowHighScoreEntry         ; 0: player cheated, no high score entry, 1: did not cheat
        rts

addOneLife                              ; add additional life/player
        inc lives                       ; add a life
        bne _noOverflow                 ; no overflow ->
        dec lives                       ; handle overflow situation
_noOverflow
        jsr printLives                  ; print number of lives
        lsr allowHighScoreEntry         ; 0: player cheated, no high score entry, 1: did not cheat
        jmp getPlayerControllerInput

increaseEnemySpeed                      ; unused: increase enemySpeed, restart level
        inc enemySpeed                  ; enemy speed (0-10)
        inc lives                       ; add a life
        lsr playerAlive                 ; 0: player dead, 1: player alive
        rts

pauseGame
        jsr waitForKey
        cmp #KEY_CODE_RUN_STOP          ; RUN/STOP pressed a second time?
        bne pauseGame
        jmp getPlayerControllerInput

terminateCurrentGame                    ; terminate current game (enter demo mode)
        jmp terminateGame

abortLife                               ; abort man if stuck without means of death or escape
        lsr playerAlive                 ; 0: player dead, 1: player alive
        rts

setJoystickControl
        lda #PETSCII_J
        sta controllerMode              ; 'J': Joystick, 'K': Keyboard
        jmp getPlayerControllerInput

setKeyboardControl
        lda #PETSCII_K
        sta controllerMode              ; 'J': Joystick, 'K': Keyboard
        jmp getPlayerControllerInput

speedUpGameSpeed                        ; Speed up overall game speed (press repeatedly)
        lda gameDelay                   ; 3: fastest, 8: slowest
        cmp #GAME_DELAY_MIN             ; minimum game delay (3)
        beq changeGameSpeedAbort
        dec gameDelay                   ; 3: fastest, 8: slowest
        jmp getPlayerControllerInput

slowDownGameSpeed                       ; slow down overall game speed (press repeatedly)
        lda gameDelay                   ; 3: fastest, 8: slowest
        cmp #GAME_DELAY_MAX             ; maximum game delay (8)
        beq changeGameSpeedAbort
        inc gameDelay                   ; 3: fastest, 8: slowest
changeGameSpeedAbort
        jmp getPlayerControllerInput

toggleIrisAnimation                     ; toggle use of iris animation (on/off)
        lda irisAnimationOn             ; $ff: iris animation on, $00: turned off
        eor #$ff                        ; toggle animation on/off
        sta irisAnimationOn             ; $ff: iris animation on, $00: turned off
        jmp getPlayerControllerInput

toggleDigDirection                      ; toggle direction of dig (forward or behind runner)
        lda digDirection                ; $ff: dig forward, $00: dig behind runner
        eor #$ff                        ; invert direction
        sta digDirection                ; $ff: dig forward, $00: dig behind runner
        jmp getPlayerControllerInput


readJoystick                            ; read and evaluate joystick
        ; The game checks for joystick events and replaces them
        ; by the corresponding keyboard events.

        ; joystick handling see register $dc00:
        ; https://sta.c64.org/cbm64mem.html

        lda Cia1PortA                   ; read joystick port #2
        and #$10                        ; joystick fire button?
        bne _joyCheckVertical           ; no ->
        lda zpPlayerOrientation         ; $ff: player facing left, $01: facing right
        eor digDirection                ; $ff: dig forward, $00: dig behind runner
        bpl _joyDigLeft
_joyDigRight
        lda #KEY_CODE_O                 ; 'O' (dig right)
        sta inputVertical
        sta inputHorizontal
        rts
_joyDigLeft
        lda #KEY_CODE_U                 ; 'U' (dig left)
        sta inputVertical
        sta inputHorizontal
        rts

_joyCheckVertical
        lda Cia1PortA                   ; read joystick port #2
        sta zpJoystickPort
        and #$02                        ; joystick down?
        beq _joyDown                    ; yes ->
        lda zpJoystickPort
        and #$01                        ; joystick up?
        beq _joyUp                      ; yes ->
        ldx #$00
        stx inputVertical
        beq _joyCheckHorizontal
_joyUp
        ldx #KEY_CODE_I                 ; 'I' (move up)
        stx inputVertical
        bne _joyCheckHorizontal
_joyDown
        ldx #KEY_CODE_K                 ; 'K' (move down)
        stx inputVertical

_joyCheckHorizontal
        lda zpJoystickPort
        and #$08                        ; joystick right?
        beq _joyRight                   ; yes ->
        lda zpJoystickPort
        and #$04                        ; joystick left?
        beq _joyLeft                    ; yes ->
        ldx #$00
        stx inputHorizontal
        rts
_joyRight
        ldx #KEY_CODE_L                 ; 'L' (move right)
        stx inputHorizontal
        rts
_joyLeft
        ldx #KEY_CODE_J                 ; 'J' (move left)
        stx inputHorizontal
        rts

gameCmdKeyTbl
        .byte KEY_CODE_CTRL+KEY_CODE_U  ; CTRL-U: Advance a level
        .byte KEY_CODE_CTRL+KEY_CODE_F  ; CTRL-F: Add additional lives/players
        .byte KEY_CODE_RUN_STOP
        .byte KEY_CODE_CTRL+KEY_CODE_R  ; CTRL-R: Terminate current game (enter demo mode)
        .byte KEY_CODE_CTRL+KEY_CODE_A  ; CTRL-A: Abort man if stuck without means of death or escape
        .byte KEY_CODE_CTRL+KEY_CODE_J  ; CTRL-J: Set game to joystick control
        .byte KEY_CODE_CTRL+KEY_CODE_K  ; CTRL-K: Set game to keyboard control
        .byte KEY_CODE_MINUS            ; '-'   : Slow down overall game speed (press repeatedly)
        .byte KEY_CODE_PLUS             ; '#'   : Speed up overall game speed (press repeatedly)
        .byte KEY_CODE_CTRL+KEY_CODE_Z  ; CTRL-Z: Toggle the iris view
        .byte KEY_CODE_CTRL+KEY_CODE_D  ; CTRL-D: Toggle direction of dig (forward or behind runner)
        .byte $00

gameCmdJmpTbl
        .word advanceOneLevel-1
        .word addOneLife-1
        .word pauseGame-1
        .word terminateCurrentGame-1
        .word abortLife-1
        .word setJoystickControl-1
        .word setKeyboardControl-1
        .word slowDownGameSpeed-1
        .word speedUpGameSpeed-1
        .word toggleIrisAnimation-1
        .word toggleDigDirection-1

prepareDisplayPlayer
        ldx zpPlayerX                   ; player, X position on board
        ldy zpPlayerStepX               ; player, X position (fine)
        jsr calcPixelPosX               ; calculate pixel position (X)
        stx zpTmpPixelPosX              ; temporarily store pixel position (X)
        ldy zpPlayerY                   ; player, Y position on board
        ldx zpPlayerStepY               ; player, Y position (fine)
        jsr calcPixelPosY               ; calculate pixel position (Y)
        ldx zpPlayerAnimPhase           ; player animation phase
        lda tabPlayerAnimPhaseToShape,x ; get shape data offset from animation phase
        ldx zpTmpPixelPosX              ; restore pixel position (X)
        rts

playerCheckCollectGold                  ; check for and collect gold chests
        lda zpPlayerStepX               ; player, X position (fine)
        cmp #STEP_MIDDLE                ; player in center of block (X)?
        bne _exit                       ; no -> exit
        lda zpPlayerStepY               ; player, Y position (fine)
        cmp #STEP_MIDDLE                ; player in center of block (Y)?
        bne _exit                       ; no -> exit

        ldy zpPlayerY                   ; player, Y position on board
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpPlayerX                   ; player, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_GOLD_CHEST           ; gold chest at player's location?
        bne _exit                       ; no ->
        lsr playerNoGoldPickedUp        ; change to: 0: gold pick up in process

        ldx #$10                        ; minimum raster beam position
        ldy #$20                        ; maximum raster beam position

        lda #$04                        ; minimum delta between consecutive pitch values
        jsr randomizeGoldJinglePitchVal ; randomize pitch for voice 1
        sta _goldJingleData+1

        lda #$04                        ; minimum delta between consecutive pitch values
        jsr randomizeGoldJinglePitchVal ; randomize pitch for voice 1
        sta _goldJingleData+5

        lda #$04                        ; minimum delta between consecutive pitch values
        jsr randomizeGoldJinglePitchVal ; randomize pitch for voice 1
        sta _goldJingleData+9

        lda #$04                        ; minimum delta between consecutive pitch values
        jsr randomizeGoldJinglePitchVal ; randomize pitch for voice 1
        sta _goldJingleData+13


        jsr initJingleCollectGold       ; start playing gold collected jingle
_goldJingleData
        .byte $04,$00,$ff,$b0           ; jingle data:
        .byte $04,$00,$ff,$a0           ; note time / voice 1 / voice 2 / volume
        .byte $04,$00,$ff,$90
        .byte $04,$00,$ff,$a0
        .byte $00

        dec goldLeft                    ; decrease # of gold chests left

        ldy zpPlayerY                   ; player, Y position on board
        sty zpCursorRow
        ldy zpPlayerX                   ; player, X position on board
        sty zpCursorCol
        lda #SHAPE_BLANK
        sta (zpBoardLayoutPtr),y        ; remove gold chest from board
        jsr replaceTileBitmap1          ; print blank shape to buffer bitmap
        ldy zpCursorRow
        ldx zpCursorCol
        jsr convertBoardPosToPixelPos   ; return: (X,Y) pixel coordinates
        lda #SHAPE_GOLD_CHEST
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        ldy #>SCORE_PICK_UP_GOLD        ; score for picking up a gold chest: 0250
        lda #<SCORE_PICK_UP_GOLD
        jsr addPrintScore
_exit
        rts

updatePlayerAnimPhase                   ; set/inc animation phase between min (A) and max (X)
        inc zpPlayerAnimPhase           ; increment player animation phase
        cmp zpPlayerAnimPhase           ; minimum < animation phase?
        bcc _checkPlayerAnimPhaseMax    ; yes ->
_setPlayerAnimPhaseToMin
        sta zpPlayerAnimPhase           ; set animation phase to minimum
        rts
_checkPlayerAnimPhaseMax
        cpx zpPlayerAnimPhase           ; maximum < animation phase (too big)?
        bcc _setPlayerAnimPhaseToMin    ; yes -> set animation phase to minimum
        rts

displayPlayerCheckEnemy                 ; display player, check for enemy collision
        jsr prepareDisplayPlayer
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
        lda zpPlayerEnemyCollision      ; 0: no collision of player and enemy; 1: collision detected
        beq _exit                       ; no collision -> exit
        lda playerNoGoldPickedUp        ; 0: gold pick up in process, 1: no gold picked up
        beq _exit                       ; player picked up gold in this round -> exit
        lsr playerAlive                 ; change to: 0: player dead (from 1: alive)
_exit
        rts

nudgePlayerToStepXMiddle                ; nudge player towards StepX middle position
        lda zpPlayerStepX               ; player, X position (fine)
        cmp #STEP_MIDDLE
        bcc _nudgePlayerRight           ; player x step < middle: -> inc x step
        beq _exit                       ; player x step == middle: -> exit
        dec zpPlayerStepX               ; player x step > middle: dec x step (nudge to left)
        jmp playerCheckCollectGold      ; check for and collect gold chests
_nudgePlayerRight
        inc zpPlayerStepX               ; player, X position (fine)
        jmp playerCheckCollectGold      ; check for and collect gold chests
_exit
        rts

nudgePlayerToStepYMiddle                ; nudge player towards StepY middle position
        lda zpPlayerStepY               ; player, Y position (fine)
        cmp #STEP_MIDDLE
        bcc _nudgePlayerDown            ; player y step < middle: -> inc y step
        beq _exit                       ; player y step == middle: -> exit
        dec zpPlayerStepY               ; player, Y position (fine) (nudge up)
        jmp playerCheckCollectGold      ; check for and collect gold chests
_nudgePlayerDown
        inc zpPlayerStepY               ; player, Y position (fine)
        jmp playerCheckCollectGold      ; check for and collect gold chests
_exit
        rts

initOpenHole                            ; initialize open hole counter and position
        lda #$00
        sta playerDiggingState          ; 0: player not digging, 1: digging right, -1 ($ff): digging left
        ldy zpPlayerY                   ; player, Y position on board
        iny                             ; hole Y position = player Y position + 1
        stx zpCursorCol
        sty zpCursorRow
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda #SHAPE_BLANK
        ldy zpCursorCol
        sta (zpBoardActionPtr),y        ; store hole as a blank in action board
        jsr replaceTileBitmap0
        lda #SHAPE_BLANK
        jsr replaceTileBitmap1          ; clear dust in hole
        dec zpCursorRow                 ; dec row to location above hole
        lda #SHAPE_BLANK
        jsr replaceTileBitmap0          ; clear dust above hole
        inc zpCursorRow                 ; inc row to hole's location

        ldx #$ff                        ; index into open hole array
_findHolesOpenSlotL
        inx                             ; inc to zero
        cpx #MAX_HOLES_OPEN             ; max number of open holes reached?
        beq _initOpenHoleDone           ; yes ->
        lda holesOpenCtr,x              ; open hole slot busy?
        bne _findHolesOpenSlotL         ; yes -> try next slot
        lda zpCursorRow                 ; found an empty slot: store current row
        sta holesPosY,x                 ; as hole, y-position
        lda zpCursorCol                 ; store current column
        sta holesPosX,x                 ; as hole, x-position
        lda #HOLE_OPEN_CTR_INIT         ; initialize time until hole closes
        sta holesOpenCtr,x              ; time until hole closes
        sec                             ; confirm: opened a new hole
_initOpenHoleDone
        rts


handleEnemies
        ldx numEnemies                  ; any enemies to handle on this board?
        beq _exit                       ; no -> exit
        inc zpEnemyMoveCycleIdx         ; index into current enemy move cycle (0-2)
        ldy zpEnemyMoveCycleIdx
        cpy #$03                        ; move cycle index >= 3?
        bcc _handleEnemiesJ1            ; no ->
        ldy #$00                        ; move cycle index: 0
        sty zpEnemyMoveCycleIdx         ; reset index into current enemy move cycle (0-2)
_handleEnemiesJ1
        lda @w zpEnemyMoveCycleTbl,y    ; fetch the current move cycle from table
        sta zpEnemyMoveCycleCur         ; current enemy move cycle
_handleEnemiesL
        lsr zpEnemyMoveCycleCur         ; rotate rightmost bit of current move cycle into carry
        bcc _handleEnemiesSkip          ; if bit is 0 -> do no move enemy
        jsr handleEnemy                 ; handle the next enemy
        lda playerAlive                 ; 0: player dead, 1: player alive
        beq _exit                       ; if player is dead -> exit
_handleEnemiesSkip
        lda zpEnemyMoveCycleCur         ; any enemy moves left in current cycle?
        bne _handleEnemiesL             ; yes ->
_exit
        rts

tabDefEnemyMoveCycles

tabDefEnemyMoveCyclesBlock00
tabDefEnemyMoveCycle0
        .byte %00000000
tabDefEnemyMoveCycle1
        .byte %00000000
tabDefEnemyMoveCycle2
        .byte %00000000
tabDefEnemyMoveCyclesBlock01
        .byte %00000000
        .byte %00000001
        .byte %00000001
tabDefEnemyMoveCyclesBlock02
        .byte %00000001
        .byte %00000001
        .byte %00000001
tabDefEnemyMoveCyclesBlock03
        .byte %00000001
        .byte %00000011
        .byte %00000001
tabDefEnemyMoveCyclesBlock04
        .byte %00000001
        .byte %00000011
        .byte %00000011
tabDefEnemyMoveCyclesBlock05
        .byte %00000011
        .byte %00000011
        .byte %00000011
tabDefEnemyMoveCyclesBlock06
        .byte %00000011
        .byte %00000011
        .byte %00000111
tabDefEnemyMoveCyclesBlock07
        .byte %00000011
        .byte %00000111
        .byte %00000111
tabDefEnemyMoveCyclesBlock08
        .byte %00000111
        .byte %00000111
        .byte %00000111
tabDefEnemyMoveCyclesBlock09
        .byte %00000111
        .byte %00000111
        .byte %00001111
tabDefEnemyMoveCyclesBlock0a
        .byte %00000111
        .byte %00001111
        .byte %00001111
tabDefEnemyMoveCyclesBlock0b
        .byte %00001111
        .byte %00001111
        .byte %00001111
tabDefEnemyMoveCyclesBlock0c
        .byte %00001111
        .byte %00001111
        .byte %00011111
tabDefEnemyMoveCyclesBlock0d
        .byte %00001111
        .byte %00011111
        .byte %00011111
tabDefEnemyMoveCyclesBlock0e
        .byte %00011111
        .byte %00011111
        .byte %00011111
tabDefEnemyMoveCyclesBlock0f
        .byte %00011111
        .byte %00011111
        .byte %00111111
tabDefEnemyMoveCyclesBlock10
        .byte %00011111
        .byte %00111111
        .byte %00111111
tabDefEnemyMoveCyclesBlock11
        .byte %00111111
        .byte %00111111
        .byte %00111111
tabDefEnemyMoveCyclesBlock12
        .byte %00111111
        .byte %00111111
        .byte %01111111
tabDefEnemyMoveCyclesBlock13
        .byte %00111111
        .byte %01111111
        .byte %01111111
tabDefEnemyMoveCyclesBlock14
        .byte %01111111
        .byte %01111111
        .byte %01111111

tabEnemyAnimPhaseToShape
        .byte $08               ; EN_ANIM_PHASE_RUN_LEFT_0
        .byte $2b               ; EN_ANIM_PHASE_RUN_LEFT_1
        .byte $2c               ; EN_ANIM_PHASE_RUN_LEFT_2
        .byte $30               ; EN_ANIM_PHASE_BAR_LEFT_0
        .byte $31               ; EN_ANIM_PHASE_BAR_LEFT_1
        .byte $32               ; EN_ANIM_PHASE_BAR_LEFT_2
        .byte $36               ; EN_ANIM_PHASE_FALL_LEFT
        .byte $28               ; EN_ANIM_PHASE_RUN_RIGHT_0
        .byte $29               ; EN_ANIM_PHASE_RUN_RIGHT_1
        .byte $2a               ; EN_ANIM_PHASE_RUN_RIGHT_2
        .byte $2d               ; EN_ANIM_PHASE_BAR_RIGHT_0
        .byte $2e               ; EN_ANIM_PHASE_BAR_RIGHT_1
        .byte $2f               ; EN_ANIM_PHASE_BAR_RIGHT_2
        .byte $35               ; EN_ANIM_PHASE_FALL_RIGHT
        .byte $33               ; EN_ANIM_PHASE_CLIMB_0
        .byte $34               ; EN_ANIM_PHASE_CLIMB_1

handleEnemy                             ; handle the next enemy
        inc enemyIndex                  ; increment current enemy index
        ldx numEnemies                  ; number of enemies in current level
        cpx enemyIndex                  ; num enemies >= enemy index?
        bcs _handleEnemyJ1              ; yes ->
        ldx #$01
        stx enemyIndex                  ; reset current enemy index to 1
_handleEnemyJ1
        jsr loadEnemyState              ; set enemy variables for current enemy ID from enemy object table

        ; check whether enemy is stuck in a hole and tries to climb out

        lda zpEnemyActionCtr            ; current enemy, special action counter
        bmi _handleEnemyNotInHole       ; <0: remaining time until enemy drops gold ->
        beq _handleEnemyNotInHole       ; no current timer ->
        dec zpEnemyActionCtr            ; decrement remaining time enemy is trapped (decreasing)
        ldy zpEnemyActionCtr            ; >0: remaining time enemy is trapped (decreasing)
        cpy #ENEMY_PIT_CTR_WIGGLE_START ; enemy about to escape trap?
        bcs _handleEnemyInHoleJ1        ; no ->
        jmp enemyTriesEscapeHole        ; yes -> enemy attempts to escape hole
_handleEnemyInHoleJ1
        ldx enemyIndex                  ; current enemy, index
        lda enemiesRespawnCtr,x         ; time left until enemy respawns
        beq _handleEnemyJ2              ; respawn counter == 0? ->
        jmp saveEnemyState              ; enemy currently stuck in hole ->
_handleEnemyJ2
        jmp displayEnemy                ; enemy ready to respawn, but stuck in hole ->

_handleEnemyNotInHole
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y        ; get layout tile under enemy
        cmp #SHAPE_LADDER
        beq _handleEnemyNoFall          ; ladder -> free movement
        cmp #SHAPE_BAR
        bne _handleEnemyJ3
        lda zpEnemyStepY                ; current enemy, Y position (fine)
        cmp #STEP_MIDDLE                ; bar, STEP_MIDDLE -> free movement
        beq _handleEnemyNoFall
_handleEnemyJ3
        lda zpEnemyStepY                ; current enemy, Y position (fine)
        cmp #STEP_MIDDLE
        bcc handleEnemyFall             ; above middle? -> falls
        ldy zpEnemyY                    ; current enemy, Y position on board
        cpy #BOARD_HEIGHT-1
        beq _handleEnemyNoFall          ; enemy on lower most row -> free movement
        lda boardActionOffsetLb+1,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        lda boardActionOffsetHb+1,y
        sta zpBoardActionPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardActionPtr),y
        cmp #SHAPE_BLANK
        beq handleEnemyFall
        cmp #SHAPE_PLAYER
        beq handleEnemyFall
        cmp #SHAPE_ENEMY
        beq _handleEnemyNoFall          ; enemy on top another enemy -> free movement
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG
        beq _handleEnemyNoFall          ; enemy walking on diggable floor -> free movement
        cmp #SHAPE_FLOOR_UNDIG
        beq _handleEnemyNoFall          ; enemy walking on solid floor -> free movement
        cmp #SHAPE_LADDER
        bne handleEnemyFall

_handleEnemyNoFall
        jmp enemyPathfinderMove

handleEnemyFall
        jsr prepareDisplayEnemy
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        jsr nudgeEnemyToStepXMiddle     ; nudge enemy towards StepX middle position, check gold
        lda #EN_ANIM_PHASE_FALL_LEFT
        ldy zpEnemyOrientation          ; $ff: enemy facing left, $01: facing right
        bmi _handleEnemyFallAnimate
        lda #EN_ANIM_PHASE_FALL_RIGHT
_handleEnemyFallAnimate
        sta zpEnemyAnimPhase            ; enemy animation phase
        inc zpEnemyStepY                ; increment enemy Y position (fine)
        lda zpEnemyStepY                ; current enemy, Y position (fine)
        cmp #STEP_MAX+1                 ; Y pos > STEP_MAX -> need to check block below
        bcs handleEnemyFallNextBlock
        lda zpEnemyStepY                ; current enemy, Y position (fine)
        cmp #STEP_MIDDLE
        bne displayEnemy                ; not in the middle -> no further handling
        jsr enemyCheckPickupGold        ; check for and pickup gold
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG            ; enemy falling into a hole?
        bne displayEnemy                ; not falling into a hole -> no further handling
        lda zpEnemyActionCtr            ; <0: remaining time until enemy drops gold (increasing)
                                        ; >0: remaining time enemy is trapped (decreasing)
        bpl _handleEnemyFallJ1          ; enemy not carrying gold ->
        dec goldLeft                    ; gold carried by enemy is lost
_handleEnemyFallJ1
        lda enemyTrappedDuration        ; duration an enemy is trapped in a hole
        sta zpEnemyActionCtr            ; current enemy, special action counter
        ldy #>SCORE_TRAP_ENEMY          ; score for trapping an enemy: 0075
        lda #<SCORE_TRAP_ENEMY
        jsr addPrintScore

displayEnemy
        jsr prepareDisplayEnemy
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
        jmp saveEnemyState

handleEnemyFallNextBlock
        lda #$00
        sta zpEnemyStepY                ; current enemy, Y position (fine)
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y        ; get layout tile from current tile
        cmp #SHAPE_FLOOR_DIG            ; leaving diggable floor?
        bne _updateActionTile           ; no ->
        lda #SHAPE_BLANK                ; replace diggable floor tile by empty tile
_updateActionTile
        sta (zpBoardActionPtr),y        ; remove enemy from old action tile
        inc zpEnemyY                    ; inc current enemy, Y position on board
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardActionPtr),y        ; get action tile from new tile
        cmp #SHAPE_PLAYER               ; entering same tile as player?
        bne _handleEnemyFallNextBlockJ1 ; no ->
        lsr playerAlive                 ; enemy kills player (0: player dead, 1: player alive)

_handleEnemyFallNextBlockJ1
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG            ; is enemy falling into a dug up brick?
        bne _handleEnemyFallJ3          ; no ->
        lda zpEnemyActionCtr            ; does enemy carry gold?
        bpl _handleEnemyFallJ3          ; no ->
        ldy zpEnemyY                    ; current enemy, Y position on board
        dey                             ; row above enemy
        sty zpCursorRow
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        sty zpCursorCol
        lda (zpBoardLayoutPtr),y        ; check board layout above the enemy
        cmp #SHAPE_BLANK                ; is the field empty?
        beq _enemyDropsGold             ; yes -> drop gold above trapped enemy
        dec goldLeft                    ; no space to drop gold -> gold is lost
        jmp _handleEnemyFallJ2

_enemyDropsGold
        lda #SHAPE_GOLD_CHEST
        sta (zpBoardActionPtr),y        ; store gold chest in board action
        sta (zpBoardLayoutPtr),y        ; store gold chest in board layout
        jsr replaceTileBitmap1
        ldy zpCursorRow
        ldx zpCursorCol
        jsr convertBoardPosToPixelPos
        lda #SHAPE_GOLD_CHEST
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)

_handleEnemyFallJ2
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda #$00
        sta zpEnemyActionCtr
        ldy zpEnemyX                    ; current enemy, X position on board

_handleEnemyFallJ3
        lda #SHAPE_ENEMY
        sta (zpBoardActionPtr),y
        jsr prepareDisplayEnemy
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
        jmp saveEnemyState

enemyTriesEscapeHole
        cpy #ENEMY_PIT_CTR_ESCAPE_START ; below this value, enemy climbs out of hole ($07)
        bcc enemyClimbsOutOfHole
        jsr prepareDisplayEnemy
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        ldy zpEnemyActionCtr            ; current enemy, special action counter
        lda _tabEnemyInHoleWiggleX-7,y  ; make enemy wiggle in hole by altering zpEnemyStepX
        sta zpEnemyStepX                ; current enemy, X position (fine)
        jsr prepareDisplayEnemy
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
        jmp saveEnemyState

_tabEnemyInHoleWiggleX
        .byte $02,$01,$02,$03,$02,$01

enemyClimbsOutOfHole
enemyPathfinderMove
        ldx zpEnemyX                    ; current enemy, X position on board
        ldy zpEnemyY                    ; current enemy, Y position on board
        jsr enemyFindNextMove           ; run pathfinding algorithm (???)
        asl                             ; convert direction value into 16 bit pointer
        tay
        lda _enemyMoveJmpTbl+1,y
        pha
        lda _enemyMoveJmpTbl+0,y
        pha
        rts

                                       ; jump table (via rts)
_enemyMoveJmpTbl
        .word saveEnemyState-1
        .word moveEnemyLeft-1
        .word moveEnemyRight-1
        .word moveEnemyUp-1
        .word moveEnemyDown-1

moveEnemyUpBlocked
        lda zpEnemyActionCtr            ; current enemy, special action counter
        beq _exit                       ; no action counter active ->
        bmi _exit                       ; <0: enemy carries gold ->
                                        ; >0: remaining time enemy is trapped (increase)
        inc zpEnemyActionCtr            ; (enemy has nowhere to climb, cancel previous decrease)
_exit
        jmp saveEnemyState


moveEnemyUp
        ldy zpEnemyStepY                ; current enemy, Y position (fine)
        cpy #STEP_MIDDLE+1              ; y position fine > middle?
        bcs _moveEnemyUpOk              ; yes ->
        ldy zpEnemyY                    ; current enemy, Y position on board
        beq moveEnemyUpBlocked          ; enemy on top position? -> can't move up
        dey                             ; y position above enemy
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardActionPtr),y        ; fetch action tile above enemy
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq moveEnemyUpBlocked          ; yes -> can't move up
        cmp #SHAPE_FLOOR_UNDIG          ; undiggable floor?
        beq moveEnemyUpBlocked          ; yes -> can't move up
        cmp #SHAPE_TRAP_DOOR            ; trap door?
        beq moveEnemyUpBlocked          ; yes -> can't move up
        cmp #SHAPE_ENEMY                ; occupied by another enemy?
        beq moveEnemyUpBlocked          ; yes -> can't move up

_moveEnemyUpOk
        jsr prepareDisplayEnemy
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        jsr nudgeEnemyToStepXMiddle     ; nudge enemy towards StepX middle position
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        dec zpEnemyStepY                ; decrement current enemy Y position (fine)
        bpl moveEnemyUpDownFinish
        jsr enemyCheckDropGold          ; enemy: check/drop gold
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG
        bne _moveEnemyUpJ1
        lda #SHAPE_BLANK
_moveEnemyUpJ1
        sta (zpBoardActionPtr),y        ; replace action tile in old position
        dec zpEnemyY                    ; current enemy, Y position on board
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardActionPtr),y
        cmp #SHAPE_PLAYER               ; is enemy catching the player?
        bne _moveEnemyUpJ2              ; no ->
        lsr playerAlive                 ; 0: player dead, 1: player alive
_moveEnemyUpJ2
        lda #SHAPE_ENEMY
        sta (zpBoardActionPtr),y
        lda #STEP_MAX                   ; reset Y position (fine) for new position
        sta zpEnemyStepY                ; current enemy, Y position (fine)
        bne setEnemyAnimationClimb
moveEnemyUpDownFinish
        jsr enemyCheckPickupGold        ; check for and pickup gold
setEnemyAnimationClimb
        lda #EN_ANIM_PHASE_CLIMB_0
        ldx #EN_ANIM_PHASE_CLIMB_1
        jsr updateEnemyAnimPhase
        jsr prepareDisplayEnemy
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
        jmp saveEnemyState


moveEnemyDown
        ldy zpEnemyStepY                ; current enemy, Y position (fine)
        cpy #STEP_MIDDLE                ; y position fine < middle?
        bcc _moveEnemyDownOk            ; yes ->
        ldy zpEnemyY                    ; current enemy, Y position on board
        cpy #BOARD_HEIGHT-1             ; enemy on lower most position?
        bcs _moveEnemyDownBlocked       ; yes -> can't move down
        iny
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardActionPtr),y        ; fetch action tile below enemy
        cmp #SHAPE_FLOOR_UNDIG          ; undiggable floor?
        beq _moveEnemyDownBlocked       ; yes -> can't move down
        cmp #SHAPE_ENEMY                ; occupied by another enemy?
        beq _moveEnemyDownBlocked       ; yes -> can't move down
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        bne _moveEnemyDownOk            ; no -> can move down
_moveEnemyDownBlocked
        jmp saveEnemyState

_moveEnemyDownOk
        jsr prepareDisplayEnemy
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        jsr nudgeEnemyToStepXMiddle     ; nudge enemy towards StepX middle position
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        inc zpEnemyStepY                ; current enemy, Y position (fine)
        lda zpEnemyStepY
        cmp #STEP_MAX+1                 ; enemy Y position < STEP_MAX?
        bcc _moveEnemyDownFinish        ; yes ->
        jsr enemyCheckDropGold          ; enemy: check/drop gold
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG
        bne _moveEnemyDownJ1
        lda #SHAPE_BLANK
_moveEnemyDownJ1
        sta (zpBoardActionPtr),y        ; replace action tile in old position
        inc zpEnemyY                    ; current enemy, Y position on board
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardActionPtr),y
        cmp #SHAPE_PLAYER               ; is enemy catching the player?
        bne _moveEnemyDownJ2            ; no ->
        lsr playerAlive                 ; 0: player dead, 1: player alive
_moveEnemyDownJ2
        lda #SHAPE_ENEMY
        sta (zpBoardActionPtr),y        ; update action tile in new position
        lda #STEP_MIN                   ; reset Y position (fine) for new position
        sta zpEnemyStepY                ; current enemy, Y position (fine)
        jmp setEnemyAnimationClimb
_moveEnemyDownFinish
        jmp moveEnemyUpDownFinish


moveEnemyLeft
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldx zpEnemyStepX                ; current enemy, X position (fine)
        cpx #STEP_MIDDLE+1              ; x position fine > middle?
        bcs _moveEnemyLeftOk            ; yes ->
        ldy zpEnemyX                    ; current enemy, X position on board
        beq _moveEnemyLeftBlocked       ; enemy on left most position? -> can't move left
        dey
        lda (zpBoardActionPtr),y        ; fetch action tile left of enemy
        cmp #SHAPE_ENEMY                ; occupied by another enemy?
        beq _moveEnemyLeftBlocked       ; yes -> can't move left
        cmp #SHAPE_FLOOR_UNDIG          ; undiggable floor?
        beq _moveEnemyLeftBlocked       ; yes -> can't move left
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq _moveEnemyLeftBlocked       ; yes -> can't move left
        lda (zpBoardLayoutPtr),y        ; fetch layout tile left of enemy
        cmp #SHAPE_TRAP_DOOR            ; trap door?
        bne _moveEnemyLeftOk            ; no ->

_moveEnemyLeftBlocked
        jmp saveEnemyState
_moveEnemyLeftOk
        jsr prepareDisplayEnemy
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        jsr nudgeEnemyToStepYMiddle     ; nudge enemy towards StepY middle position
        lda #$ff                        ; orientation: facing left
        sta zpEnemyOrientation          ; $ff: enemy facing left, $01: facing right
        dec zpEnemyStepX                ; stepping into next tile to the left?
        bpl _moveEnemyLeftCheckGold     ; no ->
        jsr enemyCheckDropGold          ; enemy: check/drop gold
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG
        bne _moveEnemyLeftJ1
        lda #SHAPE_BLANK
_moveEnemyLeftJ1
        sta (zpBoardActionPtr),y        ; replace action tile in old position
        dec zpEnemyX                    ; current enemy, X position on board
        dey                             ; decrement to new tile column
        lda (zpBoardActionPtr),y        ; fetch action tile for new position
        cmp #SHAPE_PLAYER               ; is enemy catching the player?
        bne _moveEnemyLeftJ2            ; no ->
        lsr playerAlive                 ; mark player as dead ($00)
_moveEnemyLeftJ2
        lda #SHAPE_ENEMY
        sta (zpBoardActionPtr),y        ; update action tile in new position
        lda #STEP_MAX                   ; rightmost fine position in new tile
        sta zpEnemyStepX                ; current enemy, X position (fine)
        bne _moveEnemyLeftAnimate

_moveEnemyLeftCheckGold
        jsr enemyCheckPickupGold        ; check for and pickup gold
_moveEnemyLeftAnimate
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y        ; fetch layout tile for updated position
        cmp #SHAPE_BAR
        beq _moveEnemyLeftAnimateBar
        lda #EN_ANIM_PHASE_RUN_LEFT_0
        ldx #EN_ANIM_PHASE_RUN_LEFT_2
        bne _moveEnemyLeftAnimateJ1
_moveEnemyLeftAnimateBar
        lda #EN_ANIM_PHASE_BAR_LEFT_0
        ldx #EN_ANIM_PHASE_BAR_LEFT_2
_moveEnemyLeftAnimateJ1
        jsr updateEnemyAnimPhase
        jsr prepareDisplayEnemy
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
        jmp saveEnemyState


moveEnemyRight
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldx zpEnemyStepX                ; current enemy, X position (fine)
        cpx #STEP_MIDDLE                ; x position fine < middle?
        bcc _moveEnemyRightOk           ; yes ->
        ldy zpEnemyX                    ; current enemy, X position on board
        cpy #BOARD_WIDTH-1              ; enemy on right most position?
        beq _moveEnemyRightBlocked      ; yes -> can't move right
        iny
        lda (zpBoardActionPtr),y        ; fetch action tile right of enemy
        cmp #SHAPE_ENEMY                ; occupied by another enemy?
        beq _moveEnemyRightBlocked      ; yes -> can't move right
        cmp #SHAPE_FLOOR_UNDIG          ; undiggable floor?
        beq _moveEnemyRightBlocked      ; yes -> can't move right
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq _moveEnemyRightBlocked      ; yes -> can't move right
        lda (zpBoardLayoutPtr),y        ; fetch layout tile right of enemy
        cmp #SHAPE_TRAP_DOOR            ; trap door?
        bne _moveEnemyRightOk           ; no ->
_moveEnemyRightBlocked
        jmp saveEnemyState

_moveEnemyRightOk
        jsr prepareDisplayEnemy
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        jsr nudgeEnemyToStepYMiddle     ; nudge enemy towards StepY middle position
        lda #$01                        ; orientation: facing right
        sta zpEnemyOrientation          ; $ff: enemy facing left, $01: facing right
        inc zpEnemyStepX                ; current enemy, X position (fine)
        lda zpEnemyStepX                ; current enemy, X position (fine)
        cmp #STEP_MAX+1                 ; stepping into next tile to the right?
        bcc _moveEnemyRightCheckGold    ; no ->
        jsr enemyCheckDropGold          ; enemy: check/drop gold
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG
        bne _moveEnemyRightJ1
        lda #SHAPE_BLANK
_moveEnemyRightJ1
        sta (zpBoardActionPtr),y        ; replace action tile in old position
        inc zpEnemyX                    ; current enemy, X position on board
        iny                             ; increment to new tile column
        lda (zpBoardActionPtr),y        ; fetch action tile for new position
        cmp #SHAPE_PLAYER               ; is enemy catching the player?
        bne _moveEnemyRightJ2           ; no ->
        lsr playerAlive                 ; mark player as dead ($00)
_moveEnemyRightJ2
        lda #SHAPE_ENEMY
        sta (zpBoardActionPtr),y        ; update action tile in new position
        lda #STEP_MIN                   ; leftmost fine position in new tile
        sta zpEnemyStepX                ; current enemy, X position (fine)
        beq _moveEnemyRightAnimate

_moveEnemyRightCheckGold
        jsr enemyCheckPickupGold        ; check for and pickup gold
_moveEnemyRightAnimate
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y        ; fetch layout tile for updated position
        cmp #SHAPE_BAR
        beq _moveEnemyRightAnimateBar
        lda #EN_ANIM_PHASE_RUN_RIGHT_0
        ldx #EN_ANIM_PHASE_RUN_RIGHT_2
        bne _moveEnemyRightAnimateJ1    ; set enemy animation phase
_moveEnemyRightAnimateBar
        lda #EN_ANIM_PHASE_BAR_RIGHT_0  ; animate enemy on bar right
        ldx #EN_ANIM_PHASE_BAR_RIGHT_2
_moveEnemyRightAnimateJ1
        jsr updateEnemyAnimPhase
        jsr prepareDisplayEnemy
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
        jmp saveEnemyState


enemyFindNextMove
        stx zpEnemyXOrigin
        sty zpEnemyYOrigin
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyXOrigin
        lda (zpBoardLayoutPtr),y        ; fetch layout tile for current enemy position
        cmp #SHAPE_FLOOR_DIG
        bne _enemyFindMoveNotTrapped
        lda zpEnemyActionCtr            ; current enemy, special action counter
        beq _enemyFindMoveNotTrapped    ; no counter ->
        bmi _enemyFindMoveNotTrapped    ; drop gold counter in use ->
        lda #ENEMY_MOVE_UP              ; enemy climbs out of hole
        rts

_enemyFindMoveNotTrapped
        ldy zpEnemyYOrigin
        cpy zpPlayerY                   ; player, Y position on board
        beq enemyTryMoveOnPlayersRow    ; enemy on same row as player? ->
        jmp moveEnemyFindVerticalPath   ; find a path leading enemy up or down


enemyTryMoveOnPlayersRow                ; Y enemy == Y player
        ldy zpEnemyXOrigin
        sty zpEnemyPlayerRowX
        cpy zpPlayerX                   ; player, X position on board
        bcs _enemyTryMoveToLeftL        ; player x >= enemy x ->

_enemyTryMoveToRightL                   ; X enemy < X player
        inc zpEnemyPlayerRowX           ; extend projected path to the right
        ldy zpEnemyYOrigin
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyPlayerRowX
        lda (zpBoardLayoutPtr),y        ; fetch board layout for projected tile
        cmp #SHAPE_LADDER
        beq _moveEnemyRightOk
        cmp #SHAPE_BAR
        beq _moveEnemyRightOk
        ldy zpEnemyYOrigin
        cpy #BOARD_HEIGHT-1
        beq _moveEnemyRightOk
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyPlayerRowX
        lda (zpBoardLayoutPtr),y        ; read board layout tile below probed tile
        cmp #SHAPE_BLANK                ; enemy would fall through hole?
        beq moveEnemyRightAvoid         ; yes -> avoid
        cmp #SHAPE_TRAP_DOOR            ; enemy would fall through trap door?
        beq moveEnemyRightAvoid         ; yes -> avoid
_moveEnemyRightOk
        ldy zpEnemyPlayerRowX
        cpy zpPlayerX                   ; player, X position on board
        bne _enemyTryMoveToRightL       ; player not reached yet ->
        lda #ENEMY_MOVE_RIGHT           ; path is good! Move enemy to right!
        rts

        ; X enemy > X player (not equal since player would be already dead)
_enemyTryMoveToLeftL
        dec zpEnemyPlayerRowX           ; extend projected path to the left
        ldy zpEnemyYOrigin
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyPlayerRowX
        lda (zpBoardLayoutPtr),y        ; fetch board layout for projected tile
        cmp #SHAPE_LADDER
        beq _moveEnemyLeftOk
        cmp #SHAPE_BAR
        beq _moveEnemyLeftOk
        ldy zpEnemyYOrigin
        cpy #BOARD_HEIGHT-1
        beq _moveEnemyLeftOk
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyPlayerRowX
        lda (zpBoardLayoutPtr),y        ; read board layout tile below probed tile
        cmp #SHAPE_BLANK                ; enemy would fall through hole?
        beq moveEnemyLeftAvoid          ; yes -> avoid
        cmp #SHAPE_TRAP_DOOR            ; enemy would fall through trap door?
        beq moveEnemyLeftAvoid          ; yes -> avoid
_moveEnemyLeftOk
        ldy zpEnemyPlayerRowX
        cpy zpPlayerX                   ; player, X position on board
        bne _enemyTryMoveToLeftL        ; player not reached yet ->
        lda #ENEMY_MOVE_LEFT            ; path is good! Move enemy to left!
        rts

moveEnemyFindVerticalPath
moveEnemyRightAvoid
moveEnemyLeftAvoid
        lda #ENEMY_MOVE_NONE            ; initalize move with None (no move found yet)
        sta zpEnemyMoveDir              ; next move for enemy (pathfinder algorithm)
        lda #$ff                        ; score for initial position: $ff (worst)
        sta zpEnemyPositionScore        ; weighed distance between enemy and player
        ldx zpEnemyXOrigin
        ldy zpEnemyYOrigin
        jsr enemyProbeLeftRight         ; step 1: probe how far enemy can move to the left and to the right
        jsr enemyTryVerticalMove        ; step 2: try a direct vertical move (down or up)
        jsr enemyTryLeftVertical        ; step 3: try moving to the left, then vertical (down or up)
        jsr enemyTryRightVertical       ; step 4: try moving to the right, then vertical (down or up)
        lda zpEnemyMoveDir              ; next move for enemy (pathfinder algorithm)
        rts

enemyTryLeftVertExit
        rts

enemyTryLeftVertical                    ; try a move to the left followed by down/up
_tryLeftDown
        ldy zpEnemyMoveLeftPosX         ; reachable position to the left
        cpy zpEnemyXOrigin              ; equals current position of enemy?
        beq enemyTryLeftVertExit        ; yes ->
        ldy zpEnemyYOrigin
        cpy #BOARD_HEIGHT-1             ; enemy at bottom most position?
        beq _tryLeftUp                  ; yes ->
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyMoveLeftPosX
        lda (zpBoardLayoutPtr),y        ; get layout board tile below probe position
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq _tryLeftUp                  ; yes -> blocked
        cmp #SHAPE_FLOOR_UNDIG          ; solid floor?
        beq _tryLeftUp                  ; yes -> blocked
        ldx zpEnemyMoveLeftPosX
        ldy zpEnemyYOrigin
        jsr enemyProbeDown              ; probe down from horziontal probe position
        ldx zpEnemyMoveLeftPosX
        jsr calcEnemyPositionScore      ; calculate enemy position score
        cmp zpEnemyPositionScore        ; compare with best score so far
        bcs _tryLeftUp                  ; score is worse ->
        sta zpEnemyPositionScore        ; update best score for projected enemy position
        lda #ENEMY_MOVE_LEFT
        sta zpEnemyMoveDir              ; next move for enemy (pathfinder algorithm)
_tryLeftUp
        ldy zpEnemyYOrigin              ; enemy at the top of the screen?
        beq _tryLeftVerticalCont        ; yes ->
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyMoveLeftPosX         ; reachable position to the left
        lda (zpBoardLayoutPtr),y        ; get layout board tile at probe position
        cmp #SHAPE_LADDER               ; ladder?
        bne _tryLeftVerticalCont        ; no ->
        ldy zpEnemyYOrigin
        ldx zpEnemyMoveLeftPosX
        jsr enemyProbeUp                ; probe up from horziontal probe position
        ldx zpEnemyMoveLeftPosX
        jsr calcEnemyPositionScore      ; calculate enemy position score
        cmp zpEnemyPositionScore        ; compare with best score so far
        bcs _tryLeftVerticalCont        ; score is worse ->
        sta zpEnemyPositionScore        ; update best score for projected enemy position
        lda #ENEMY_MOVE_LEFT
        sta zpEnemyMoveDir              ; next move for enemy (pathfinder algorithm)
_tryLeftVerticalCont
        inc zpEnemyMoveLeftPosX         ; try one position closer to origin
        jmp enemyTryLeftVertical
enemyTryRightVertExit
        rts

enemyTryRightVertical
_tryRightDown
        ldy zpEnemyMoveRightPosX        ; reachable position to the right
        cpy zpEnemyXOrigin              ; equals current position of enemy?
        beq enemyTryRightVertExit       ; yes ->
        ldy zpEnemyYOrigin
        cpy #BOARD_HEIGHT-1             ; enemy at bottom most position?
        beq _tryRightUp                 ; yes ->
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyMoveRightPosX
        lda (zpBoardLayoutPtr),y        ; get layout board tile below probe position
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq _tryRightUp                 ; yes -> blocked
        cmp #SHAPE_FLOOR_UNDIG          ; solid floor?
        beq _tryRightUp                 ; yes -> blocked
        ldx zpEnemyMoveRightPosX
        ldy zpEnemyYOrigin
        jsr enemyProbeDown              ; probe down from horziontal probe position
        ldx zpEnemyMoveRightPosX
        jsr calcEnemyPositionScore      ; calculate enemy position score
        cmp zpEnemyPositionScore        ; compare with best score so far
        bcs _tryRightUp                 ; score is worse ->
        sta zpEnemyPositionScore        ; update best score for projected enemy position
        lda #ENEMY_MOVE_RIGHT
        sta zpEnemyMoveDir              ; next move for enemy (pathfinder algorithm)
_tryRightUp
        ldy zpEnemyYOrigin              ; enemy at the top of the screen?
        beq _tryRightVerticalCont       ; yes ->
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyMoveRightPosX        ; reachable position to the right
        lda (zpBoardLayoutPtr),y        ; get layout board tile at probe position
        cmp #SHAPE_LADDER               ; ladder?
        bne _tryRightVerticalCont       ; no ->
        ldy zpEnemyYOrigin
        ldx zpEnemyMoveRightPosX
        jsr enemyProbeUp                ; probe up from horziontal probe position
        ldx zpEnemyMoveRightPosX
        jsr calcEnemyPositionScore      ; calculate enemy position score
        cmp zpEnemyPositionScore        ; compare with best score so far
        bcs _tryRightVerticalCont       ; score is worse ->
        sta zpEnemyPositionScore        ; update best score for projected enemy position
        lda #ENEMY_MOVE_RIGHT
        sta zpEnemyMoveDir              ; next move for enemy (pathfinder algorithm)
_tryRightVerticalCont
        dec zpEnemyMoveRightPosX
        jmp enemyTryRightVertical


enemyTryVerticalMove                    ; try directly moving down or up
        ldy zpEnemyYOrigin
        cpy #BOARD_HEIGHT-1             ; enemy on bottom row?
        beq _enemyTryMoveUp             ; yes ->
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyXOrigin
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG
        beq _enemyTryMoveUp
        cmp #SHAPE_FLOOR_UNDIG
        beq _enemyTryMoveUp
        ldx zpEnemyXOrigin
        ldy zpEnemyYOrigin
        jsr enemyProbeDown
        ldx zpEnemyXOrigin
        jsr calcEnemyPositionScore      ; calculate enemy position score
        cmp zpEnemyPositionScore        ; score for projected enemy position (pathfinder algorithm)
        bcs _enemyTryMoveUp
        sta zpEnemyPositionScore        ; score for projected enemy position (pathfinder algorithm)
        lda #ENEMY_MOVE_DOWN
        sta zpEnemyMoveDir              ; next move for enemy (pathfinder algorithm)

_enemyTryMoveUp
        ldy zpEnemyYOrigin
        beq _exit
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyXOrigin
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_LADDER
        bne _exit
        ldx zpEnemyXOrigin
        ldy zpEnemyYOrigin
        jsr enemyProbeUp
        ldx zpEnemyXOrigin
        jsr calcEnemyPositionScore      ; calculate enemy position score
        cmp zpEnemyPositionScore        ; score for projected enemy position (pathfinder algorithm)
        bcs _exit
        sta zpEnemyPositionScore        ; score for projected enemy position (pathfinder algorithm)
        lda #ENEMY_MOVE_UP
        sta zpEnemyMoveDir              ; next move for enemy (pathfinder algorithm)
_exit
        rts


calcEnemyPositionScore
        sta zpTmpProbeY                 ; temporary variable enemy probe Y
        cmp zpPlayerY                   ; player, Y position on board
        bne _calcOnDifferentRow
_calcOnSameRow
        cpx zpEnemyX                    ; current enemy, X position on board
        bcc _calcLeftOfPlayer
_calcRightOfPlayer
        txa                             ; calculate (probe X - current X)
        sec
        sbc zpEnemyX                    ; current enemy, X position on board
        rts
_calcLeftOfPlayer
        stx zpTmpProbeX                 ; temporary variable enemy probe X
        lda zpEnemyX                    ; calculate (current X - probe X)
        sec
        sbc zpTmpProbeX                 ; temporary variable enemy probe X
        rts

_calcOnDifferentRow
        bcc _calcAbovePlayer            ; probe Y < player Y? (above)
_calcBelowPlayer
        sec
        sbc zpPlayerY                   ; calculate (probe Y - player Y)
        clc
        adc #200                        ; base score: Y probe below player
        rts
_calcAbovePlayer
        lda zpPlayerY                   ; player, Y position on board
        sec                             ; subtract
        sbc zpTmpProbeY                 ; temporary variable enemy probe Y
        clc
        adc #100                        ; base score: Y probe above player
        rts

enemyProbeUpExit
        lda zpEnemyProbeY               ; enemy path probe position Y
        rts

enemyProbeUp
        sty zpEnemyProbeY               ; enemy path probe position Y
        stx zpEnemyProbeX               ; enemy path probe position X
enemyProbeUpL1
_probeLeft
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyProbeX               ; enemy path probe position X
        lda (zpBoardLayoutPtr),y        ; fetch board tile at probe position
        cmp #SHAPE_LADDER               ; ladder at probe position?
        bne enemyProbeUpExit            ; no -> can't climb
        dec zpEnemyProbeY               ; enemy path probe position Y
        ldy zpEnemyProbeX               ; enemy in leftmost column?
        beq _probeRight                 ; yes -> skip probing for left platform
        dey
        lda (zpBoardLayoutPtr),y        ; fetch board tile below left ladder exit
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq _canExitLeft                ; yes -> possible exit to the left
        cmp #SHAPE_FLOOR_UNDIG          ; solid floor?
        beq _canExitLeft                ; yes -> possible exit to the left
        cmp #SHAPE_LADDER               ; ladder?
        beq _canExitLeft                ; yes -> possible exit to the left

        ldy zpEnemyProbeY               ; enemy path probe position Y
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyProbeX               ; enemy path probe position X
        dey
        lda (zpBoardLayoutPtr),y        ; fetch board tile at left ladder exit
        cmp #SHAPE_BAR
        bne _probeRight                 ; no -> exit to left not good, continue probing right
_canExitLeft
        ldy zpEnemyProbeY               ; enemy path probe position Y
        sty zpEnemyProbeBestY
        cpy zpPlayerY                   ; player, Y position on board
        bcc _enemyProbeUpFinish
        beq _enemyProbeUpFinish

_probeRight
        ldy zpEnemyProbeX               ; enemy path probe position X
        cpy #BOARD_WIDTH-1
        beq _enemyProbeUpContinue
        ldy zpEnemyProbeY               ; enemy path probe position Y
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyProbeX               ; enemy path probe position X
        iny
        lda (zpBoardLayoutPtr),y        ; fetch board tile below right ladder exit
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq _canExitRight               ; yes -> possible exit to the right
        cmp #SHAPE_FLOOR_UNDIG          ; solid floor?
        beq _canExitRight               ; yes -> possible exit to the right
        cmp #SHAPE_LADDER               ; ladder?
        beq _canExitRight               ; yes -> possible exit to the right
        ldy zpEnemyProbeY               ; enemy path probe position Y
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyProbeX               ; enemy path probe position X
        iny
        lda (zpBoardLayoutPtr),y        ; fetch board tile at right ladder exit
        cmp #SHAPE_BAR
        bne _enemyProbeUpContinue
_canExitRight
        ldy zpEnemyProbeY               ; enemy path probe position Y
        sty zpEnemyProbeBestY
        cpy zpPlayerY                   ; player, Y position on board
        bcc _enemyProbeUpFinish         ; enemy on level with or above player? ->
        beq _enemyProbeUpFinish

_enemyProbeUpContinue
        ldy zpEnemyProbeY               ; enemy path probe position Y
        cpy #$01                        ; top line reached?
        bcc _topReached                 ; yes ->
        jmp enemyProbeUpL1              ; else, continue probing up
_topReached
        tya
        rts

_enemyProbeUpFinish
        lda zpEnemyProbeBestY           ; reachable Y-position closest to player
        rts


enemyProbeDownExit
        lda zpEnemyProbeY               ; enemy path probe position Y
        rts

enemyProbeDown
        sty zpEnemyProbeY               ; enemy path probe position Y
        stx zpEnemyProbeX               ; enemy path probe position X
enemyProbeDownL1
_probeLeft
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyProbeX               ; enemy path probe position X
        lda (zpBoardLayoutPtr),y        ; fetch board tile below probe position
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq enemyProbeDownExit          ; yes -> not passable
        cmp #SHAPE_FLOOR_UNDIG          ; solid floor?
        beq enemyProbeDownExit          ; yes -> not passable
        ldy zpEnemyProbeY               ; enemy path probe position Y
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyProbeX               ; enemy path probe position X
        lda (zpBoardLayoutPtr),y        ; fetch board tile at probe position
        cmp #SHAPE_BLANK                ; enemy not supported by ladder?
        beq _enemyProbeDownContinue     ; not supported -> continue falling
        cpy #$00                        ; enemy in leftmost column?
        beq _probeRight                 ; yes -> skip probing for left platform
        dey
        lda (zpBoardLayoutPtr),y        ; fetch board tile left of probe position
        cmp #SHAPE_BAR                  ; bar?
        beq _canExitLeft                ; yes -> reached row is useful
        ldy zpEnemyProbeY               ; enemy path probe position Y
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyProbeX               ; enemy path probe position X
        dey
        lda (zpBoardLayoutPtr),y        ; fetch board tile below probe position to left
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq _canExitLeft                ; yes -> possible exit to the left
        cmp #SHAPE_FLOOR_UNDIG          ; solid floor?
        beq _canExitLeft                ; yes -> possible exit to the left
        cmp #SHAPE_LADDER               ; ladder?
        bne _probeRight                 ; no -> exit to left not good, continue probing right
_canExitLeft
        ldy zpEnemyProbeY               ; enemy path probe position Y
        sty zpEnemyProbeBestY
        cpy zpPlayerY                   ; player, Y position on board
        bcs _enemyProbeDownFinish       ; enemy on level with or below player? ->

_probeRight
        ldy zpEnemyProbeX               ; enemy path probe position X
        cpy #BOARD_WIDTH-1
        bcs _enemyProbeDownContinue
        iny
        lda (zpBoardLayoutPtr),y        ; fetch board tile right of probe position
        cmp #SHAPE_BAR                  ; bar?
        beq _canExitRight               ; yes -> reached row is useful
        ldy zpEnemyProbeY               ; enemy path probe position Y
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyProbeX               ; enemy path probe position X
        iny
        lda (zpBoardLayoutPtr),y        ; fetch board tile below probe position to right
        cmp #SHAPE_FLOOR_DIG            ; diggable floor?
        beq _canExitRight               ; yes -> possible exit to the right
        cmp #SHAPE_LADDER               ; ladder?
        beq _canExitRight               ; yes -> possible exit to the right
        cmp #SHAPE_FLOOR_UNDIG          ; solid floor?
        bne _enemyProbeDownContinue
_canExitRight
        ldy zpEnemyProbeY               ; enemy path probe position Y
        sty zpEnemyProbeBestY
        cpy zpPlayerY                   ; player, Y position on board
        bcs _enemyProbeDownFinish       ; enemy on level with or below player? ->

_enemyProbeDownContinue
        inc zpEnemyProbeY               ; enemy path probe position Y
        ldy zpEnemyProbeY               ; enemy path probe position Y
        cpy #BOARD_HEIGHT               ; bottom reached?
        bcs _bottomReached              ; yes ->
        jmp enemyProbeDownL1            ; else, continue probing down
_bottomReached
        lda #BOARD_HEIGHT-1
        rts
_enemyProbeDownFinish
        lda zpEnemyProbeBestY           ; reachable Y-position closest to player
        rts

; pathfinder step 1: probe how far enemy can move to the left and to the right
enemyProbeLeftRight
        stx zpEnemyMoveLeftPosX         ; minimum reachable X position on current row
        stx zpEnemyMoveRightPosX        ; maximum reachable X position on current row
        sty zpEnemyTestY                ; enemy current Y pos
_probeEnemyLeftL
        lda zpEnemyMoveLeftPosX
        beq _moveEnemyLeftBlocked       ; in leftmost column? ->
        ldy zpEnemyTestY
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpEnemyMoveLeftPosX
        dey
        lda (zpBoardActionPtr),y        ; get action tile left of current seek position
        cmp #SHAPE_FLOOR_DIG
        beq _moveEnemyLeftBlocked
        cmp #SHAPE_FLOOR_UNDIG
        beq _moveEnemyLeftBlocked
        cmp #SHAPE_LADDER
        beq _moveEnemyLeftOk
        cmp #SHAPE_BAR
        beq _moveEnemyLeftOk
        ldy zpEnemyTestY
        cpy #BOARD_HEIGHT-1
        beq _moveEnemyLeftOk
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyMoveLeftPosX
        dey
        lda (zpBoardLayoutPtr),y        ; get layout tile left and below of current seek position
        cmp #SHAPE_FLOOR_DIG
        beq _moveEnemyLeftOk
        cmp #SHAPE_FLOOR_UNDIG
        beq _moveEnemyLeftOk
        cmp #SHAPE_LADDER
        bne _moveEnemyLeftLast

_moveEnemyLeftOk
        dec zpEnemyMoveLeftPosX
        bpl _probeEnemyLeftL
_moveEnemyLeftLast                      ; last possible move to the left
        dec zpEnemyMoveLeftPosX

_moveEnemyLeftBlocked
_probeEnemyRightL
        lda zpEnemyMoveRightPosX
        cmp #BOARD_WIDTH-1
        beq _moveEnemyRightBlocked
        ldy zpEnemyTestY
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpEnemyMoveRightPosX
        iny
        lda (zpBoardActionPtr),y
        cmp #SHAPE_FLOOR_DIG
        beq _moveEnemyRightBlocked
        cmp #SHAPE_FLOOR_UNDIG
        beq _moveEnemyRightBlocked
        cmp #SHAPE_LADDER
        beq _moveEnemyRightOk
        cmp #SHAPE_BAR
        beq _moveEnemyRightOk
        ldy zpEnemyTestY
        cpy #BOARD_HEIGHT-1
        beq _moveEnemyRightOk
        lda boardLayoutOffsetLb+1,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb+1,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyMoveRightPosX
        iny
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_FLOOR_DIG
        beq _moveEnemyRightOk
        cmp #SHAPE_FLOOR_UNDIG
        beq _moveEnemyRightOk
        cmp #SHAPE_LADDER
        bne _moveEnemyRightLast
_moveEnemyRightOk
        inc zpEnemyMoveRightPosX
        bpl _probeEnemyRightL
_moveEnemyRightLast                     ; last possible move to the right
        inc zpEnemyMoveRightPosX
_moveEnemyRightBlocked
        rts

prepareDisplayEnemy
        ldx zpEnemyX                    ; current enemy, X position on board
        ldy zpEnemyStepX                ; current enemy, X position (fine)
        jsr calcPixelPosX               ; calculate pixel position (X)
        stx zpTmpPixelPosX              ; temporarily store pixel position (X)
        ldy zpEnemyY                    ; current enemy, Y position on board
        ldx zpEnemyStepY                ; current enemy, Y position (fine)
        jsr calcPixelPosY               ; calculate pixel position (Y)
        ldx zpEnemyAnimPhase            ; enemy animation phase
        lda tabEnemyAnimPhaseToShape,x  ; get shape data offset from animation phase
        ldx zpTmpPixelPosX              ; restore pixel position (X)
        rts

enemyCheckPickupGold                    ; check for and pickup gold
        lda zpEnemyStepX                ; current enemy, X position (fine)
        cmp #STEP_MIDDLE                ; x-centered on board position?
        bne _exit                       ; no ->
        lda zpEnemyStepY                ; current enemy, Y position (fine)
        cmp #STEP_MIDDLE                ; y-centered on board position?
        bne _exit                       ; no ->
        ldy zpEnemyY                    ; current enemy, Y position on board
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        lda (zpBoardLayoutPtr),y
        cmp #SHAPE_GOLD_CHEST           ; enemy on position containing gold chest?
        bne _exit                       ; no ->
        lda zpEnemyActionCtr            ; current enemy, special action counter
        bmi _exit                       ; yes ->
        lda #$ff                        ; max. drop gold counter
        sec
        sbc enemyRespawnCol             ; randomize counter using respawn column
        sta zpEnemyActionCtr            ; current enemy, special action counter
        lda #SHAPE_BLANK                ; replace gold chest shape by blank shape
        sta (zpBoardLayoutPtr),y
        ldy zpEnemyY                    ; current enemy, Y position on board
        sty zpCursorRow
        ldy zpEnemyX                    ; current enemy, X position on board
        sty zpCursorCol
        jsr replaceTileBitmap1
        ldy zpCursorRow
        ldx zpCursorCol
        jsr convertBoardPosToPixelPos
        lda #SHAPE_GOLD_CHEST
        jmp cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
_exit
        rts

enemyCheckDropGold                      ; enemy: check/drop gold
        lda zpEnemyActionCtr            ; does enemy carry gold? (<0)
        bpl _exit                       ; no ->
        inc zpEnemyActionCtr            ; increment gold counter
        bne _exit                       ; counter is not finished ->
        ldy zpEnemyY                    ; current enemy, Y position on board
        sty zpCursorRow
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpEnemyX                    ; current enemy, X position on board
        sty zpCursorCol
        lda (zpBoardLayoutPtr),y        ; get layout tile at enemy's position
        cmp #SHAPE_BLANK                ; space for enemy to drop gold?
        bne _noSpaceToDropGold          ; no ->
        lda #SHAPE_GOLD_CHEST
        sta (zpBoardLayoutPtr),y        ; store gold chest in board layout
        jsr replaceTileBitmap1          ; replace tile in buffer bitmap
        ldy zpCursorRow
        ldx zpCursorCol
        jsr convertBoardPosToPixelPos
        lda #SHAPE_GOLD_CHEST
        jmp pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
_noSpaceToDropGold
        dec zpEnemyActionCtr            ; retry to drop gold in next enemy move
_exit
        rts

updateEnemyAnimPhase
        inc zpEnemyAnimPhase            ; increment enemy animation phase
        cmp zpEnemyAnimPhase            ; minimum < animation phase?
        bcc _checkEnemyAnimPhaseMax     ; yes ->
_setEnemyAnimPhaseToMin
        sta zpEnemyAnimPhase            ; set animation phase to minimum
        rts
_checkEnemyAnimPhaseMax
        cpx zpEnemyAnimPhase            ; maximum < animation phase (too big)?
        bcc _setEnemyAnimPhaseToMin     ; yes -> set animation phase to minimum
        rts

nudgeEnemyToStepXMiddle                 ; nudge enemy towards StepX middle position
        lda zpEnemyStepX                ; current enemy, X position (fine)
        cmp #STEP_MIDDLE
        bcc _nudgeEnemyRight            ; enemy x step < middle: -> inc x step
        beq _exit                       ; enemy x step == middle: -> exit
        dec zpEnemyStepX                ; current enemy, X position (fine)
        jmp enemyCheckPickupGold        ; check for and pickup gold
_nudgeEnemyRight
        inc zpEnemyStepX                ; current enemy, X position (fine)
        jmp enemyCheckPickupGold        ; check for and pickup gold
_exit
        rts

nudgeEnemyToStepYMiddle                 ; nudge enemy towards StepY middle position
        lda zpEnemyStepY                ; current enemy, Y position (fine)
        cmp #STEP_MIDDLE
        bcc _nudgeEnemyDown             ; enemy y step < middle: -> inc y step
        beq _exit                       ; enemy y step == middle: -> exit
        dec zpEnemyStepY                ; current enemy, Y position (fine)
        jmp enemyCheckPickupGold        ; check for and pickup gold
_nudgeEnemyDown
        inc zpEnemyStepY                ; current enemy, Y position (fine)
        jmp enemyCheckPickupGold        ; check for and pickup gold
_exit
        rts

saveEnemyState
        ldx enemyIndex                  ; current enemy, index
        lda zpEnemyX                    ; current enemy, X position on board
        sta enemiesX,x
        lda zpEnemyY                    ; current enemy, Y position on board
        sta enemiesY,x
        lda zpEnemyStepX                ; current enemy, X position (fine)
        sta enemiesStepX,x
        lda zpEnemyStepY                ; current enemy, Y position (fine)
        sta enemiesStepY,x
        lda zpEnemyActionCtr            ; current enemy, special action counter
        sta enemiesActionCtr,x
        lda zpEnemyOrientation          ; $ff: enemy facing left, $01: facing right
        sta enemiesOrientation,x
        lda zpEnemyAnimPhase            ; enemy animation phase
        sta enemiesAnimPhase,x
        rts

loadEnemyState                          ; set enemy variables for current enemy ID from enemy object table
        ldx enemyIndex                  ; current enemy, index
        lda enemiesX,x
        sta zpEnemyX                    ; current enemy, X position on board
        lda enemiesY,x
        sta zpEnemyY                    ; current enemy, Y position on board
        lda enemiesStepX,x
        sta zpEnemyStepX                ; current enemy, X position (fine)
        lda enemiesStepY,x
        sta zpEnemyStepY                ; current enemy, Y position (fine)
        lda enemiesAnimPhase,x
        sta zpEnemyAnimPhase            ; enemy animation phase
        lda enemiesOrientation,x
        sta zpEnemyOrientation          ; $ff: enemy facing left, $01: facing right
        lda enemiesActionCtr,x
        sta zpEnemyActionCtr            ; current enemy, special action counter
        rts

respawnEnemiesHandleHoles               ; respawn enemies, handle holes
        jsr checkRespawnEnemies         ; check whether any enemy can respawn and do it
        inc enemyRespawnCol             ; increment column for respawning enemy
        lda enemyRespawnCol
        cmp #BOARD_WIDTH                ; ensure respawn column < BOARD_WIDTH
        bcc _respawnColOk               ; respawn col < BOARD_WIDTH ->
        lda #$00                        ; reset respawn column to $00
        sta enemyRespawnCol
_respawnColOk

        ldx #MAX_HOLES_OPEN             ; maximum number of open holes (30)
_handleHolesL
        lda holesOpenCtr,x              ; time until hole closes
        stx zpHoleIdx                   ; current hole index
        bne _handleHoleJ1               ; active hole entry ->
        jmp _continueNextHole
_handleHoleJ1
        dec holesOpenCtr,x              ; decrement time until hole closes
        beq _handleHoleFinish           ; the hole finally closes ->
        lda holesPosX,x                 ; fetch hole position (X,Y)
        sta zpCursorCol
        lda holesPosY,x
        sta zpCursorRow
        lda holesOpenCtr,x              ; time until hole closes
        cmp #20                         ; time for animation phase #0?
        bne _handleHoleJ2
        lda #ANIM_CLOSE_HOLE_0          ; closing hole animation phase #0
_animateClosingHole
        jsr replaceTileBitmap1          ; update tile in buffer bitmap
        ldx zpCursorCol
        ldy zpCursorRow
        jsr convertBoardPosToPixelPos
        lda #SHAPE_BLANK
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1

_continueNextHoleJmp
        jmp _continueNextHole

_handleHoleJ2
        cmp #10                         ; time for animation phase #1?
        bne _continueNextHoleJmp
        lda #ANIM_CLOSE_HOLE_1          ; closing hole animation phase #1
        bne _animateClosingHole         ; update closing hole in bitmap

_handleHoleFinish                       ; a hole finally closes
        ldx zpHoleIdx                   ; current hole index
        ldy holesPosY,x
        sty zpCursorRow
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy holesPosX,x
        sty zpCursorCol
        lda (zpBoardActionPtr),y        ; read action tile at hole's position
        cmp #SHAPE_BLANK                ; location is empty?
        bne _handleHoleFinishOccupied   ; no -> hole is occupied
        jmp _updateHoleAsClosed

_handleHoleFinishOccupied
        cmp #SHAPE_PLAYER               ; player at hole's location?
        bne _handleHoleFinishOccupiedJ1 ; no ->
        lsr playerAlive                 ; yes: mark player as dead (0: player dead, 1: player alive)
_handleHoleFinishOccupiedJ1
        cmp #SHAPE_ENEMY                ; enemy at hole's location?
        beq _handleHoleFinishKillEnemy
        cmp #SHAPE_GOLD_CHEST           ; gold chest at hole's location?
        bne _handleHoleFinishOccupiedJ2 ; occupied with something else (ignore)
        dec goldLeft
_handleHoleFinishOccupiedJ2
        jmp _updateHoleAsClosed

_handleHoleFinishKillEnemy
        lda #SHAPE_FLOOR_DIG            ; diggable floor
        sta (zpBoardActionPtr),y        ; replace action board tile
        sta (zpBoardLayoutPtr),y        ; replace layout board tile
        jsr replaceTileBitmap0          ; update in display bitmap
        lda #SHAPE_FLOOR_DIG
        jsr replaceTileBitmap1          ; update in buffer bitmap
        ldx numEnemies
_findKilledEnemyL
        lda enemiesX,x
        cmp zpCursorCol                 ; enemy at hole's column?
        beq _findKilledEnemyJ1
        jmp _continueNextEnemy
_findKilledEnemyJ1
        lda enemiesY,x
        cmp zpCursorRow                 ; enemy at hole's row?
        bne _continueNextEnemy

        lda tabEnemySpriteDisable,x     ; disable mask for sprite x
        and VicSpriteEnable             ; disable enemie's sprite
        sta VicSpriteEnable
        lda enemiesActionCtr,x          ; was the enemy carrying gold?
        bpl _killEnemyJ1                ; no ->
        dec goldLeft                    ; yes -> this gold chest is lost
_killEnemyJ1
        lda #$7f                        ; maximum: remaining time enemy is trapped
        sta enemiesActionCtr,x          ; set enemy trapped time to maximum (why? $0d at end of U1 command?)
        stx enemyIndex                  ; current enemy, index
        jsr loadEnemyState              ; set enemy variables for current enemy ID from enemy object table
        jsr prepareDisplayEnemy
        jsr cookieCutTileFromBuffer     ; cut out shape (A), add in shape from Bitmap1
        ldx enemyIndex                  ; current enemy, index

        ; find a respawn location for enemy X

        ldy #$01                        ; start searching in row 1
        sty zpEnemyRespawnRow           ; row for respawning enemy (local)
_findRespawnLocationL0
        ldy zpEnemyRespawnRow
        lda boardLayoutOffsetLb,y
        sta zpBoardLayoutPtr+0
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy enemyRespawnCol             ; column for respawning enemy
_findRespawnLocationL1
        lda (zpBoardLayoutPtr),y        ; potential respawn tile in board layout
        cmp #SHAPE_BLANK                ; is this an empty tile?
        beq _killEnemyJ2                ; yes -> continue with killing enemy
        inc enemyRespawnCol
        ldy enemyRespawnCol
        cpy #BOARD_WIDTH
        bcc _findRespawnLocationL1
        inc zpEnemyRespawnRow           ; try to find respawn tile in next row
        lda #$00
        sta enemyRespawnCol
        beq _findRespawnLocationL0
_killEnemyJ2
        tya
        sta enemiesX,x
        lda zpEnemyRespawnRow
        sta enemiesY,x
        lda #ENEMY_RESPAWN_CTR_INIT     ; time until enemy respawns
        sta enemiesRespawnCtr,x         ; time left until enemy respawns
        lda #STEP_MIDDLE                ; center enemy within start tile
        sta enemiesStepY,x
        sta enemiesStepX,x
        lda #$00
        sta enemiesAnimPhase,x          ; enemy animation phase
        ldy #>SCORE_KILL_ENEMY          ; score for burying an enemy: 0075
        lda #<SCORE_KILL_ENEMY
        jsr addPrintScore
        jmp _continueNextHole

_continueNextEnemy
        dex                             ; next enemy index
        beq _updateHoleAsClosed         ; no enemy found ->
        jmp _findKilledEnemyL           ; continue loop finding killed enemy

_updateHoleAsClosed
        lda #SHAPE_FLOOR_DIG
        sta (zpBoardActionPtr),y
        jsr replaceTileBitmap0
        lda #SHAPE_FLOOR_DIG
        jsr replaceTileBitmap1
_continueNextHole
        ldx zpHoleIdx                   ; current hole index
        dex                             ; dec hole index (next hole)
        bmi _exit
        jmp _handleHolesL

_exit
exitNoEnemies
        rts

checkRespawnEnemies
        ldx numEnemies
        beq exitNoEnemies
        lda enemyIndex                  ; store current enemy, index
        pha
_respawnEnemiesL
        lda enemiesRespawnCtr,x         ; time left until enemy respawns
        beq _continueNextEnemy
        stx enemyIndex                  ; current enemy, index
        jsr loadEnemyState              ; set enemy variables for current enemy ID from enemy object table
        lda #$7f
        sta enemiesActionCtr,x          ; >0: remaining time enemy is trapped (decreasing)
        lda enemiesX,x
        sta zpCursorCol
        lda enemiesY,x
        sta zpCursorRow
        dec enemiesRespawnCtr,x         ; time left until enemy respawns
        beq _enemyRespawnFinish         ; respawn is finishing ->
        lda enemiesRespawnCtr,x         ; time left until enemy respawns
        cmp #19                         ; respawn counter phase #1?
        bne _respawnEnemyJ1
        lda #ANIM_RESPAWN_0             ; (will be converted to shape $2a)
        jsr replaceTileBitmap1
        jsr prepareDisplayEnemy
        lda #ANIM_RESPAWN_0
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
        jmp _respawnEnemyJ2
_respawnEnemyJ1
        cmp #10                         ; respawn counter phase #2?
        bne _continueNextEnemy
        lda #ANIM_RESPAWN_1             ; (will be converted to shape $2b)
        jsr replaceTileBitmap1
        jsr prepareDisplayEnemy
        lda #ANIM_RESPAWN_1
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)

_respawnEnemyJ2
        ldx enemyIndex                  ; current enemy, index
_continueNextEnemy
        dex
        bne _respawnEnemiesL

        pla                             ; restore current enemy index
        sta enemyIndex                  ; current enemy, index
        rts

_enemyRespawnFinish
        ldy zpCursorRow
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldx enemyIndex                  ; current enemy, index
        inc enemiesRespawnCtr,x         ; inc time left until enemy respawns (back to 1)
        ldy zpCursorCol                 ; (allows to retry next iteration if the location is busy)
        lda (zpBoardActionPtr),y        ; get action board tile at respawn location
        bne _continueNextEnemy          ; respawn location is busy -> defer to next cycle

        lda #SHAPE_ENEMY
        sta (zpBoardActionPtr),y        ; set enemy in action board
        lda #SHAPE_BLANK
        jsr replaceTileBitmap1          ; clear tile in buffer bitmap
        lda #SHAPE_BLANK
        jsr replaceTileBitmap0          ; clear tile in visible bitmap

        lda #$00
        ldx enemyIndex                  ; current enemy, index
        sta enemiesActionCtr,x          ; enemy carries no gold, not in a hole
        sta enemiesRespawnCtr,x         ; time left until enemy respawns
        lda #SHAPE_ENEMY
        jsr replaceTileBitmap0          ; set enemy sprite
        ldx enemyIndex                  ; current enemy, index
        lda tabEnemySpriteEnable,x      ; enable enemy sprite
        ora VicSpriteEnable
        sta VicSpriteEnable
        jmp _continueNextEnemy

tabEnemySpriteEnable
        .byte $00,$04,$08,$10,$40,$80

tabEnemySpriteDisable
        .byte $00,$fb,$f7,$ef,$bf,$7f


clearBitmapsPrintFooter
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        jsr clearBitmap1                ; clear bitmap 1

printBoardFooter                        ; draw solid ground and print status line
        ldx #$22                        ; solid ground has $22 columns of $aa byte
        lda #>areaCopyProtection        ; set page (copy protection integrity check)
        sta checkAdcIntegrityPtr+2      ; self-modifying code (set checked data ptr hb)
        lda zpBitmapPage2               ; active bitmap page for printing
        cmp #>Bitmap1
        beq _printBedrockBitmap1

        ; draw bedrock (4 pixels high) on bitmap 0
        lda #$0a                        ; bit pattern: left edge of solid ground
        sta $3b90                       ; draw left edge of solid ground
        sta $3b91
        sta $3b92
        sta $3b93
        lda #$a0                        ; bit pattern: right edge of solid ground
        sta $3ca8                       ; draw right edge of solid ground
        sta $3ca9
        sta $3caa
        sta $3cab
_drawSolidGroundL1
        ldy #$03                        ; iterator: in total draw 4 times
        lda #$aa                        ; bit pattern: solid ground
_drawSolidGroundL2
_staBmpDst1
        sta $3b98,y                     ; draw solid ground (4 pixel high)
        dey
        bpl _drawSolidGroundL2
        lda _staBmpDst1+1
        clc
        adc #$08                        ; next column in line
        sta _staBmpDst1+1
        bcc _skipInc1
        inc _staBmpDst1+2
_skipInc1
        dex
        bne _drawSolidGroundL1
        lda #$3b
        sta _staBmpDst1+2
        lda #$98
        sta _staBmpDst1+1
        bne _printStatusLine

_printBedrockBitmap1
        ; draw bedrock (4 pixels high) on bitmap 1
        lda #$0a                        ; bit pattern: left edge of solid ground
        sta $5b90                       ; draw left edge of solid ground
        sta $5b91
        sta $5b92
        sta $5b93
        lda #$a0                        ; bit pattern: right edge of solid ground
        sta $5ca8                       ; draw right edge of solid ground
        sta $5ca9
        sta $5caa
        sta $5cab
_drawSolidGroundL3
        ldy #$03                        ; iterator: in total draw 4 times
        lda #$aa                        ; bit pattern: solid ground
_drawSolidGroundL4
_staBmpDst2
        sta $5b98,y
        dey
        bpl _drawSolidGroundL4
        lda _staBmpDst2+1
        clc
        adc #$08                        ; next column in line
        sta _staBmpDst2+1
        bcc _skipInc2
        inc _staBmpDst2+2
_skipInc2
        dex
        bne _drawSolidGroundL3
        lda #$5b
        sta _staBmpDst2+2
        lda #$98
        sta _staBmpDst2+1

_printStatusLine
        lda #BOARD_HEIGHT
        sta zpCursorRow
        lda #$00
        sta zpCursorCol
        jsr printString
.enc "high"
        .text "SCORE        MEN    LEVEL   ",$00


        ; game integrity check (check copy protection results)
        ; checks $8e11-$8e56

        ldy #$45
        lda #$db                        ; initial checksum
checkIntegrityL
        clc
checkAdcIntegrityPtr
        adc la800,y                     ; add byte to check to checksum
        eor #$01                        ; flip bit 0
        dey                             ; iterate over $45 bytes
        bpl checkIntegrityL             ; continue adding to checksum

        cmp #$c8                        ; check game integrity
        beq _checkIntegrityOk           ; integrity good ->
        inc ldaShapes+1                 ; mess up shape print routine
                                        ; (punish game modification)
_checkIntegrityOk
        lda #$a8                        ; revert pointer in integrity check
        sta checkAdcIntegrityPtr+2      ; self-modifying code (set checked data ptr hb)
        jsr printLives                  ; print number of lives
        jsr printLevel                  ; print current level
        lda #$00                        ; add to score: 0000
        tay
        jmp addPrintScore               ; print score (6 digits)

printLives
        lda lives
        ldx #16                         ; column offset: Men

printThreeDigitNumber
        stx zpCursorCol                 ; set cursor column
        jsr convertHexToDecimal         ; convert hex number to 3 digit decimal
        lda #BOARD_HEIGHT               ; row offset for status: Score, Men, Level
        sta zpCursorRow
        lda digitHigh
        jsr printDigit
        lda digitMid
        jsr printDigit
        lda digitLow
        jmp printDigit

printLevel
        lda displayedLevel              ; current level (displayed)
        ldx #25                         ; column offset: Level
        bne printThreeDigitNumber

addPrintScore
        clc                             ; add A to score (low byte)
        sed
        adc score+0
        sta score+0
        tya                             ; add Y to score (mid byte)
        adc score+1
        sta score+1
        lda #$00
        adc score+2
        sta score+2
        lda #$00
        adc score+3
        sta score+3
        cld

        lda #$05                        ; column
        sta zpCursorCol
        lda #$10                        ; row
        sta zpCursorRow
        lda score+3
        jsr splitBcdIntoDigits
        lda digitLow
        jsr printDigit
        lda score+2
        jsr splitBcdIntoDigits
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit
        lda score+1
        jsr splitBcdIntoDigits
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit
        lda score+0
        jsr splitBcdIntoDigits
        lda digitMid
        jsr printDigit
        lda digitLow
        jmp printDigit

splitBcdIntoDigits
        sta digitMid
        and #$0f                        ; extract digit from low nibble
        sta digitLow
        lda digitMid
        lsr                             ; extract digit from high nibble
        lsr
        lsr
        lsr
        sta digitMid
        rts

convertHexToDecimal                     ; convert hex number to 3 digit decimal
        ldx #$00
        stx digitMid
        stx digitHigh
_handleHundredsLoop
        cmp #100
        bcc _handleTensLoop
        inc digitHigh                   ; inc high digit (x 100)
        sbc #100
        bne _handleHundredsLoop
_handleTensLoop
        cmp #10
        bcc _handleLowDigit
        inc digitMid                    ; inc mid digit (x 10)
        sbc #10
        bne _handleTensLoop
_handleLowDigit
        sta digitLow                    ; set low digit (x 1)
        rts

printDigit
        clc
        adc #CHAR_0                     ; character "0" ($3b)
        ldx zpBitmapPage2               ; active bitmap page for printing
        cpx #>Bitmap1
        beq printTileIncCol
        jsr replaceTileBitmap0
        inc zpCursorCol
        rts

printTileIncCol
        jsr replaceTileBitmap1
        inc zpCursorCol
        rts

convertAsciiToShapeId
        cmp #'A'                        ; ascii high: $c1
        bcc _notLetter
        cmp #'Z'+1                      ; ascii high: $db
        bcc _convertLetter
_notLetter
        ldx #$7c
        cmp #' '                        ; ascii high: $a0
        beq _convertOther
        ldx #$db                        ; '>'
        cmp #$be
        beq _convertOther
        inx                             ; '.'
        cmp #'.'
        beq _convertOther
        inx
        cmp #'('
        beq _convertOther
        inx
        cmp #')'
        beq _convertOther
        inx
        cmp #'/'
        beq _convertOther
        inx
        cmp #'-'
        beq _convertOther
        inx
        cmp #'<'
        beq _convertOther
        lda #$10                        ; shape: cursor
        rts

_convertOther
        txa
_convertLetter
        sec
        sbc #('A'-SHAPE_A)              ; convert the letter character to printable shape (#$7c)
        rts

printChar
        cmp #$8d
        beq printNewline
        jsr convertAsciiToShapeId
        ldx zpBitmapPage2               ; active bitmap page for printing
        cpx #>Bitmap1
        beq _printCharBitmap1
        jsr replaceTileBitmap0          ; print to bitmap 0 (displayed)
        inc zpCursorCol
        rts
_printCharBitmap1
        jsr replaceTileBitmap1          ; print to bitmap 1 (buffer)
        inc zpCursorCol
        rts

printNewline
        inc zpCursorRow
        lda #$00
        sta zpCursorCol
        rts

printString
        pla                             ; pull return address from stac
        sta zpPrintDataPtr+0            ; (data pointer)
        pla
        sta zpPrintDataPtr+1
        bne _incDataPtr                 ; inc data pointer, then enter loop

_printCharL
        ldy #$00                        ; index (always 0)
        lda (zpPrintDataPtr),y          ; read character to print
        beq _printExit                  ; end of string ->
        jsr printChar                   ; print character
_incDataPtr
        inc zpPrintDataPtr+0
        bne _printCharL
        inc zpPrintDataPtr+1
        bne _printCharL
_printExit
        lda zpPrintDataPtr+1            ; push return address on stack
        pha
        lda zpPrintDataPtr+0
        pha
        rts

soundBell
        sei
        lda #$60                        ; pitch
        sta SidVoice1FreqHb
        ldy #$38
        ldx #$00
_delayL
        dex
        bne _delayL
        dey
        bne _delayL
        sty SidVoice1FreqHb
        cli
        rts

waitForUserKey
        sta charAtCursor                ; current character at cursor
_flashCursorLoop
        lda #$00                        ; init flash cursor counter
        sta flashCursorCtr+0
        lda #$0a
        sta flashCursorCtr+1
        lda #SHAPE_BLANK                ; shape: blank cursor ($00)
        ldx charAtCursor                ; current character at cursor
        bne _printCursor                ; not blank ->
        lda #SHAPE_CURSOR               ; shape: solid cursor ($0a)
_printCursor
        jsr replaceTileBitmap0          ; print blank shape or cursor
_delayCursorL1
        jsr detectUserInput             ; check for user input
        bcs _exitCursorLoop             ; joystick or keyboard event detected ->
        dec flashCursorCtr+0
        bne _delayCursorL1
        dec flashCursorCtr+1
        bne _delayCursorL1

        lda charAtCursor                ; current character at cursor
        jsr replaceTileBitmap0          ; print character at cursor

        lda #$00                        ; init flash cursor counter
        sta flashCursorCtr+0
        lda #$0a
        sta flashCursorCtr+1
_delayCursorL2
        jsr detectUserInput             ; check for user input
        bcs _exitCursorLoop             ; joystick or keyboard event detected ->
        dec flashCursorCtr+0
        bne _delayCursorL2
        dec flashCursorCtr+1
        bne _delayCursorL2

        jmp _flashCursorLoop

_exitCursorLoop
        pha                             ; store joystick/keyboard event
        lda charAtCursor                ; current character at cursor
        jsr replaceTileBitmap0          ; (restore original character at cursor position)
        pla                             ; restore joystick/keyboard event
        rts

detectUserInput
        lda Cia1PortA                   ; read joystick port 2
        and #$0f                        ; mask: direction bits
        eor #$0f                        ; invert active (positive logic)
        bne _detectedUserInputEvent     ; direction detected ->
        lda Cia1PortA                   ; read joystick port 2
        and #$10                        ; fire button pressed?
        beq _detectedUserInputEvent     ; yes ->
        lda keyboardCode                ; keyboard matrix code (no key: 0)
        bne _detectedUserInputEvent

        clc                             ; no joystick or keyboard event detected
        rts

_detectedUserInputEvent
        sec                             ; joystick or keyboard event detected
        rts

replaceTileBitmap0
        sta zpShapeId                   ; current shape Id (before conversion)
        lda #>Bitmap0                   ; print to Bitmap0
        bne printTileJ1

replaceTileBitmap1
        sta zpShapeId                   ; current shape Id (before conversion)
        lda #>Bitmap1                   ; print to Bitmap1
printTileJ1
        sta zpBitmapPage                ; destination bitmap page
        ldy zpCursorRow
        ldx zpCursorCol
        jsr convertBoardPosToPixelPos   ; return: (X,Y) pixel coordinates
        sty zpPixelPosY                 ; pixel position Y
        jsr convertShapeOrPrintSprite
        jsr getShapeBitmapOffsetX
        stx zpShapeDblPixelShift        ; double-pixel shift applied to shape for printing
        lda andMaskShiftTblLeft,x
        sta zpBmpAndMask+0              ; and-mask for drawing to bitmap (left)
        lda andMaskShiftTblRight,x
        sta zpBmpAndMask+1              ; and-mask for drawing to bitmap (right)
        jsr copyShapeToHopper           ; copy/shift the shape to zpShiftedShapeHopper
        lda #SHAPE_HEIGHT
        sta zpShapeRowIter              ; iterator over rows in shape
        ldx #$00                        ; index into shifted shape hopper
        lda zpShapeDblPixelShift        ; double-pixel shift applied to shape for printing
        cmp #$05                        ; pixel shift >= 5? (Note: pixel shift always in range [0..3]
        bcs _copyShapeToBitmapL2        ; yes -> (Optimization: this branch is never taken)
_copyShapeToBitmapL1
        ldy zpPixelPosY
        jsr getBitmapPtr                ; get bitmap pointer for pos X,Y on zpBitmapPage
        ldy #$00
        lda (zpBmpPtr),y                ; byte in current bitmap
        and zpBmpAndMask+0              ; and-mask for drawing to bitmap (left)
        ora zpShiftedShapeHopper,x      ; insert left half of shape to bitmap
        sta (zpBmpPtr),y                ; update bitmap
        inx                             ; proceed to right byte
        ldy #$08                        ; increase bitmap pointer to next byte on same line
        lda (zpBmpPtr),y                ; byte in current bitmap
        and zpBmpAndMask+1              ; and-mask for drawing to bitmap (right)
        ora zpShiftedShapeHopper,x      ; insert right half of shape to bitmap
        sta (zpBmpPtr),y                ; update bitmap
        inx                             ; proceed to byte three (unused)
        inx                             ; proceed to byte one on next line
        inc zpPixelPosY                 ; inc destination line
        dec zpShapeRowIter              ; dec shape row iterator
        bne _copyShapeToBitmapL1
        rts

;============== begin unreachable code ==============
;= use case: shifted shape spans three bitmap bytes =

_copyShapeToBitmapL2
        ldy zpPixelPosY
        jsr getBitmapPtr                ; get bitmap pointer for pos X,Y on zpBitmapPage
        ldy #$00
        lda (zpBmpPtr),y                ; byte in current bitmap
        and zpBmpAndMask+0              ; and-mask for drawing to bitmap (left)
        ora zpShiftedShapeHopper,x      ; insert left part of shape to bitmap
        sta (zpBmpPtr),y
        inx                             ; proceed to middle byte
        ldy #$08
        lda zpShiftedShapeHopper,x      ; insert middle part of shape to bitmap
        sta (zpBmpPtr),y                ; directly copy, all bits are new
        inx                             ; proceed to right byte
        ldy #$10
        lda (zpBmpPtr),y                ; byte in current bitmap
        and zpBmpAndMask+1              ; and-mask for drawing to bitmap (right)
        ora zpShiftedShapeHopper,x      ; insert right part of shape to bitmap
        sta (zpBmpPtr),y
        inx
        inc zpPixelPosY
        dec zpShapeRowIter              ; dec shape row iterator
        bne _copyShapeToBitmapL2
        rts
;=============== end unreachable code ==============

andMaskShiftTblLeft
        .byte %00000000
        .byte %11000000
        .byte %11110000
        .byte %11111100

andMaskShiftTblRight
        .byte %00111111
        .byte %00001111
        .byte %00000011
        .byte %00000000

cookieCutTileFromBuffer
        sty zpPixelPosY                 ; pixel position Y
        sta zpShapeId                   ; current shape ID
        jsr getShapeBitmapOffsetX       ; calculate horizontal pixel position
        sta zpPixelOffsetXlb            ; pixel offset x in bitmap, low byte (unused?)
        stx zpShapeDblPixelShift        ; double-pixel shift applied to shape for printing
        jsr copyShapeToHopper           ; copy/shift the shape to zpShiftedShapeHopper
        ldx #SHAPE_HEIGHT               ; iterate over shape height
        stx zpShapeRowIter              ; iterator over rows in shape
        ldx #$00                        ; index into shifted shape hopper
        lda zpShapeDblPixelShift        ; double-pixel shift applied to shape for printing
        cmp #$05                        ; pixel shift >= 5? (Note: pixel shift always in range [0..3]
        bcs _copyShapeToBitmapL2

        ; handle pixel offsets 0-4
_copyShapeToBitmapL1
        ldy zpPixelPosY
        jsr getBitmapPtrsBoth           ; get both bitmap pointers for pos X,Y
        ldy #$00
        lda zpShiftedShapeHopper,x
        eor #$ff                        ; invert shape on hopper
        and (zpBmpPtr0),y               ; cut out shape from displayed bitmap
        ora (zpBmpPtr1),y               ; add in shape from preparation bitmap
        sta (zpBmpPtr0),y               ; store back in displayed bitmap
        inx                             ; handle second byte in sprite row
        ldy #$08
        lda zpShiftedShapeHopper,x
        eor #$ff
        and (zpBmpPtr0),y
        ora (zpBmpPtr1),y
        sta (zpBmpPtr0),y
        inx                             ; third byte in sprite row
        inx                             ; (skip)
        inc zpPixelPosY                 ; inc destination line
        dec zpShapeRowIter              ; dec shape row iterator
        bne _copyShapeToBitmapL1
        rts

;============== begin unreachable code ==============
;= use case: shifted shape spans three bitmap bytes =

_copyShapeToBitmapL2
        ldy zpPixelPosY
        jsr getBitmapPtrsBoth           ; get both bitmap pointers for pos X,Y
        ldy #$00
        lda zpShiftedShapeHopper,x
        eor #$ff
        and (zpBmpPtr0),y
        ora (zpBmpPtr1),y
        sta (zpBmpPtr0),y
        inx
        ldy #$08                        ; increase bitmap pointer to next byte on same line
        lda zpShiftedShapeHopper,x
        eor #$ff
        and (zpBmpPtr0),y
        ora (zpBmpPtr1),y
        sta (zpBmpPtr0),y
        inx
        ldy #$10                        ; increase bitmap pointer to next byte on same line
        lda zpShiftedShapeHopper,x
        eor #$ff
        and (zpBmpPtr0),y
        ora (zpBmpPtr1),y
        sta (zpBmpPtr0),y
        inx
        inc zpPixelPosY                 ; inc destination line
        dec zpShapeRowIter              ; dec shape row iterator
        bne _copyShapeToBitmapL2
        rts
;=============== end unreachable code ==============

pasteTileBitmap0                        ; paste tile over bitmap 0 (no erase)
; print shape at pixel position (?)
; load shape into sprite data
        sty zpPixelPosY                 ; pixel position Y
        sta zpShapeId                   ; current shape ID
        jsr convertShapeOrPrintSprite
        jsr getShapeBitmapOffsetX
        sta zpPixelOffsetXlb            ; pixel offset x in bitmap, low byte (unused?)
        stx zpShapeDblPixelShift        ; double-pixel shift applied to shape for printing
        jsr copyShapeToHopper           ; copy/shift the shape to zpShiftedShapeHopper
        lda #SHAPE_HEIGHT
        sta zpShapeRowIter              ; iterator over rows in shape
        ldx #$00                        ; index into shifted shape hopper
        stx zpPlayerEnemyCollision      ; 0: no collision of player and enemy; 1: collision detected
        lda zpShapeDblPixelShift        ; double-pixel shift applied to shape for printing
        cmp #$05                        ; pixel shift >= 5? (Note: pixel shift always in range [0..3]
        bcs _copyShapeToBitmapL2
_copyShapeToBitmapL1
        ldy zpPixelPosY
        jsr getBitmapPtrsBoth           ; get both bitmap pointers for pos X,Y
        ldy #$00
        lda zpShiftedShapeHopper,x      ; paste left half of shape onto bitmap
        ora (zpBmpPtr0),y
        sta (zpBmpPtr0),y
        inx                             ; proceed to right byte
        ldy #$08                        ; increase bitmap pointer to next byte on same line
        lda zpShiftedShapeHopper,x      ; paste right half of shape onto bitmap
        ora (zpBmpPtr0),y
        sta (zpBmpPtr0),y
        inx                             ; proceed to byte three (unused)
        inx                             ; proceed to byte one on next line
        inc zpPixelPosY                 ; inc destination line
        dec zpShapeRowIter              ; dec shape row iterator
        bne _copyShapeToBitmapL1
        rts

;============== begin unreachable code ==============
;= use case: shifted shape spans three bitmap bytes =

_copyShapeToBitmapL2
        ldy zpPixelPosY
        jsr getBitmapPtrsBoth           ; get both bitmap pointers for pos X,Y
        ldy #$00
        lda zpShiftedShapeHopper,x
        ora (zpBmpPtr0),y
        sta (zpBmpPtr0),y
        inx
        ldy #$08                        ; increase bitmap pointer to next byte on same line
        lda zpShiftedShapeHopper,x
        ora (zpBmpPtr0),y
        sta (zpBmpPtr0),y
        inx
        ldy #$10                        ; increase bitmap pointer to next byte on same line
        lda zpShiftedShapeHopper,x
        ora (zpBmpPtr0),y
        sta (zpBmpPtr0),y
        inx
        inc zpPixelPosY                 ; inc destination line
        dec zpShapeRowIter              ; dec shape row iterator
        bne _copyShapeToBitmapL2
        rts
;=============== end unreachable code ==============

copyShapeToHopper                       ; copy/shift the shape to zpShiftedShapeHopper
        lda #SHAPE_HEIGHT
        sta zpShapeRowIter              ; iterator over rows in shape
        lda #<Shapes
        sta zpWideShapePtr+0
ldaShapes
        lda #>Shapes                    ; high byte of address modified by copy protection check
        sta zpWideShapePtr+1
        ldy tabShapeShiftTablePage,x    ; x: # of double pixels to shift
        sty _ldaShiftTableLeft1+2       ; Self-modifying code (selects shapeShiftTable for shift)
        sty _ldaShiftTableLeft2+2       ; Self-modifying code
        iny                             ; inc. shape shift table page (left byte -> right byte)
        sty _ldaShiftTableRight1+2      ; Self-modifying code
        sty _ldaShiftTableRight2+2      ; Self-modifying code
        ldx #$00
_copyShapeToHopperL
        ldy zpShapeId                   ; shape to print
        lda (zpWideShapePtr),y          ; get shape byte (left)
        tay
_ldaShiftTableLeft1
        lda shapeShiftTables,y          ; shifted value, lb
        sta zpShiftedShapeHopper+0,x    ; 33 byte of shifted shape data
_ldaShiftTableRight1
        lda shapeShiftTables,y          ; shifted value, hb
        sta zpShiftedShapeHopper+1,x
        lda zpWideShapePtr+0
        clc
        adc #NUM_SHAPES                 ; number of distinct shapes
        sta zpWideShapePtr+0
        lda zpWideShapePtr+1
        adc #$00
        sta zpWideShapePtr+1
        ldy zpShapeId                   ; shape to print
        lda (zpWideShapePtr),y          ; get shape byte (right)
        tay
_ldaShiftTableLeft2
        lda shapeShiftTables,y
        ora zpShiftedShapeHopper+1,x
        sta zpShiftedShapeHopper+1,x
_ldaShiftTableRight2
        lda shapeShiftTables,y
        sta zpShiftedShapeHopper+2,x
        lda zpWideShapePtr+0            ; increase ptr to next byte in shape
        clc
        adc #NUM_SHAPES                 ; number of distinct shapes
        sta zpWideShapePtr+0
        lda zpWideShapePtr+1
        adc #$00
        sta zpWideShapePtr+1
        inx
        inx
        inx
        dec zpShapeRowIter              ; dec shape row iterator
        bne _copyShapeToHopperL
        rts

tabShapeShiftTablePage
        .byte >(shapeShiftTables+$0000) ; shift 0 pixels
        .byte >(shapeShiftTables+$0200) ; shift 2 pixels
        .byte >(shapeShiftTables+$0400) ; shift 4 pixels
        .byte >(shapeShiftTables+$0600) ; shift 6 pixels

                                        ; convert charset to shape id
convertShapeOrPrintSprite
        lda skipShapeConversion         ; 0: don't skip shape conversion, >0: skip shape conversion
        bne _exit                       ; skip shape conversion? ->
        lda zpShapeId                   ; current shape ID (before conversion)
        cmp #ANIM_RESPAWN_0
        bcs _exit
        tay                             ; (zpShapeId)
        lda logicalToShapeConvTbl,y     ; convert logical shape to shape
        bmi _exit                       ; table entry negative? leave shape untouched
        beq _printSprite                ; table entry 0 (player shape) -> don't change shape
        sta zpShapeId                   ; current shape ID (enemy shape after conversion)
_printSprite
        stx zpPixelOffsetXlb            ; pixel position X
        jsr setupSprite                 ; copy shape from zpShiftedShapeHopper to sprite definition memory
                                        ; side effect: X = (2 * sprite number)
        lda zpPixelOffsetXlb            ; pixel position X
        clc
        adc #$0c                        ; x-offset for sprites: 24/2
        asl                             ; x-pos = 2 * A, MSB into carry
        and #$f8
        sta VicSprite0XPos,x            ; set sprite x position
        bcc _clearXPosMsb               ; x pos < 256 ->
_setXPosMsb
        lda XPosMsbOraMask,x
        ora VicSpritesXPosMsb           ; set the MSB bit for sprite X/2
        sta VicSpritesXPosMsb
        jmp _setSpriteYPos
_clearXPosMsb
        lda XPosMsbAndMask,x
        and VicSpritesXPosMsb           ; clear the MSB bit for sprite X/2
        sta VicSpritesXPosMsb

_setSpriteYPos
        lda zpPixelPosY                 ; pixel position Y
        clc
        adc #$32                        ; y-offset for sprites: 50
        sta VicSprite0YPos,x            ; set sprite y position
        lda VicSpriteToSpriteCol        ; read sprite-to-sprite collision registers
        and #$01                        ; restrict to collisions with player involved
        sta zpPlayerEnemyCollision      ; 0: no collision of player and enemy; 1: collision detected
        pla
        pla                             ; bail out of original print shape routine (use sprite instead)
_exit
        rts

XPosMsbOraMask
        XPosMsbAndMask = XPosMsbOraMask + 1

        .byte $01,$fe                   ; SET and CLEAR masks for Sprite XPosMsb
        .byte $02,$fd
        .byte $04,$fb
        .byte $08,$f7
        .byte $10,$ef
        .byte $20,$df
        .byte $40,$bf
        .byte $80,$7f

logicalToShapeConvTbl
; table: internal character set to shapes (???)
        .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff   ; $00
        .byte $0b,$00,$ff,$00,$00,$00,$00,$00   ; player shapes
        .byte $00,$00,$00,$00,$00,$00,$00,$00   ; $10
        .byte $00,$00,$00,$ff,$ff,$ff,$ff,$ff
        .byte $ff,$ff,$ff,$ff,$ff,$00,$ff,$ff   ; $20
        .byte $09,$10,$11,$0c,$0d,$15,$16,$17   ; enemy shape translations to player shapes
        .byte $18,$19,$1a,$0e,$12,$14,$13,$ff   ; $30
        .byte $ff

setupSprite
        pha                             ; store shape id
        lda zpPixelOffsetXlb
        and #$03
        tax
        jsr copyShapeToHopper
        pla                             ; restore shape id
        beq _copyShapeToSpriteBuffer    ; player sprite? ->
        lda enemyIndex                  ; current enemy, index
_copyShapeToSpriteBuffer
        tax                             ; hardware sprite ID
        lda tblSpritePtrLb,x            ; sprite pointer lb for sprite x
        sta _writeSpriteDst+1           ; self-modifying write address
        lda tblSpritePtrHb,x            ; sprite pointer hb for sprite x
        sta _writeSpriteDst+2           ; self-modifying write address
        lda tblSpriteNumber,x
        tax

        ldy #$20                        ; index: copy 33 bytes
_copySpriteL
        lda @w zpShiftedShapeHopper,y
_writeSpriteDst
        sta _writeSpriteDst,y           ; copy bytes to sprite buffer (self modified)
        dey
        bpl _copySpriteL

        txa                             ; sprite number
        asl                             ; as 16 bit offset
        tax
        rts

        ; sprite pointer locations
tblSpritePtrLb
        .byte $00,$40,$80,$c0,$00,$40   ; (sprite pointer low byte)

tblSpritePtrHb
        .byte $0c,$0c,$0c,$0c,$0d,$0d   ; (sprite pointer high byte)

tblSpriteNumber
        .byte $00,$02,$03,$04,$06,$07

convertBoardPosToPixelPos
        lda pixelPosByBoardRow,y
        pha
        lda pixelPosByBoardCol,x
        tax                             ; X: pixel pos x
        pla                             ; A: pixel pos y
        tay
        rts

pixelPosByBoardRow                      ; y-positions in pixel rows
        .byte $00 * SHAPE_HEIGHT
        .byte $01 * SHAPE_HEIGHT
        .byte $02 * SHAPE_HEIGHT
        .byte $03 * SHAPE_HEIGHT
        .byte $04 * SHAPE_HEIGHT
        .byte $05 * SHAPE_HEIGHT
        .byte $06 * SHAPE_HEIGHT
        .byte $07 * SHAPE_HEIGHT
        .byte $08 * SHAPE_HEIGHT
        .byte $09 * SHAPE_HEIGHT
        .byte $0a * SHAPE_HEIGHT
        .byte $0b * SHAPE_HEIGHT
        .byte $0c * SHAPE_HEIGHT
        .byte $0d * SHAPE_HEIGHT
        .byte $0e * SHAPE_HEIGHT
        .byte $0f * SHAPE_HEIGHT
        .byte $b5                       ; *not* $10 * SHAPE_HEIGHT

pixelPosByBoardCol                      ; x-positions in double pixels
        .byte $0a + $00 * SHAPE_WIDTH / 2
        .byte $0a + $01 * SHAPE_WIDTH / 2
        .byte $0a + $02 * SHAPE_WIDTH / 2
        .byte $0a + $03 * SHAPE_WIDTH / 2
        .byte $0a + $04 * SHAPE_WIDTH / 2
        .byte $0a + $05 * SHAPE_WIDTH / 2
        .byte $0a + $06 * SHAPE_WIDTH / 2
        .byte $0a + $07 * SHAPE_WIDTH / 2
        .byte $0a + $08 * SHAPE_WIDTH / 2
        .byte $0a + $09 * SHAPE_WIDTH / 2
        .byte $0a + $0a * SHAPE_WIDTH / 2
        .byte $0a + $0b * SHAPE_WIDTH / 2
        .byte $0a + $0c * SHAPE_WIDTH / 2
        .byte $0a + $0d * SHAPE_WIDTH / 2
        .byte $0a + $0e * SHAPE_WIDTH / 2
        .byte $0a + $0f * SHAPE_WIDTH / 2
        .byte $0a + $10 * SHAPE_WIDTH / 2
        .byte $0a + $11 * SHAPE_WIDTH / 2
        .byte $0a + $12 * SHAPE_WIDTH / 2
        .byte $0a + $13 * SHAPE_WIDTH / 2
        .byte $0a + $14 * SHAPE_WIDTH / 2
        .byte $0a + $15 * SHAPE_WIDTH / 2
        .byte $0a + $16 * SHAPE_WIDTH / 2
        .byte $0a + $17 * SHAPE_WIDTH / 2
        .byte $0a + $18 * SHAPE_WIDTH / 2
        .byte $0a + $19 * SHAPE_WIDTH / 2
        .byte $0a + $1a * SHAPE_WIDTH / 2
        .byte $0a + $1b * SHAPE_WIDTH / 2


calcPixelPosX                           ; calculate pixel position (X)
        tya                             ; store stepX
        pha
        jsr convertBoardPosToPixelPos   ; (X,Y) (board) -> (X,Y) (pixels)
        pla                             ; restore stepX
        tay
        txa                             ; X position (pixels)
        clc
        adc _xPixelOffsetFine,y         ; add X position (pixels fine)
        tax
        rts

_xPixelOffsetFine
        .byte $fe,$ff,$00,$01,$02

calcPixelPosY                           ; calculate pixel position (Y)
        txa                             ; store stepY
        pha
        jsr convertBoardPosToPixelPos   ; (X,Y) (board) -> (X,Y) (pixels)
        pla                             ; restore stepY
        tax
        tya                             ; Y position (pixels)
        clc
        adc _yPixelOffsetFine,x         ; add Y position (pixels fine)
        tay
        rts

_yPixelOffsetFine
        .byte $fb,$fd,$00,$02,$04

; unused subroutine

unusedReadValuesFromTable
        lda _tab_lb,x
        pha
        lda _tab_hb,x
        tax
        pla
        rts

_tab_lb
        .byte $02,$03,$05,$06,$07,$08,$0a,$0b
        .byte $0c,$0d,$0f,$10,$11,$12,$14,$15
        .byte $16,$17,$19,$1a,$1b,$1c,$1e,$1f
        .byte $20,$21,$23,$24,$25,$26

_tab_hb
        .byte $02,$03,$00,$01,$02,$03,$00,$01
        .byte $02,$03,$00,$01,$02,$03,$00,$01
        .byte $02,$03,$00,$01,$02,$03,$00,$01
        .byte $02,$03,$00,$01,$02,$03


getShapeBitmapOffsetX
        lda #$00
        sta bitmapOffsetX+1             ; horizontal bitmap offset of shape (hb)
        txa                             ; pixel x coordinate of shape (double pixels)
        pha
        and #$03                        ; modulo-4 offset
        tax
        pla
        asl                             ; convert to single pixel offset
        rol bitmapOffsetX+1             ; horizontal bitmap offset of shape (hb)
        and #$f8
        sta bitmapOffsetX+0             ; horizontal bitmap offset of shape (lb)
        rts

getLayoutAndActionBoardPtrs
        ; both action board and layout board are page aligned.
        ; Corresponding locations have the same pointer low byte

        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0          ; uses same lb (page alignment)
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        rts

getBitmapPtrLine                        ; get bitmap pointer for line Y on zpBitmapPage
        lda #$00
        sta bitmapOffsetX+0             ; horizontal bitmap offset of shape (lb)
        sta bitmapOffsetX+1             ; horizontal bitmap offset of shape (hb)


getBitmapPtr                            ; get bitmap pointer for pos X,Y on zpBitmapPage
        lda TblBmpLinePtrLb,y
        clc
        adc bitmapOffsetX+0             ; horizontal bitmap offset of shape (lb)
        sta zpBmpPtr+0
        lda TblBmpLinePtrHb,y
        adc bitmapOffsetX+1             ; horizontal bitmap offset of shape (hb)
        ora zpBitmapPage                ; destination bitmap page ($20 or $40)
        sta zpBmpPtr+1
        rts

getBitmapPtrsCircle                     ; calc bitmap pointers for screen col X, raster line Y
        lda #$00                        ; init bitmap offset hb
        sta bitmapOffsetX+1             ; horizontal bitmap offset hb
        txa
        asl
        asl                             ; offset = column * 8
        asl
        rol bitmapOffsetX+1             ; carry into bitmap offset hb
        sta bitmapOffsetX+0             ; horizontal bitmap offset

getBitmapPtrsBoth                       ; get both bitmap pointers for pos X,Y
        lda TblBmpLinePtrLb,y
        clc
        adc bitmapOffsetX+0             ; horizontal bitmap offset of shape (lb)
        sta zpBmpPtr0+0                 ; bitmap pointer 0 (lb)
        sta zpBmpPtr1+0                 ; bitmap pointer 1 (lb)
        lda TblBmpLinePtrHb,y
        adc bitmapOffsetX+1             ; horizontal bitmap offset of shape (hb)
        ora #>Bitmap0
        sta zpBmpPtr0+1                 ; bitmap pointer 0 (hb)
        eor #>(Bitmap0^Bitmap1)
        sta zpBmpPtr1+1                 ; bitmap pointer 1 (hb)
        rts

clearBitmap0                            ; clear bitmap 0 and turn off sprites
        lda #>Bitmap0                   ; start page
        ldx #$00
        stx VicSpriteEnable             ; disable sprites
        ldx #>(Bitmap0+$2000)           ; end page
        bne clearMemory                 ; clear memory $2000-$3fff

clearBitmap1
        lda #>Bitmap1                   ; start page clear memory $4000-$5fff
        ldx #>(Bitmap1+$2000)           ; end page

clearMemory
        sta zpMemPtr+1                  ; A: start page
        ldy #$00
        sty zpMemPtr+0
        tya                             ; A = $00 (fill value)
_clearMemoryLoop
        sta (zpMemPtr),y
        iny
        bne _clearMemoryLoop
        inc zpMemPtr+1
        cpx zpMemPtr+1                  ; X: end page
        bne _clearMemoryLoop
        rts

boardLayoutOffsetLb
boardActionOffsetLb
        ; note: there is an unusual offset between row 8 and row 9.
        ; Reason: Line 9 starts at offset 0 of a fresh page.
        ; This doesn't seem to be necessarily so, it would be more
        ; natural just to continue with offset = <line> * BOARD_WIDTH
        .byte $0 * BOARD_WIDTH
        .byte $1 * BOARD_WIDTH
        .byte $2 * BOARD_WIDTH
        .byte $3 * BOARD_WIDTH
        .byte $4 * BOARD_WIDTH
        .byte $5 * BOARD_WIDTH
        .byte $6 * BOARD_WIDTH
        .byte $7 * BOARD_WIDTH
        .byte $8 * BOARD_WIDTH
        .byte $0 * BOARD_WIDTH   ; offset 0 on new page
        .byte $1 * BOARD_WIDTH
        .byte $2 * BOARD_WIDTH
        .byte $3 * BOARD_WIDTH
        .byte $4 * BOARD_WIDTH
        .byte $5 * BOARD_WIDTH
        .byte $6 * BOARD_WIDTH


boardActionOffsetHb
        .byte >boardAction+$0000
        .byte >boardAction+$0000
        .byte >boardAction+$0000
        .byte >boardAction+$0000
        .byte >boardAction+$0000
        .byte >boardAction+$0000
        .byte >boardAction+$0000
        .byte >boardAction+$0000
        .byte >boardAction+$0000
        .byte >boardAction+$0100
        .byte >boardAction+$0100
        .byte >boardAction+$0100
        .byte >boardAction+$0100
        .byte >boardAction+$0100
        .byte >boardAction+$0100
        .byte >boardAction+$0100

boardLayoutOffsetHb
        .byte >boardLayout+$0000
        .byte >boardLayout+$0000
        .byte >boardLayout+$0000
        .byte >boardLayout+$0000
        .byte >boardLayout+$0000
        .byte >boardLayout+$0000
        .byte >boardLayout+$0000
        .byte >boardLayout+$0000
        .byte >boardLayout+$0000
        .byte >boardLayout+$0100
        .byte >boardLayout+$0100
        .byte >boardLayout+$0100
        .byte >boardLayout+$0100
        .byte >boardLayout+$0100
        .byte >boardLayout+$0100
        .byte >boardLayout+$0100


activateExitLadders
        lda #$00
        sta notAllExitLaddersDrawn
        ldx numExitLadders              ; number of exit ladders
        stx exitLadderIdx
_activateExitLaddersLoop
        ldx exitLadderIdx               ; all exit ladders handled?
        beq _updateLevelExitConditions  ; yes ->
        lda exitLaddersX,x              ; exit ladder, x position
        bmi _continue                   ; already shown ->
        sta zpCursorCol
        lda exitLaddersY,x              ; exit ladder, y position
        sta zpCursorRow
        tay
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        ldy zpCursorCol
        lda (zpBoardLayoutPtr),y        ; check if position is empty in layout board
        bne _targetTileBusy             ; not empty ->
        lda #SHAPE_LADDER               ; activate the exit ladder
        sta (zpBoardLayoutPtr),y        ; store ladder in board layout
        lda (zpBoardActionPtr),y        ; check if position is empty in action board
        bne _printExitLadder            ; not empty ->
        lda #SHAPE_LADDER
        sta (zpBoardActionPtr),y        ; store ladder in board action
_printExitLadder
        lda #SHAPE_LADDER
        jsr replaceTileBitmap1
        ldx zpCursorCol
        ldy zpCursorRow
        jsr convertBoardPosToPixelPos
        lda #SHAPE_LADDER
        jsr pasteTileBitmap0            ; paste tile over bitmap 0 (no erase)
        ldx exitLadderIdx
        lda #$ff                        ; mark exit ladder as drawn
        sta exitLaddersX,x
        bmi _continue
_targetTileBusy
        lda #$01
        sta notAllExitLaddersDrawn

_continue
        dec exitLadderIdx               ; iterate over exit ladders
        jmp _activateExitLaddersLoop    ; continue with next exit ladder

_updateLevelExitConditions
        lda notAllExitLaddersDrawn      ; all exit ladders drawn?
        bne _exit                       ; no -> retry in next main loop iteration
        dec goldLeft                    ; set gold left to $ff: ladders have been drawn
_exit
        rts

exitLadderIdx
        .byte $00


waitForKey
        lda keyboardCode                ; keyboard matrix code (no key: 0)
        beq waitForKey
        ldx #$00                        ; no key pressed (strobe keyboard)
        stx keyboardCode                ; keyboard matrix code (no key: 0)
        rts


areaCopyProtection
.if FULL_COPY_PROTECTION == true
runCopyProtection
.endif
.enc "none"
l8e11   ldx #'0'                        ; track: 02
        ldy #'2'
        lda #$00                        ; protection byte offset
        jsr testCopyProtTrack           ; test track 2 (error code 5)
        beq abortRunCopyProtection      ; no error detected ->

        ldx #'0'                        ; track: 01
        ldy #'1'
        lda #$01                        ; protection byte offset
        jsr testCopyProtTrack           ; test track 1 (no error)
        bne abortRunCopyProtection      ; error detected ->

        ldx #'3'                        ; track: 34
        ldy #'4'
        lda #$02                        ; protection byte offset
        jsr testCopyProtTrack           ; test track 34 (error code 9)
        beq abortRunCopyProtection

.if FULL_COPY_PROTECTION == false
runCopyProtection
.endif

        ldx #'3'                        ; track: 35
        ldy #'5'
        lda #$03                        ; protection byte offset
        jsr testCopyProtTrack           ; test track 35 (no error)
abortRunCopyProtection
        rts

copyProtCheckBytes
.if FULL_COPY_PROTECTION == true
        .byte $00,$00,$00,$00
        .byte $00,$00,$00,$00
.else
        .byte $32,$30,$32,$30
        .byte $33,$30,$37,$30
.endif


testCopyProtTrack
        sta zpCopyProtByteOffset        ; copy protection check byte offset
        stx trackCopyProt+0             ; track 1st digit
        sty trackCopyProt+1             ; track 2nd digit
        lda #$00
        jsr SETNAM                      ; setnam ""
        lda #$0f
        ldx #$08
        tay
        jsr SETLFS                      ; setlfs 15,8,15
        jsr OPEN                        ; open 15,8,15,""
        lda #$01
        ldx #<fnCopyProtDataChannel
        ldy #>fnCopyProtDataChannel
        jsr SETNAM                      ; setnam "#"
        lda #$05
        ldx #$08
        tay
        jsr SETLFS                      ; setlfs 5,8,5
        jsr OPEN                        ; open 5,8,5,"#"
        jsr CLRCHN                      ; clrchn
        ldx #$0f
        jsr CHKOUT                      ; chkout 15
        ldy #$00
_sendU1CommandL
        lda strCopyProtU1Command,y      ; "U1: 5 0 01 2",$0d,$00
        beq _copyProtJ1
        jsr CHROUT                      ; output command byte to 1541
        iny
        bne _sendU1CommandL

_copyProtJ1
        jsr CLRCHN                      ; clrchn
        ldx #$0f
        jsr CHKIN                       ; chkin 15
        jsr CHRIN                       ; read byte from 1541
        ldy zpCopyProtByteOffset        ; copy protection check byte offset
        sta copyProtCheckBytes+0,y      ; (first byte of error code)
        jsr CHRIN                       ; read byte from 1541
        sta copyProtCheckBytes+4,y      ; (second byte of error code)

_readFloppyStatusL
        jsr CHRIN                       ; read byte from 1541
        cmp #$0d                        ; read/skip remaining status bytes
        bne _readFloppyStatusL

        jsr CLALL                       ; clall
        lda #<areaCopyProtection       ; set low byte (copy protection integrity check)
        sta checkAdcIntegrityPtr+1      ; self-modifying code (set checked data ptr lb)
        ldy zpCopyProtByteOffset        ; copy protection check byte offset
        lda copyProtCheckBytes+0,y      ; (first byte of error code)
        ora copyProtCheckBytes+4,y      ; (second byte of error code)
        cmp #$00                        ; copy protection check successful?
        rts

fnCopyProtDataChannel
        .text "#"

strCopyProtU1Command
        .text "U1: 5 0 "                ; "U1: 5 0 01 2",$0d,$00
trackCopyProt
        .text "01 2",$0d,$00


displayGameOver
        lda #$01                        ; initial frame delay (minimum)
        sta spinGameOverDelay           ; frame delay when spinning the "GAME OVER" display
        lda #>Bitmap0
        sta zpBitmapPage                ; destination bitmap page
_spinGameOverPhase1L
        jsr _frame5
        jsr _frame4
        jsr _frame3                     ; spin through all phases
        jsr _frame2                     ; for a full rotation of the
        jsr _frame1                     ; GAME OVER logo
        jsr _frame0
        jsr _frame1
        jsr _frame2
        jsr _frame3
        jsr _frame4
        jsr _frame5
        jsr _frame10
        jsr _frame9
        jsr _frame8
        jsr _frame7
        jsr _frame6
        jsr _frame7
        jsr _frame8
        jsr _frame9
        jsr _frame10
        lda spinGameOverDelay           ; frame delay when spinning the "GAME OVER" display
        cmp #$64
        bcc _spinGameOverPhase1L

        jsr _frame5
        jsr _frame4
        jsr _frame3                     ; do another half rotation
        jsr _frame2
        jsr _frame1
        jsr _frame0
        clc
        rts

; each frame consists of an array of line ids.
; each line id indexes into an array of bitmap bytes

_frame0
        jsr displayGameOverFrame
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$02,$01,$00   ; (full size)

_frame1
        jsr displayGameOverFrame
        .byte $00,$00,$01,$02,$03,$04,$05,$07,$09,$0a,$02,$01,$00,$00

_frame2
        jsr displayGameOverFrame
        .byte $00,$00,$00,$01,$02,$03,$04,$09,$0a,$02,$01,$00,$00,$00

_frame3
        jsr displayGameOverFrame
        .byte $00,$00,$00,$00,$01,$02,$03,$0a,$02,$01,$00,$00,$00,$00

_frame4
        jsr displayGameOverFrame
        .byte $00,$00,$00,$00,$00,$01,$03,$0a,$01,$00,$00,$00,$00,$00

_frame5
        jsr displayGameOverFrame
        .byte $00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00

_frame6
        jsr displayGameOverFrame
        .byte $00,$01,$02,$0a,$09,$08,$07,$06,$05,$04,$03,$02,$01,$00   ; (upside down)

_frame7
        jsr displayGameOverFrame
        .byte $00,$00,$01,$02,$0a,$09,$07,$05,$04,$03,$02,$01,$00,$00

_frame8
        jsr displayGameOverFrame
        .byte $00,$00,$00,$01,$02,$0a,$09,$04,$03,$02,$01,$00,$00,$00

_frame9
        jsr displayGameOverFrame
        .byte $00,$00,$00,$00,$01,$02,$0a,$03,$02,$01,$00,$00,$00,$00

_frame10
        jsr displayGameOverFrame
        .byte $00,$00,$00,$00,$00,$01,$0a,$03,$01,$00,$00,$00,$00,$00

gameOverLine00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
gameOverLine01
        .byte $0a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$80
gameOverLine02
        .byte $20,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20
gameOverLine03
        .byte $22,$a8,$a8,$aa,$8a,$a0,$2a,$8a,$22,$a8,$aa,$20
gameOverLine04
        .byte $22,$08,$88,$a8,$8a,$00,$22,$8a,$22,$a8,$82,$20
gameOverLine05
        .byte $22,$00,$88,$88,$8a,$00,$22,$8a,$22,$80,$82,$20
gameOverLine06
        .byte $22,$00,$88,$88,$8a,$a0,$22,$8a,$22,$a0,$aa,$20
gameOverLine07
        .byte $22,$28,$a8,$88,$8a,$a0,$20,$8a,$22,$00,$a8,$20
gameOverLine08
        .byte $22,$08,$88,$88,$88,$00,$20,$8a,$a2,$00,$a8,$20
gameOverLine09
        .byte $22,$08,$88,$88,$88,$00,$20,$82,$82,$00,$a2,$20
gameOverLine0a
        .byte $22,$a8,$88,$88,$8a,$a0,$2a,$82,$02,$a8,$a2,$20

; 000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
; 000010101010101010101010101010101010101010101010101010101010101010101010101010101010101010000000
; 001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000
; 001000101010100010101000101010101000101010100000001010101000101000100010101010001010101000100000
; 001000100000100010001000101010001000101000000000001000101000101000100010101010001000001000100000
; 001000100000100010001000101010001000101000000000001000101000101000100010101010001000001000100000
; 001000100000000010001000100010001000101010100000001000101000101000100010101000001010101000100000
; 001000100010100010101000100010001000101010100000001000001000101000100010000000001010100000100000
; 001000100000100010001000100010001000100000000000001000001000101010100010000000001010100000100000
; 001000100000100010001000100010001000100000000000001000001000101010100010000000001010100000100000
; 001000101010100010001000100010001000101010100000001010101000001000000010101010001010001000100000

GameOverLineData
        .word gameOverLine00-$0e
        .word gameOverLine01-$0e
        .word gameOverLine02-$0e
        .word gameOverLine03-$0e
        .word gameOverLine04-$0e
        .word gameOverLine05-$0e
        .word gameOverLine06-$0e
        .word gameOverLine07-$0e
        .word gameOverLine08-$0e
        .word gameOverLine09-$0e
        .word gameOverLine0a-$0e

displayGameOverFrame
        pla                             ; pull return address from stac
        sta zpSrcPtr+0                  ; data pointer (phase of "GAME OVER")
        pla
        sta zpSrcPtr+1

        ldy #80                         ; "GAME OVER" starts at raster line 80 (+1)
        sty zpBitmapLine
        bne _continueNextLine           ; inc zpSrcPtr and zpBitmapLine, then enter loop

_copyGameOverLinesL
        jsr getBitmapPtrLine            ; get bitmap pointer for line Y on zpBitmapPage
        ldy #$00
        lda (zpSrcPtr),y                ; read line ID from frame data pointer
        asl                             ; (as 16 bit index into second table)
        tax
        lda GameOverLineData+0,x        ; pointer to array of bitmap data (lb)
        sta _ldaBitmapByte+1            ; (self modified address) source pointer lb
        lda GameOverLineData+1,x        ; pointer to array of bitmap data (hb)
        sta _ldaBitmapByte+2            ; (self modified address) source pointer hb
        ldy #$70                        ; initial byte offset
        sty zpBmpByteOffset             ; bitmap byte offset (in steps of 8 within same line)
        ldy #$0e                        ; init source data offset
        sty zpSrcCol
_copyGameOverColumnsL
        ldy zpSrcCol
        inc zpSrcCol
_ldaBitmapByte
        lda _ldaBitmapByte,y            ; (self-modified source address) - read bitmap byte
        lsr                             ; adjust bit pattern (%10 -> %01, %00 -> %00)
        ldy zpBmpByteOffset             ; bitmap byte offset (in steps of 8 within same line)
        sta (zpBmpPtr),y                ; store in bitmap destination line + column offset
        tya
        clc
        adc #$08                        ; inc to next byte in bitmap line
        sta zpBmpByteOffset             ; bitmap byte offset (in steps of 8 within same line)
        ldy zpSrcCol
        cpy #$1a                        ; source data offset stop value
        bcc _copyGameOverColumnsL       ; iterate over columns in line

_continueNextLine
        jsr incZpSrcPtr                 ; increment zpSrcPtr

        inc zpBitmapLine
        ldy zpBitmapLine
        cpy #95                         ; last line of "GAME OVER"?
        bcc _copyGameOverLinesL

        ldx spinGameOverDelay           ; frame delay when spinning the "GAME OVER" display
        ldy #$ff
_delayL                                 ; delay spinGameOverDelay * 256 times (busy wait)
        dey
        bne _delayL
        dex
        bne _delayL

        inc spinGameOverDelay           ; inc frame delay when spinning the "GAME OVER" display
        lda Cia1PortA                   ; read joystick port 2
        and #$10                        ; fire button pressed?
        beq _exitGameOverDisplay        ; yes -> exit
        lda keyboardCode                ; keyboard matrix code (no key: 0)
        bne _exitGameOverDisplay
        rts                             ; regular return from displayGameOverFrame

_exitGameOverDisplay
        pla                             ; skip rts (displayGameOverFrame)
        pla
        sec                             ; return from: displayGameOver
        rts

spinGameOverDelay                       ; frame delay when spinning the "GAME OVER" display
        .byte $00

incZpSrcPtr
        inc zpSrcPtr+0
        bne _skipInc
        inc zpSrcPtr+1
_skipInc
        rts


doIrisAnimation
        lda irisPhase                   ; iris animation phase: 0: animate open only, $ff: animate both
        beq _animateIrisOpen
_animateCloseIris
        ldx #IRIS_RADIUS_MAX            ; iris max wide open
        stx zpIrisRadius                ; iris: current radius
        ldx #$00
        stx zpIrisAnimDirection         ; set direction to closing (0)

_animateCloseL
        jsr updateIrisDisplay           ; update the iris display
        dec zpIrisRadius                ; iris: current radius
        bne _animateCloseL

_animateIrisOpen
        lda #IRIS_RADIUS_MIN            ; (1)
        sta zpIrisRadius                ; iris: current radius
        sta zpIrisAnimDirection         ; set direction to opening (1)
        jsr printLives                  ; print number of lives
        jsr printLevel                  ; print current level

_animateOpenL
        jsr updateIrisDisplay           ; update the iris display
        inc zpIrisRadius                ; iris: current radius
        lda zpIrisRadius                ; iris: current radius
        cmp #IRIS_RADIUS_MAX
        bne _animateOpenL

        rts

updateIrisDisplay                       ; draw one ring (circle) of the iris
        lda zpIrisRadius                ; iris: current radius
        sta zpCircleX+0                 ; circle algorithm: x start = radius
        lda #$00
        sta zpCircleX+1
        sta zpCircleY+0                 ; circle algorithm: y start = 0
        sta zpCircleY+1
        lda zpCircleX+0
        asl                             ; calculate diameter (16 bit)
        sta zpCircleA+0
        lda zpCircleX+1
        rol
        sta zpCircleA+1

        lda #$03                        ; diameter as negative number, offset by 3 pixels
        sec
        sbc zpCircleA+0
        sta zpCircleA+0
        lda #$00
        sbc zpCircleA+1
        sta zpCircleA+1

        ; calculate y start positions for octants 0-7

        lda #IRIS_CENTER_Y              ; pixel y position: iris center
        sec
        sbc zpIrisRadius                ; iris: current radius
        sta zpIrisPosYOct34             ; Y position for octants 3/4

        lda #IRIS_CENTER_Y              ; pixel y position: iris center
        sta zpIrisPosYOct78             ; Y position for octants 7/8
        sta zpIrisPosYOct56             ; Y position for octants 5/6

        lda #IRIS_CENTER_Y              ; pixel y position: iris center
        clc
        adc zpIrisRadius                ; iris: current radius
        sta zpIrisPosYOct12             ; Y position for octants 1/2

        ; calculate x start positions for octants 0-7

        lda #IRIS_CENTER_X              ; pixel x position: iris center
        sec
        sbc zpIrisRadius                ; iris: current radius
        tax                             ; (low byte)
        lda #$00                        ; high byte
        sbc #$00
        jsr xPosToColumn
        sty zpIrisColXOct68             ; X position (column) for octants 6/8
        sta zpIrisOffXOct68             ; X position (offset) for octants 6/8

        ldx #IRIS_CENTER_X              ; pixel x position: iris center
        lda #$00                        ; high byte
        jsr xPosToColumn
        sty zpIrisColXOct24             ; X position (column) for octants 2/4
        sty zpIrisColXOct13             ; X position (column) for octants 1/3
        sta zpIrisOffXOct24             ; X position (offset) for octants 2/4
        sta zpIrisOffXOct13             ; X position (offset) for octants 1/3

        lda #IRIS_CENTER_X              ; pixel x position: iris center
        clc
        adc zpIrisRadius                ; iris: current radius
        tax
        lda #$00                        ; high byte
        adc #$00
        jsr xPosToColumn
        sty zpIrisColXOct57             ; X position (column) for octants 5/7
        sta zpIrisOffXOct57             ; X position (offset) for octants 5/7

drawCircleLoop
        ; check the exit condition
        lda zpCircleY+1                 ; circle Y < circle X?
        cmp zpCircleX+1
        bcc _drawCircleJ1               ; yes -> continue drawing circle
        beq _compareLowBytes
_drawCircleFinish
        lda zpCircleY+0                 ; circle Y = circle X? (lb)
        cmp zpCircleX+0
        bne _exit                       ; no ->
        lda zpCircleY+1                 ; circle Y = circle X? (hb)
        cmp zpCircleX+1
        bne _exit                       ; no ->
        jmp irisDrawOctants             ; one more draw: points in-between segments
_exit
        rts

_compareLowBytes
        lda zpCircleY+0                 ; compare circle Y < circle X (lb)
        cmp zpCircleX+0
        bcs _drawCircleFinish           ; circle Y >= circle X -> exit condition 

_drawCircleJ1
        jsr irisDrawOctants             ; update iris for all 8 octants

        lda zpCircleA+1                 ; does slow changing axis change?
        bpl _circleDecrementX           ; yes ->

        lda zpCircleY+0                 ; algorithm: current Y coordinate
        asl                             ; * 2
        sta zpCircleTmpTimes4+0
        lda zpCircleY+1
        rol
        sta zpCircleTmpTimes4+1

        lda zpCircleTmpTimes4+0
        asl                             ; * 4
        sta zpCircleTmpTimes4+0
        lda zpCircleTmpTimes4+1
        rol
        sta zpCircleTmpTimes4+1

        lda zpCircleA+0                 ; algorithm: a = a + x
        clc
        adc zpCircleTmpTimes4+0
        sta zpCircleTmpTimes4+0
        lda zpCircleA+1
        adc zpCircleTmpTimes4+1
        sta zpCircleTmpTimes4+1
        lda #$06                        ; add $0006 for rounding(?)
        clc
        adc zpCircleTmpTimes4+0
        sta zpCircleA+0
        lda #$00
        adc zpCircleTmpTimes4+1
        sta zpCircleA+1
        jmp irisAdvanceCircleStep       ; advance fast changing axis of circle

_circleDecrementX
        lda zpCircleY+0                 ; completed steps on fast changing axis when drawing octants
        sec
        sbc zpCircleX+0
        sta zpCircleTmpTimes4+0
        lda zpCircleY+1                 ; completed steps on fast changing axis when drawing octants
        sbc zpCircleX+1
        sta zpCircleTmpTimes4+1

        lda zpCircleTmpTimes4+0
        asl                             ; * 2
        sta zpCircleTmpTimes4+0
        lda zpCircleTmpTimes4+1
        rol
        sta zpCircleTmpTimes4+1

        lda zpCircleTmpTimes4+0
        asl                             ; * 4
        sta zpCircleTmpTimes4+0
        lda zpCircleTmpTimes4+1
        rol
        sta zpCircleTmpTimes4+1

        lda zpCircleTmpTimes4+0
        clc
        adc #$10                        ; add $0010 for rounding(?)
        sta zpCircleTmpTimes4+0
        lda zpCircleTmpTimes4+1
        adc #$00
        sta zpCircleTmpTimes4+1

        lda zpCircleTmpTimes4+0
        clc
        adc zpCircleA+0
        sta zpCircleA+0
        lda zpCircleTmpTimes4+1
        adc zpCircleA+1
        sta zpCircleA+1

        lda zpCircleX+0                 ; decrement X coordinate in algorithm
        php
        dec zpCircleX+0
        plp
        bne _skipDecXHb
        dec zpCircleX+1
_skipDecXHb
                                        ; advance slow changing axis of circle
        inc zpIrisPosYOct34             ; inc. Y position for octants 3/4
        dec zpIrisOffXOct57             ; dec. X position (offset) for octants 5/7
        bpl _skipDecColX                ; X position (offset) >= 0? ->
        lda #$07                        ; reset offset X to maximum:
        sta zpIrisOffXOct57             ; X position (offset) for octants 5/7
        dec zpIrisColXOct57             ; dec. X position (column) for octants 5/7
_skipDecColX
        inc zpIrisOffXOct68             ; X position (offset) for octants 6/8
        lda zpIrisOffXOct68             ; X position (offset) for octants 6/8
        cmp #$08
        bne _skipIncColX
        lda #$00                        ; reset offset X to minimum:
        sta zpIrisOffXOct68             ; X position (offset) for octants 6/8
        inc zpIrisColXOct68             ; inc. X position (column) for octants 6/8
_skipIncColX
        dec zpIrisPosYOct12             ; dec Y position for octants 1/2


irisAdvanceCircleStep                   ; advance fast changing axis of circle
        inc zpCircleY+0           ; inc. completed steps on fast changing axis
        bne _skipInc
        inc zpCircleY+1           ; (inc. high byte)
_skipInc
        inc zpIrisOffXOct13             ; inc. X position (offset) for octants 1/3
        lda zpIrisOffXOct13             ; inc. X position (offset) for octants 1/3
        cmp #$08                        ; offset == 8?
        bne _advanceStepJ1              ; no ->
        lda #$00                        ; new value for offset: 0
        sta zpIrisOffXOct13             ; reset X position (offset) for octants 1/3
        inc zpIrisColXOct13             ; inc. X position (column) for octants 1/3
_advanceStepJ1
        dec zpIrisPosYOct78             ; dec. Y position for octants 7/8
        inc zpIrisPosYOct56             ; inc. Y position for octants 5/6
        dec zpIrisOffXOct24             ; dec. X position (offset) for octants 2/4
        bpl _advanceStepJ2              ; offset >= 0? ->
        lda #$07                        ; new value for offset: 7
        sta zpIrisOffXOct24             ; reset X position (offset) for octants 2/4
        dec zpIrisColXOct24             ; dec. X position (column) for octants 2/4
_advanceStepJ2
        jmp drawCircleLoop


xPosToColumn
        sta zpIrisXPosHbTemp            ; x pos, high byte
        txa                             ; x pos, low byte
        and #$07                        ; calc x pos offset within column
        pha                             ; store it
        tay                             ; (no effect, y overwritten below)
        txa                             ; x pos, low byte
        ror zpIrisXPosHbTemp
        ror
        ror zpIrisXPosHbTemp            ; divide x pos by 8
        ror                             ; (calculate screen column)
        ror zpIrisXPosHbTemp
        ror
        tay                             ; result: Y = column
        pla                             ; result: A = pixel offset within column
        rts

irisDrawOctants
_drawOctant1
        ldy zpIrisPosYOct12             ; Y position for octants 1/2
        cpy #IRIS_MAX_POS_Y+1           ; > max screen line? (175)
        bcs _drawOctant3                ; yes -> skip octants 1 and 2
        ldx zpIrisColXOct13             ; X position (column) for octants 1/3
        cpx #IRIS_MAX_COL_X+1           ; > max screen column? (39)
        bcs _drawOctant2                ; yes -> skip octant 1
        jsr getBitmapPtrsCircle         ; fetch bitmap pointers for screen col X, raster line Y
        ldx zpIrisOffXOct13             ; X position (offset) for octants 1/3
        ldy #$00
        lda zpIrisAnimDirection         ; 0: closing, 1: opening
        bne _drawOct1Opening            ; opening ->
_drawOct1Closing
        lda (zpBmpPtr0),y               ; get current byte from dest bitmap
        and tabIrisOpenMask,x           ; table: bitmasks for copying pixels
        sta (zpBmpPtr0),y               ; update dest bitmap
        jmp _drawOctant2
_drawOct1Opening
        lda (zpBmpPtr1),y               ; get current byte from buffer bitmap
        and tabIrisCloseMask,x          ; table: bitmasks for clearing pixels
        ora (zpBmpPtr0),y               ; combine with current byte from dest bitmap
        sta (zpBmpPtr0),y               ; update dest bitmap

_drawOctant2
        ldx zpIrisColXOct24             ; X position (column) for octants 2/4
        cpx #IRIS_MAX_COL_X+1           ; X position (column) negative?
        bcs _drawOctant3                ; yes -> skip octant 2
        ldy zpIrisPosYOct12             ; Y position for octants 1/2
        jsr getBitmapPtrsCircle         ; fetch bitmap pointers for screen col X, raster line Y
        ldx zpIrisOffXOct24             ; X position (offset) for octants 2/4
        ldy #$00
        lda zpIrisAnimDirection         ; 0: closing, 1: opening
        bne _drawOct2Opening            ; opening ->
_drawOct2Closing
        lda (zpBmpPtr0),y               ; get current byte from dest bitmap
        and tabIrisOpenMask,x           ; table: bitmasks for copying pixels
        sta (zpBmpPtr0),y               ; update dest bitmap
        jmp _drawOctant3
_drawOct2Opening
        lda (zpBmpPtr1),y               ; get current byte from buffer bitmap
        and tabIrisCloseMask,x          ; table: bitmasks for clearing pixels
        ora (zpBmpPtr0),y               ; combine with current byte from dest bitmap
        sta (zpBmpPtr0),y               ; update dest bitmap

_drawOctant3
        ldy zpIrisPosYOct34             ; Y position for octants 3/4
        cpy #IRIS_MAX_POS_Y+1           ; >= screen height? (176)
        bcs _drawOctant5                ; yes -> skip octants 3 and 4
        ldx zpIrisColXOct13             ; X position (column) for octants 1/3
        cpx #IRIS_MAX_COL_X+1           ; > max screen column? (39)
        bcs _drawOctant4                ; yes -> skip octant 3
        jsr getBitmapPtrsCircle         ; fetch bitmap pointers for screen col X, raster line Y
        ldx zpIrisOffXOct13             ; inc. X position (offset) for octants 1/3
        ldy #$00
        lda zpIrisAnimDirection         ; 0: closing, 1: opening
        bne _drawOct3Opening            ; opening ->
_drawOct3Closing
        lda (zpBmpPtr0),y               ; get current byte from dest bitmap
        and tabIrisOpenMask,x           ; table: bitmasks for copying pixels
        sta (zpBmpPtr0),y               ; update dest bitmap
        jmp _drawOctant4
_drawOct3Opening
        lda (zpBmpPtr1),y               ; get current byte from buffer bitmap
        and tabIrisCloseMask,x          ; table: bitmasks for clearing pixels
        ora (zpBmpPtr0),y               ; combine with current byte from dest bitmap
        sta (zpBmpPtr0),y               ; update dest bitmap

_drawOctant4
        ldx zpIrisColXOct24             ; X position (column) for octants 2/4
        cpx #IRIS_MAX_COL_X+1           ; X position (column) negative?
        bcs _drawOctant5                ; yes -> skip octant 4
        ldy zpIrisPosYOct34             ; Y position for octants 3/4
        jsr getBitmapPtrsCircle         ; fetch bitmap pointers for screen col X, raster line Y
        ldx zpIrisOffXOct24             ; X position (offset) for octants 2/4
        ldy #$00
        lda zpIrisAnimDirection         ; 0: closing, 1: opening
        bne _drawOct4Opening            ; opening ->
_drawOct4Closing
        lda (zpBmpPtr0),y               ; get current byte from dest bitmap
        and tabIrisOpenMask,x           ; table: bitmasks for copying pixels
        sta (zpBmpPtr0),y               ; update dest bitmap
        jmp _drawOctant5
_drawOct4Opening
        lda (zpBmpPtr1),y               ; get current byte from buffer bitmap
        and tabIrisCloseMask,x          ; table: bitmasks for clearing pixels
        ora (zpBmpPtr0),y               ; combine with current byte from dest bitmap
        sta (zpBmpPtr0),y               ; update dest bitmap

_drawOctant5
        ldy zpIrisPosYOct56             ; Y position for octants 5/6
        cpy #IRIS_MAX_POS_Y+1           ; >= screen height? (176)
        bcs _drawOctant7                ; yes -> skip octants 5 and 6
        ldx zpIrisColXOct57             ; X position (column) for octants 5/7
        cpx #IRIS_MAX_COL_X+1           ; > max screen column? (39)
        bcs _drawOctant6                ; yes -> skip octant 5
        jsr getBitmapPtrsCircle         ; fetch bitmap pointers for screen col X, raster line Y
        ldx zpIrisOffXOct57             ; inc. X position (offset) for octants 5/7
        ldy #$00
        lda zpIrisAnimDirection         ; 0: closing, 1: opening
        bne _drawOct5Opening            ; opening ->
_drawOct5Closing
        lda (zpBmpPtr0),y               ; get current byte from dest bitmap
        and tabIrisOpenMask,x           ; table: bitmasks for copying pixels
        sta (zpBmpPtr0),y               ; update dest bitmap
        jmp _drawOctant6
_drawOct5Opening
        lda (zpBmpPtr1),y               ; get current byte from buffer bitmap
        and tabIrisCloseMask,x          ; table: bitmasks for clearing pixels
        ora (zpBmpPtr0),y               ; combine with current byte from dest bitmap
        sta (zpBmpPtr0),y               ; update dest bitmap

_drawOctant6
        ldx zpIrisColXOct68             ; X position (column) for octants 6/8
        cpx #IRIS_MAX_COL_X+1           ; X position (column) negative?
        bcs _drawOctant7                ; yes -> skip octant 6
        ldy zpIrisPosYOct56             ; Y position for octants 5/6
        jsr getBitmapPtrsCircle         ; fetch bitmap pointers for screen col X, raster line Y
        ldx zpIrisOffXOct68             ; X position (offset) for octants 6/8
        ldy #$00
        lda zpIrisAnimDirection         ; 0: closing, 1: opening
        bne _drawOct6Opening            ; opening ->
_drawOct6Closing
        lda (zpBmpPtr0),y               ; get current byte from dest bitmap
        and tabIrisOpenMask,x           ; table: bitmasks for copying pixels
        sta (zpBmpPtr0),y               ; update dest bitmap
        jmp _drawOctant7
_drawOct6Opening
        lda (zpBmpPtr1),y               ; get current byte from buffer bitmap
        and tabIrisCloseMask,x          ; table: bitmasks for clearing pixels
        ora (zpBmpPtr0),y               ; combine with current byte from dest bitmap
        sta (zpBmpPtr0),y               ; update dest bitmap

_drawOctant7
        ldy zpIrisPosYOct78             ; Y position for octants 7/8
        cpy #IRIS_MAX_POS_Y+1
        bcs _exitDrawOctants
        ldx zpIrisColXOct57             ; X position (column) for octants 5/7
        cpx #IRIS_MAX_COL_X+1           ; > max screen column? (39)
        bcs _drawOctant8                ; yes -> skip octant 7
        jsr getBitmapPtrsCircle         ; fetch bitmap pointers for screen col X, raster line Y
        ldx zpIrisOffXOct57             ; inc. X position (offset) for octants 5/7
        ldy #$00
        lda zpIrisAnimDirection         ; 0: closing, 1: opening
        bne _drawOct7Opening            ; opening ->
_drawOct7Closing
        lda (zpBmpPtr0),y               ; get current byte from dest bitmap
        and tabIrisOpenMask,x           ; table: bitmasks for copying pixels
        sta (zpBmpPtr0),y               ; update dest bitmap
        jmp _drawOctant8
_drawOct7Opening
        lda (zpBmpPtr1),y               ; get current byte from buffer bitmap
        and tabIrisCloseMask,x          ; table: bitmasks for clearing pixels
        ora (zpBmpPtr0),y               ; combine with current byte from dest bitmap
        sta (zpBmpPtr0),y               ; update dest bitmap

_drawOctant8
        ldx zpIrisColXOct68             ; X position (column) for octants 6/8
        cpx #IRIS_MAX_COL_X+1           ; X position (column) negative?
        bcs _exitDrawOctants            ; yes -> skip octant 8
        ldy zpIrisPosYOct78             ; Y position for octants 7/8
        jsr getBitmapPtrsCircle         ; fetch bitmap pointers for screen col X, raster line Y
        ldx zpIrisOffXOct68             ; X position (offset) for octants 6/8
        ldy #$00
        lda zpIrisAnimDirection         ; 0: closing, 1: opening
        bne _drawOct8Opening            ; opening ->
_drawOct8Closing
        lda (zpBmpPtr0),y               ; get current byte from dest bitmap
        and tabIrisOpenMask,x           ; table: bitmasks for copying pixels
        sta (zpBmpPtr0),y               ; update dest bitmap
        rts
_drawOct8Opening
        lda (zpBmpPtr1),y               ; get current byte from buffer bitmap
        and tabIrisCloseMask,x          ; table: bitmasks for clearing pixels
        ora (zpBmpPtr0),y               ; combine with current byte from dest bitmap
        sta (zpBmpPtr0),y               ; update dest bitmap

_exitDrawOctants
        rts

tabIrisOpenMask
        .byte $0f,$0f,$0f,$0f           ; table: bitmasks for copying pixels

tabIrisCloseMask
        .byte $f0,$f0,$f0,$f0           ; table: bitmasks for clearing pixels
        .byte $0f,$0f,$0f,$0f

; Note:
; The resolution of the circle drawing could be improved by
; clearing/copying two pixels at a time rather than four pixels:
; 
; tabIrisOpenMask
;         .byte $03,$03,$0c,$0c,$30,$30,$c0,$c0
; tabIrisCloseMask
;         .byte $fc,$fc,$f3,$f3,$cf,$cf,$3f,$3f


;============== begin assembler leftovers ============

        .text "ameover"
        .word displayGameOver

        .text $88,"gameovr2"
        .word $8ecd                     ; _spinGameOverPhase1L

        .text $84,"ban1"
        .word $8f24                     ; _frame0

        .text $84,"ban2"
        .word $8f35                     ; _frame1

        .text $84,"ban3"
        .word $8f46                     ; _frame2

        .text $84,"ban4"
        .word $8f57                     ; _frame3

        .text $84,"ban5"
        .word $8f68                     ; _frame4

        .text $84,"ban6"
        .word $8f79                     ; _frame5

        .text $84,"ban7"
        .word $8f8a                     ; _frame6

        .text $84,"ban8"
        .word $8f9b                     ; _frame7

        .text $84,"ban9"
        .word $8fac                     ; _frame8

        .text $85,"ban10"
        .word $8fbd                     ; _frame9

        .text $85,"ban11"
        .word $8fce                     ; _frame10

        .text $87,"bandat0"
        .word gameOverLine00

        .text $87,"bandat1"
        .word gameOverLine01

        .text $87,"bandat2"
        .word gameOverLine02

        .text $87,"bandat3"
        .word gameOverLine03

        .text $87,"bandat4"
        .word gameOverLine04

        .text $87,"bandat5"
        .word gameOverLine05

        .text $87,"bandat6"
        .word gameOverLine06

        .text $87,"bandat7"
        .word gameOverLine07

        .text $87,"bandat8"
        .word gameOverLine08

        .text $87,"bandat9"
        .word gameOverLine09

        .text $88,"bandat10"
        .word gameOverLine0a
        
        .text $87,"banvect"
        .word GameOverLineData

        .text $87,"drawend"
        .word displayGameOverFrame

        .text $88,"drawend1"
        .word $9085                     ; _copyGameOverLinesL

        .text $86,"drawit"
        .word $90a2                     ; _copyGameOverColumnsL

        .text $88,"drawsave"
        .word $90a6                     ; _ldaBitmapByte

        .text $88,"drawend3"
        .word $90ba                     ; _continueNextLine

        .text $87,"banwait"
        .word $90ca                     ; _delayL

        .text $88,"banwrts2"
        .word $90e0                     ; _exitGameOverDisplay

        .text $87,"bantime"
        .word spinGameOverDelay

        .text $88,"drawincr"
        .word incZpSrcPtr

        .text $88,"drawinc2"
        .word $90eb                     ; _skipInc

;================ end assembler leftovers ============


jingleGoldCompleteMax
        .byte $09

jingleTransposeMin                      ; minimally transpose jingle by 2 notes
        .byte $02

jingleTransposeMax                      ; maximally transpose jingle by 12 notes
        .byte $0c


jingleDataPtrTbl
        .word jingle_6_data
        .word jingle_9_data
        .word jingle_2_data
        .word jingle_5_data
        .word jingle_7_data
        .word jingle_1_data
        .word jingle_3_data
        .word jingle_8_data
        .word jingle_4_data
        .word jingle_0_data
        .word jingle_empty_data
        .word jingle_empty_data
        .word jingle_empty_data
        .word jingle_empty_data
        .word jingle_empty_data


frequencyTableLow
        .byte $00                       ; (pause, no tone)
        .byte     $e1,$68,$f7,$8f,$30,$da,$8f,$4e,$18,$ef,$d2   ; c-b
        .byte $c3,$c3,$d1,$ef,$1f,$60,$b5,$1e,$9c,$31,$df,$a5   ; c'-b'
        .byte $87,$86,$a2,$df,$3e,$c1,$6b,$3c,$39,$63,$be,$4b   ; c''-b''
        .byte $0f                                               ; c'''

frequencyTableHigh
        .byte $00                       ; (pause, no tone)
        .byte     $08,$09,$09,$0a,$0b,$0b,$0c,$0d,$0e,$0e,$0f   ; c-b
        .byte $10,$11,$12,$13,$15,$16,$17,$19,$1a,$1c,$1d,$1f   ; c'-b'
        .byte $21,$23,$25,$27,$2a,$2c,$2f,$32,$35,$38,$3b,$3f   ; c''-b''
        .byte $43                                               ; c'''

        ; jingle format
        ;   <event>, <event>, ..., <event>, $00
        ; event:
        ;   0: duration
        ;   1: pitch voice 2 (0 = pause)
        ;   2: pitch voice 3 (0 = pause)
        ;   3: sustain/release/volume (unused in C64 version)

jingle_0_data
        .byte $04,$00,$00,$00
        .byte $08,$07,$ff,$a0
        .byte $08,$0c,$ff,$a0
        .byte $08,$10,$ff,$a0
        .byte $04,$13,$ff,$d0
        .byte $0c,$00,$00,$00
        .byte $08,$10,$ff,$a0
        .byte $06,$13,$ff,$d0
        .byte $06,$13,$ff,$a0
        .byte $06,$13,$ff,$70
        .byte $10,$13,$ff,$40
        .byte $10,$00,$00,$00
        .byte $00

jingle_1_data
        .byte $04,$00,$00,$00
        .byte $04,$13,$ff,$c0
        .byte $04,$12,$ff,$a0
        .byte $04,$11,$ff,$a0
        .byte $04,$10,$ff,$a0
        .byte $04,$0f,$ff,$a0
        .byte $04,$14,$ff,$a0
        .byte $04,$13,$ff,$a0
        .byte $04,$12,$ff,$a0
        .byte $04,$13,$ff,$d0
        .byte $04,$12,$ff,$a0
        .byte $04,$11,$ff,$a0
        .byte $04,$10,$ff,$a0
        .byte $04,$0f,$ff,$a0
        .byte $04,$10,$ff,$a0
        .byte $04,$11,$ff,$a0
        .byte $04,$12,$ff,$a0
        .byte $03,$13,$ff,$d0
        .byte $03,$13,$ff,$a0
        .byte $03,$13,$ff,$70
        .byte $04,$13,$ff,$40
        .byte $10,$00,$00,$00
        .byte $00

jingle_2_data
        .byte $04,$00,$00,$00
        .byte $05,$02,$1c,$a0
        .byte $05,$04,$17,$a0
        .byte $05,$05,$15,$a0
        .byte $05,$07,$13,$a0
        .byte $05,$09,$11,$a0
        .byte $05,$0b,$10,$a0
        .byte $05,$0c,$0c,$a0
        .byte $05,$10,$0b,$a0
        .byte $05,$11,$09,$a0
        .byte $05,$13,$07,$a0
        .byte $05,$15,$05,$a0
        .byte $05,$17,$04,$a0
        .byte $05,$1c,$02,$a0
        .byte $10,$00,$00,$00
        .byte $00

jingle_3_data
        .byte $04,$00,$00,$00
        .byte $04,$0c,$ff,$c0
        .byte $08,$00,$00,$00
        .byte $04,$07,$ff,$a0
        .byte $04,$06,$ff,$a0
        .byte $04,$07,$ff,$a0
        .byte $04,$08,$ff,$a0
        .byte $08,$00,$00,$00
        .byte $04,$07,$ff,$a0
        .byte $08,$00,$00,$00
        .byte $0c,$00,$00,$00
        .byte $04,$17,$11,$a0
        .byte $08,$00,$00,$00
        .byte $04,$18,$10,$a0
        .byte $10,$00,$00,$00
        .byte $00

jingle_4_data
        .byte $04,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $05,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $05,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$0c,$ff,$b0
        .byte $05,$00,$00,$00
        .byte $03,$0e,$ff,$b0
        .byte $05,$00,$00,$00
        .byte $03,$10,$ff,$b0
        .byte $05,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $05,$00,$00,$a0
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$07,$ff,$a0
        .byte $05,$00,$00,$00
        .byte $03,$0c,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$10,$ff,$a0
        .byte $01,$00,$00,$00
        .byte $03,$0e,$ff,$b0
        .byte $05,$00,$00,$00
        .byte $03,$0b,$ff,$b0
        .byte $05,$00,$00,$00
        .byte $03,$07,$ff,$b0
        .byte $10,$00,$00,$00
        .byte $00

jingle_5_data
        .byte $04,$00,$00,$00
        .byte $04,$07,$ff,$a0
        .byte $04,$0c,$ff,$a0
        .byte $04,$10,$ff,$a0
        .byte $04,$13,$ff,$d0
        .byte $08,$00,$00,$00
        .byte $02,$13,$ff,$a0
        .byte $02,$00,$00,$00
        .byte $04,$13,$ff,$a0
        .byte $04,$00,$00,$00
        .byte $04,$10,$ff,$d0
        .byte $08,$00,$00,$00
        .byte $02,$10,$ff,$a0
        .byte $02,$00,$00,$00
        .byte $04,$10,$ff,$a0
        .byte $04,$00,$00,$00
        .byte $04,$0c,$ff,$a0
        .byte $04,$00,$00,$00
        .byte $04,$10,$ff,$a0
        .byte $04,$00,$00,$00
        .byte $04,$0c,$ff,$a0
        .byte $04,$00,$00,$00
        .byte $06,$07,$ff,$d0
        .byte $06,$07,$ff,$a0
        .byte $06,$07,$ff,$70
        .byte $06,$07,$ff,$50
        .byte $10,$00,$00,$00
        .byte $00

jingle_6_data
        .byte $04,$00,$00,$00
        .byte $02,$13,$10,$d0
        .byte $06,$13,$10,$a0
        .byte $08,$11,$0e,$a0
        .byte $06,$10,$0c,$a0
        .byte $02,$10,$0c,$40
        .byte $08,$10,$0c,$a0
        .byte $08,$11,$0e,$a0
        .byte $06,$10,$0c,$a0
        .byte $02,$10,$0c,$40
        .byte $08,$0e,$07,$a0
        .byte $08,$10,$0c,$a0
        .byte $06,$0e,$07,$a0
        .byte $02,$0e,$07,$40
        .byte $04,$0e,$07,$d0
        .byte $04,$0e,$07,$b0
        .byte $04,$0e,$07,$a0
        .byte $04,$0e,$07,$90
        .byte $04,$0e,$07,$80
        .byte $04,$0e,$07,$70
        .byte $08,$0e,$07,$a0
        .byte $08,$10,$0c,$a0
        .byte $06,$0e,$07,$a0
        .byte $02,$0e,$07,$40
        .byte $04,$0e,$07,$d0
        .byte $04,$0e,$07,$b0
        .byte $04,$0e,$07,$a0
        .byte $04,$0e,$07,$90
        .byte $04,$0e,$07,$80
        .byte $04,$0e,$07,$70
        .byte $04,$0e,$07,$60
        .byte $04,$0e,$07,$50
        .byte $04,$0e,$07,$40
        .byte $04,$0e,$07,$30
        .byte $08,$0e,$07,$20
        .byte $10,$00,$00,$00
        .byte $00

jingle_7_data
        .byte $04,$00,$00,$00
        .byte $04,$10,$ff,$a0
        .byte $04,$13,$ff,$a0
        .byte $04,$18,$ff,$a0
        .byte $04,$15,$ff,$a0
        .byte $04,$11,$ff,$a0
        .byte $04,$0e,$ff,$a0
        .byte $04,$13,$ff,$a0
        .byte $04,$11,$ff,$a0
        .byte $04,$0e,$ff,$a0
        .byte $04,$0c,$ff,$a0
        .byte $0c,$00,$00,$00
        .byte $02,$18,$ff,$a0
        .byte $02,$13,$ff,$a0
        .byte $02,$10,$ff,$a0
        .byte $04,$0c,$ff,$a0
        .byte $10,$00,$00,$00
        .byte $00

jingle_8_data
        .byte $04,$00,$00,$00
        .byte $04,$18,$ff,$a0
        .byte $04,$17,$ff,$a0
        .byte $04,$16,$ff,$a0
        .byte $04,$15,$ff,$a0
        .byte $04,$14,$ff,$a0
        .byte $04,$13,$ff,$a0
        .byte $04,$12,$ff,$a0
        .byte $04,$11,$ff,$a0
        .byte $04,$10,$ff,$a0
        .byte $04,$0f,$ff,$a0
        .byte $04,$0e,$ff,$a0
        .byte $04,$0d,$ff,$a0
        .byte $04,$0c,$ff,$a0
        .byte $0c,$00,$00,$00
        .byte $02,$0c,$ff,$a0
        .byte $02,$10,$ff,$a0
        .byte $02,$13,$ff,$a0
        .byte $04,$18,$ff,$a0
        .byte $10,$00,$00,$00
        .byte $00

jingle_9_data
        .byte $04,$00,$00,$00
        .byte $04,$10,$ff,$a0
        .byte $04,$13,$ff,$a0
        .byte $04,$18,$ff,$a0
        .byte $04,$13,$ff,$a0
        .byte $04,$10,$ff,$a0
        .byte $04,$13,$ff,$a0
        .byte $04,$18,$ff,$a0
        .byte $04,$13,$ff,$a0
        .byte $04,$15,$ff,$d0
        .byte $04,$00,$00,$00
        .byte $02,$11,$ff,$a0
        .byte $02,$11,$ff,$80
        .byte $04,$00,$00,$00
        .byte $02,$11,$ff,$a0
        .byte $02,$11,$ff,$80
        .byte $0c,$00,$00,$00
        .byte $04,$12,$ff,$a0
        .byte $04,$15,$ff,$a0
        .byte $04,$1f,$0f,$a0
        .byte $04,$15,$ff,$a0
        .byte $04,$12,$ff,$a0
        .byte $04,$15,$ff,$a0
        .byte $04,$1f,$0f,$a0
        .byte $04,$15,$ff,$a0
        .byte $04,$17,$ff,$d0
        .byte $04,$00,$00,$00
        .byte $02,$13,$ff,$a0
        .byte $02,$13,$ff,$80
        .byte $04,$00,$00,$00
        .byte $02,$13,$ff,$a0
        .byte $02,$13,$ff,$80
        .byte $10,$00,$00,$00
        .byte $00

jingle_empty_data
        .byte $00

;================ begin duplicate code ===============
;================ see: $693e-$69ff     ===============
        sta zpBoardActionPtr+1
        ldy zpCursorCol
        lda zpEditorNewShape            ; user selected shape (0-9)
        eor (zpBoardActionPtr),y        ; compare with current board shape at cursor
        beq _noBoardChange
        lsr zpEditorBoardUnchanged      ; mark board as changed (change a '1' into '0')
_noBoardChange
        lda zpEditorNewShape            ; user selected shape (0-9)
        sta (zpBoardActionPtr),y        ; change the tile in the board
        jsr replaceTileBitmap0
        jmp boardEditorMainLoop

_checkEditorCmdKey
        sta zpEditorUserKey             ; key pressed by user
        ldy #$ff                        ; start with index 0
_checkEditorCmdKeyL
        iny
        lda boardEditorCmdKeyTbl,y      ; compare with command key codes
        beq abortBoardEditorCmdWithBell_copy ; unsupported key? ->
        cmp zpEditorUserKey             ; key pressed by user
        bne _checkEditorCmdKeyL
        tya
        asl
        tay
        lda boardEditorCmdJmpTbl+1,y
        pha                             ; as return address on stack
        lda boardEditorCmdJmpTbl+0,y
        pha                             ; as return address on stack
        rts                             ; jump to board editor function

abortBoardEditorCmdWithBell_copy
        jsr soundBell
        jmp boardEditorMainLoop

editorCursorUp_copy
        lda zpCursorRow
        beq abortBoardEditorCmdWithBell_copy
        dec zpCursorRow
        bpl $992a                       ; boardEditorMainLoop_copy

editorCursorLeft_copy
        lda zpCursorCol
        beq abortBoardEditorCmdWithBell_copy
        dec zpCursorCol
        bpl $992a                       ; boardEditorMainLoop_copy

editorCursorRight_copy
        lda zpCursorCol
        cmp #BOARD_WIDTH-1
        bcs abortBoardEditorCmdWithBell_copy
        inc zpCursorCol
        bne $992a                       ; boardEditorMainLoop_copy

editorCursorDown_copy
        lda zpCursorRow
        cmp #BOARD_HEIGHT-1
        bcs abortBoardEditorCmdWithBell_copy
        inc zpCursorRow
        bne $992a                       ; boardEditorMainLoop_copy

saveCurrentBoard_copy                   ; save the current board
        lda zpCursorRow                 ; store cursor row and column
        pha
        lda zpCursorCol
        pha
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores
        cmp #DISK_UNKNOWN
        bne _saveCurrentBoardJ1
        jsr warningUnknownDisk          ; message: disk is not a lode runner data disk
        jmp saveCurrentBoardAbort
_saveCurrentBoardJ1
        cmp #DISK_MASTER
        bne _saveCurrentBoardJ2
        jsr warningMasterDiskNotAllowed ; message: not allowed to manipulate master disk
        jmp saveCurrentBoardAbort
_saveCurrentBoardJ2
        jsr packBoard                   ; pack and store the board in the level set
        pla
        sta zpCursorCol                 ; restore cursor row and column
        pla
        sta zpCursorRow
        lda #$01
        sta zpEditorBoardUnchanged      ; 1: board unchanged, 0: changes pending
        rts

saveCurrentBoardAbort_copy
        lda #$00                        ; set editor cursor to home (0,0)
        sta zpCursorCol
        sta zpCursorRow
        jmp boardEditorMainLoop         ; BUG: leaves four bytes dead on the stack

editorSaveBoard_copy
        jsr saveCurrentBoard
        jmp boardEditorMainLoop

editorBoardForward_copy
        lda currentLevel                ; current level (0-based)
        cmp #BOARD_MAXIMUM              ; 149
editorBoardBackAbort_copy
        beq abortBoardEditorCmdWithBell_copy
        jsr warningUnsavedLevelChanges  ; warn about unsaved level changes
        inc currentLevel                ; current level (0-based)
        inc displayedLevel              ; current level (displayed)
        jmp startBoardEditor

editorBoardBack_copy
        lda currentLevel                ; current level (0-based)
        beq editorBoardBackAbort_copy
        jsr warningUnsavedLevelChanges  ; warn about unsaved level changes
        dec displayedLevel              ; current level (displayed)
        dec currentLevel                ; current level (0-based)
        jmp startBoardEditor

editorQuit_copy
        jsr warningUnsavedLevelChanges  ; warn about unsaved level changes
        jmp editorMenuLoop

warningUnsavedLevelChanges_copy         ; warn about unsaved level changes
        lda zpEditorBoardUnchanged      ; 1: board unchanged, 0: changes pending
        beq _unsavedChanges
        rts
_unsavedChanges
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda multiColorValues            ; store current multi color values
        pha
        lda #(COL_YELLOW << 4 | COL_CYAN)
        jsr setColorMemory
        lda #>Bitmap0                   ; print to bitmap 0
        sta zpBitmapPage2               ; set active bitmap page for printing text
        lda #$00
        sta zpCursorCol                 ; set cursor to (0,0)
        sta zpCursorRow
        jsr printString
.enc "high"
        .text "LEVEL HAS BEEN CHANGED BUT",$8d
        .text "NOT SAVED. DO YOU WISH TO",$8d
        .text "SAVE MODIFIED LEVEL (Y/N) ",$00
        jsr soundBell
_promptYesNoL
        lda #$00
        jsr waitForUserKey              ; flash cursor while waiting for input
        ldx #$00
        stx keyboardCode                ; clear keyboard matrix code
        cmp #KEY_CODE_N                 ; 'N'?
        beq _continueAfterSave
        cmp #KEY_CODE_Y                 ; 'Y'?
        bne _promptYesNoL
        jsr saveCurrentBoard            ; save the current board
_continueAfterSave
        pla                             ; restore current multi color values
        jmp setColorMemory

assertUserDisk_copy
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores
        cmp #DISK_UNKNOWN               ; check for signature
        bne _assertUserDiskJ1           ; signature found ->
        jsr warningUnknownDisk          ; message: disk is not a lode runner data disk
        jmp editorMenuLoop              ; BUG: leaves two bytes dead on the stack
_assertUserDiskJ1
        cmp #$01                        ; master disk?
        bne _assertUserDiskJ2           ; no -> user disk
        jsr warningMasterDiskNotAllowed ; message: not allowed to manipulate master disk
        jmp editorMenuLoop              ; BUG: leaves two bytes dead on the stack
_assertUserDiskJ2
        rts

warningMasterDiskNotAllowed_copy        ; message: not allowed to manipulate master disk
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda multiColorValues            ; store current multi color values
        pha
        lda #>Bitmap0                   ; print to bitmap 0
        sta zpBitmapPage2               ; set active bitmap page for printing text
        lda #(COL_YELLOW << 4 | COL_CYAN)
        jsr setColorMemory
        lda #$00                        ; set cursor to home (0,0)
        sta zpCursorCol
        sta zpCursorRow
        jsr printString
        .text "USER NOT ALLOWED TO",$8d
        .text "MANIPULATE MASTER DISKETTE.",$00

dialogHitKeyToContinue_copy
        jsr printString
        .text $8d
        .text $8d
        .text "HIT A KEY TO CONTINUE ",$00
        jsr soundBell
        lda #SHAPE_BLANK                ; use b/w cursor
        jsr waitForUserKey              ; flash cursor while waiting for input
        lda #$00
        sta keyboardCode                ; keyboard matrix code (no key: 0)
        lda #>Bitmap1                   ; print to bitmap 1
        sta zpBitmapPage2               ; set active bitmap page for printing text
        jsr printBoardFooter            ; draw solid ground and print status line
        lda #$00
        sta boardRequiresFullInit       ; 0: board has been initialized before, != 0: board requires full init
        jsr initBoardState              ; initialize the board state (player, #enemies, #ladders etc)
        pla                             ; restore current multi color values
        jmp setColorMemory              ; set color memory and return

warningUnknownDisk_copy                 ; message: disk is not a lode runner data disk
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda multiColorValues            ; store current multi color values
        pha
        lda #(COL_YELLOW << 4 | COL_CYAN)
        jsr setColorMemory
        lda #>Bitmap0                   ; print to bitmap 0
        sta zpBitmapPage2               ; set active bitmap page for printing text
        lda #$00                        ; set cursor to home (0,0)
        sta zpCursorCol
        sta zpCursorRow
        jsr printString
        .text "DISKETTE IN DRIVE IS NOT A",$8d
        .text "LODE RUNNER DATA DISK.",$00
        jmp dialogHitKeyToContinue

boardEditorWaitForUserKey_copy
        ldy zpCursorRow
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        ldy zpCursorCol
        lda (zpBoardActionPtr),y        ; shape under cursor
        jsr waitForUserKey              ; flash cursor while waiting for input
        rts

boardEditorCmdKeyTbl_copy
        .byte KEY_CODE_J                ; J: cursor left
        .byte KEY_CODE_I                ; I: cursor up
        .byte KEY_CODE_K                ; K: cursor right
        .byte KEY_CODE_M                ; M: cursor down
        .byte KEY_CODE_S+$80            ; <Ctrl>-S: save level
        .byte KEY_CODE_F+$80            ; <Ctrl>-F: board forward
        .byte KEY_CODE_B+$80            ; <Ctrl>-B: board back
        .byte KEY_CODE_Q+$80            ; <Ctrl>-Q:
        .byte $00

boardEditorCmdJmpTbl_copy
        .word editorCursorLeft-1
        .word editorCursorUp-1
        .word editorCursorRight-1
        .word editorCursorDown-1
        .word editorSaveBoard-1
        .word editorBoardForward-1
        .word editorBoardBack-1
        .word editorQuit-1

inputLevelChecked_copy
        ldy currentLevel                ; current level (0-based)
        iny                             ; inc level for display
        tya
        jsr convertHexToDecimal         ; convert hex number to 3 digit decimal
        lda zpCursorCol
        sta cursorColDigit0             ; store cursor pos at first of level digit

        ldy #$00                        ; index: digit to print
_printLevelL
        lda digitHigh,y
        sty digitEntryPos               ; position of digit to be entered
        jsr printDigit
        ldy digitEntryPos
        iny
        cpy #$03                        ; print three digits
        bcc _printLevelL

        lda cursorColDigit0             ; restore cursor pos
        sta zpCursorCol
        ldy #$00                        ; next entry: first digit
        sty digitEntryPos

_readDigitLoop
        ldx digitEntryPos
        lda digitHigh,x
        clc
        adc #$3b                        ; convert to "lr" encoded digit

        jsr waitForUserKey
        jsr checkKeyDigit               ; check whether user pressed a digit key
        bcc _keyIsDigit                 ; yes -> store it, print it, continue
        cmp #KEY_CODE_RETURN            ; keyboard code = <Return> ?
        beq _keyIsReturn                ; yes ->
        cmp #KEY_CODE_UP_DOWN           ; cursor key up/down: handle as backspace
        beq _readDigitBackspace
        cmp #KEY_CODE_CTRL+KEY_CODE_LEFT_RIGHT  ; <ctrl>-cursor left
        bne _checkCursorRight
_readDigitBackspace
        ldx digitEntryPos
        beq _unexpectedKey              ; can't move cursor left ->
        dec digitEntryPos
        dec zpCursorCol
        jmp $6bcc                       ; _readDigitLoop

_checkCursorRight
        cmp #KEY_CODE_LEFT_RIGHT        ; cursor key left/right? (acts as cursor right)
        bne _checkKeyEscape
        ldx digitEntryPos
        cpx #$02                        ; max digit entry position: 2
        beq _unexpectedKey              ; can't move cursor right ->
        inc zpCursorCol                 ; increase digit entry position (move cursor to right)
        inc digitEntryPos
        jmp $6bcc                       ; _readDigitLoop
_checkKeyEscape
        cmp #KEY_CODE_RUN_STOP
        bne _checkKeyDigit
        jmp returnToEditorMenu
_checkKeyDigit
        jsr checkKeyDigit               ; check whether user pressed a digit key
        bcs _unexpectedKey              ; no ->
_keyIsDigit
        ldy digitEntryPos
        sta digitHigh,y
        jsr printDigit
        inc digitEntryPos
        lda digitEntryPos
        cmp #$03                        ; digit entry position < 3?
        bcc _readDigitLoop              ; yes -> enter next digit

        dec digitEntryPos               ; step back, in order to read digit again
        dec zpCursorCol
        jmp $6bcc                       ; _readDigitLoop

_unexpectedKey
        jsr soundBell
        jmp $6bcc                       ; _readDigitLoop

_keyIsReturn
        lda cursorColDigit0
        clc
        adc #$03                        ; position cursor after 3 digit level number
        sta zpCursorCol
        lda #$00                        ; init converted level number
        ldx digitHigh                   ; level number * 100
        beq _convertMidDigit
        clc
_decToBinLoopHigh
        adc #100                        ; convert to binary representation
        bcs _returnBadLevelNumber       ; overflow -> level number is too big
        dex                             ; dec level number * 100
        bne _decToBinLoopHigh

_convertMidDigit
        ldx digitMid
        beq _convertLowDigit
        clc
_decToBinLoopMid
        adc #10                         ; convert to binary representation
        bcs _returnBadLevelNumber       ; overflow -> level number is too big
        dex                             ; dec level number * 10
        bne _decToBinLoopMid

_convertLowDigit
        clc
        adc digitLow                    ; add level number * 1 digit
        bcs _returnBadLevelNumber       ; overflow -> level number is too big
        sta displayedLevel              ; current level (displayed)
        tay
        dey
        sty currentLevel                ; current level (0-based)
        cpy #BOARD_MAXIMUM+1            ; 150 / $95+1
_returnBadLevelNumber
        rts

waitForKeyboardInput_copy
        lda #$00                        ; shape under cursor (blank)
        jsr waitForUserKey              ; flash cursor while waiting for input
        ldx #$00
        stx keyboardCode                ; keyboard matrix code (no key: 0)
        cmp #KEY_CODE_RUN_STOP
        bne _finish
        jmp returnToEditorMenu
_finish
        rts

digitEntryPos_copy
initialsEntryPos_copy
        .byte $00                       ; position of digit to be entered

cursorColDigit0_copy
        .byte $00                       ; cursor column of first digit to be entered

selectedLevel1_copy
        .byte $00

selectedLevel2_copy
        .byte $00

checkKeyDigit_copy                      ; check whether user pressed a digit key
        lda keyboardCode                ; keyboard matrix code (no key: 0)
        ldy #$00                        ; clear last key stroke
        sty keyboardCode
        ldy #$09                        ; iterate over all digits 9..0
_checkDigitsLoop
        cmp keyCodesDigitsTab,y         ; compare with key code for digit y
        beq _digitKeyPressed
        dey
        bpl _checkDigitsLoop
        sec                             ; mark digit not found
        rts
_digitKeyPressed
        tya                             ; return digit key that has been pressed
        clc                             ; mark as found
        rts


keyCodesDigitsTab_copy
        .byte KEY_CODE_0, KEY_CODE_1
        .byte KEY_CODE_2, KEY_CODE_3
        .byte KEY_CODE_4, KEY_CODE_5
        .byte KEY_CODE_6, KEY_CODE_7
        .byte KEY_CODE_8, KEY_CODE_9

strDaneBigham_copy
        .text "DANE BIGHAM"

handleHighScoresEntry_copy
        lda allowHighScoreEntry         ; 0: player cheated, no high score entry, 1: did not cheat
        beq _exit
        lda score+0
        ora score+1
        ora score+2
        ora score+3
        beq _exit
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores          ; load high score from disk
        beq _exit                       ; DISK_UNKNOWN? ->

        ldy #$00
_copyHighScoreDataL
        lda highScoreData,y
        sta highScoreDataBackup,y
        iny
        bne _copyHighScoreDataL

        ldy #$01
_findHighScoreEntryL
        ldx tabHighScoreOffsets,y       ; offset of high score Y
        lda displayedLevel              ; current level (displayed)
        cmp highScoreData+3,x           ; entry: level
        bcc _continueNextEntry          ; level < level in entry? ->
        bne newHighScoreIndexFound_copy ; level > level in entry? ->
        ; same level as current entry
        lda score+3                     ; current score (highest)
        cmp highScoreData+4,x           ; entry: score
        bcc _continueNextEntry          ; score < score in entry? ->
        bne newHighScoreIndexFound_copy ; score > score in entry? ->
        lda score+2                     ; current score (next to highest)
        cmp highScoreData+5,x           ; entry: score
        bcc _continueNextEntry
        bne newHighScoreIndexFound_copy
        lda score+1                     ; current score (next to lowest)
        cmp highScoreData+6,x           ; entry: score
        bcc _continueNextEntry
        bne newHighScoreIndexFound_copy
        lda score+0                     ; current score (lowest)
        cmp highScoreData+7,x           ; entry: score
        bcc _continueNextEntry
        bne newHighScoreIndexFound_copy
_continueNextEntry
        iny
        cpy #11
        bcc _findHighScoreEntryL
_exit
        rts

newHighScoreIndexFound_copy
        cpy #10                         ; is new entry last entry in high score list?
        beq _updateHighScoreRecord      ; yes ->
        sty zpHighScoreEntryOffset      ; offset of current high score entry
        ldy #$09
_moveHighScoreRecordsL
        ldx tabHighScoreOffsets,y       ; get offset of high score record y
        lda #$08
        sta zpHighScoreIter             ; iterator for moving high scores
_copyRecordL
        lda highScoreData,x             ; copy high score record y
        sta highScoreData+8,x           ; to record y+1
        inx
        dec zpHighScoreIter
        bne _copyRecordL
        cpy zpHighScoreEntryOffset      ; offset of current high score entry
        beq _updateHighScoreRecord
        dey
        bne _moveHighScoreRecordsL

_updateHighScoreRecord
        ldx tabHighScoreOffsets,y       ; offset of high score record
        lda #$a0
        sta highScoreData+0,x           ; clear initials (3 bytes)
        sta highScoreData+1,x
        sta highScoreData+2,x

        lda displayedLevel              ; current level (displayed)
        sta highScoreData+3,x           ; copy to record

        lda score+3                     ; current score
        sta highScoreData+4,x           ; copy to record (4 bytes)
        lda score+2
        sta highScoreData+5,x
        lda score+1
        sta highScoreData+6,x
        lda score+0
        sta highScoreData+7,x

        sty zpNewHighScoreEntryId
        lda tabHighScoreOffsets,y       ; offset of high score record
        sta _ldaHighScoreData+1-$3000   ; self modifying code: read from current high score record
        sta _staHighScoreData+1-$3000   ; self modifying code: write to current high score record
        jsr displayHighScores           ; display current high score table

        lda zpNewHighScoreEntryId
        clc
        adc #$04                        ; set cursor position to initials within high score record
        sta zpCursorRow
        lda #$07
        sta zpCursorCol
        ldx #$00
        stx initialsEntryPos
_inputInitialsL
        ldx initialsEntryPos
_ldaHighScoreData
        lda highScoreData,x
        jsr convertAsciiToShapeId       ; convert to shape under cursor (blank)
        jsr waitForUserKey              ; flash cursor while waiting for input
        ldx #$00
        stx keyboardCode                ; keyboard matrix code (no key: 0)
        jsr convertKeyCodeToAscii       ; convert to high ascii
        cmp #$8d                        ; ascii high: CR
        beq _finishHighScoreEntry

        cmp #$88                        ; ascii high: backspace
        bne _inputInitialsJ1            ; no ->
        ldx initialsEntryPos
        beq _inputCharBad
        dec initialsEntryPos
        dec zpCursorCol
        jmp _inputInitialsL-$3000

_inputInitialsJ1
        cmp #$95                        ; cursor right
        bne _inputInitialsJ2
        ldx initialsEntryPos
        cpx #$02                        ; at last entry position?
        beq _inputCharBad               ; yes -> ignore input
        inc zpCursorCol
        inc initialsEntryPos
        jmp _inputInitialsL-$3000

_inputInitialsJ2
        cmp #'.'                        ; ascii high: $ae
        beq _inputCharOk
        cmp #' '                        ; ascii high: $a0
        beq _inputCharOk
        cmp #'A'                        ; ascii high: $c1
        bcc _inputCharBad               ; < 'A'? -> bad character
        cmp #'Z'+1                      ; ascii high: $db
        bcs _inputCharBad               ; > 'Z'? -> bad character
_inputCharOk
        ldy initialsEntryPos
_staHighScoreData
        sta highScoreData,y             ; store initials character in high score data
        jsr printChar                   ; print character
        inc initialsEntryPos
        lda initialsEntryPos
        cmp #$03                        ; all 3 initials entered?
        bcc _inputInitialsL             ; no ->

        dec initialsEntryPos            ; backup one position
        dec zpCursorCol                 ; (allow last initial to be overwritten)
        jmp _inputInitialsL-$3000       ; continue input loop

_inputCharBad
        jsr soundBell
        jmp _inputInitialsL-$3000

_finishHighScoreEntry
        ldy #$00
_copyHighScoreDataToUpdated
        lda highScoreData,y             ; copy updated high score data
        sta highScoreDataUpdated,y      ;   to dedicated buffer
        iny
        bne _copyHighScoreDataToUpdated

        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadSaveHighScores

        ldy #$00
_ensureSafeUpdateL
        lda highScoreData,y             ; ensure disk has not changed
        cmp highScoreDataBackup,y       ; (high score data is the same as before)
        bne _exit                       ; changes detected -> abort saving high scores
        lda highScoreDataUpdated,y
        sta highScoreData,y             ; update list of high scores in memory
        iny
        bne _ensureSafeUpdateL

        lda #DISK_CMD_SAVE              ; command: save file
        jsr loadSaveHighScores
_exit
        jmp attractStateLoop

displayHighScores_copy
        jsr clearBitmap0                ; clear bitmap 0 and turn off sprites
        lda #(COL_YELLOW << 4 | COL_CYAN)
        jsr setColorMemory
        lda #$00
        sta zpCursorCol                 ; set cursor to (0,0)
        sta zpCursorRow
        jsr printString
        .text "    LODE RUNNER HIGH SCORES",$8d
        .text $8d
        .text $8d
        .text "    INITIALS LEVEL  SCORE",$8d
        .text "    -------- ----- --------",$8d,$00

        lda #$01
        sta zpHighScoreEntryNumber      ; number of current high score entry (1-10)
_displayHighScoresL
        cmp #10                         ; last entry (10)?
        bne _displayHighScoresJ1        ; no ->
        lda #$01                        ; print "10" (high score entry 10)
        jsr printDigit
        lda #$00
        jsr printDigit
        jmp _displayHighScoresJ2-$3000

_displayHighScoresJ1                    ; high score entry < 10:
        lda #' '                        ; only one digit: print space
        jsr printChar                   ; print character
        lda zpHighScoreEntryNumber      ; number of current high score entry (1-10)
        jsr printDigit                  ; print the entry number

_displayHighScoresJ2
        jsr printString
        .text ".    ",$00
        ldx zpHighScoreEntryNumber      ; number of current high score entry (1-10)
        ldy tabHighScoreOffsets,x       ; fetch data offset for current entry
        sty zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+3,y           ; record: level
        bne _displayHighScoresJ3        ; level > 0 -> display record
        jmp _continueNextRecord-$3000   ; else, skip this record

_displayHighScoresJ3
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+0,y           ; record: initials, 1st character
        jsr printChar                   ; print character
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+1,y           ; record: initials, 2nd character
        jsr printChar                   ; print character
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+2,y
        jsr printChar                   ; print character

        jsr printString                 ; print spacer
        .text "    ",$00

        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+3,y           ; record: level
        jsr convertHexToDecimal         ; convert hex number to 3 digit decimal
        lda digitHigh
        jsr printDigit                  ; print three digits of level
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit

        jsr printString                 ; print spacer
        .text "  ",$00

        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+4,y
        jsr splitBcdIntoDigits          ; print first two digits of score
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+5,y
        jsr splitBcdIntoDigits          ; print second two digits of score
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+6,y
        jsr splitBcdIntoDigits          ; print third two digits of score
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit
        ldy zpHighScoreEntryOffset      ; offset of current high score entry
        lda highScoreData+7,y
        jsr splitBcdIntoDigits          ; print last two digits of score
        lda digitMid
        jsr printDigit
        lda digitLow
        jsr printDigit

_continueNextRecord
        jsr printNewline
        inc zpHighScoreEntryNumber      ; inc number of current high score entry
        lda zpHighScoreEntryNumber      ; number of current high score entry (1-10)
        cmp #11                         ; repeat for 10 high score entries
        bcs _exit                       ; finished ->
        jmp _displayHighScoresL-$3000   ; print next entry
_exit
        rts

tabHighScoreOffsets_copy
        .byte $00,$00,$08,$10,$18,$20,$28,$30,$38,$40,$48

convertKeyCodeToAscii_copy
        cmp #$82                        ; <SHIFT>-left (?)
        bne _convertJ1
        lda #$07                        ; cursor up -> converts to backspace
_convertJ1
        tay
        lda tabKeyCodeToAscii,y
        ora #$80                        ; convert to high ascii
        rts

tabKeyCodeToAscii_copy
.enc "none"
        .text $00,$0d,$15,$ff,$ff,$ff,$ff,$08,"3WA4ZSE",$ff
        .text "5RD6CFTX7YG8BHUV"
        .text "9IJ0MKON+PL-.:",$ff,","
        .text "\*;",$ff,$ff,"=",$ff,"/1",$ff,$ff,"2 ",$ff,"Q",$ff

setupBoard_copy
        stx boardRequiresFullInit       ; 0: board has been initialized before, != 0: board requires full init
        ldx #$ff                        ; init value: player not found yet
        stx zpPlayerX                   ; player, X position on board
        inx                             ; x = 0
        stx numExitLadders              ; number of exit ladders = 0
        stx goldLeft                    ; gold left = 0
        stx numEnemies                  ; enemies left = 0
        stx enemyIndex                  ; current enemy, index = 0
        stx zpHoleDigAnimCtr            ; animation counter for a hole being drilled

        stx zpPackedDataIdx             ; index into packed board data = 0
        stx zpNibbleSelect              ; next nibble to unpack
        stx zpCursorRow

        txa                             ; A = 0 (init value for array inits below)
        ldx #MAX_HOLES_OPEN
_initOpenHoleTimersL
        sta holesOpenCtr,x              ; all holes closed (timer = 0)
        dex
        bpl _initOpenHoleTimersL

        ldx #MAX_NUM_ENEMIES            ; iterate over max number of enemies (5)
_initEnemiesRespawnCtrsL
        sta enemiesRespawnCtr,x         ; time left until enemy respawns
        dex
        bpl _initEnemiesRespawnCtrsL

        lda #PLAYER_ALIVE               ; 1: player alive
                                        ;    operation: load current board from board set
        sta playerAlive                 ; 0: player dead, 1: player alive

        ; unpack board
        lda currentLevel                ; current level (0-based)
        cmp reloadBoard                 ; force loading board if != currentLevel
        beq _unpackBoard
        lda #DISK_CMD_LOAD              ; command: load file
        jsr loadStoreBoard              ; load from (1) or store (2) board in board set
_unpackBoard
        lda currentLevel                ; current level (0-based)
        sta reloadBoard                 ; force loading board if != currentLevel
        ldy zpCursorRow

_unpackBoardRowLoop
        lda boardActionOffsetLb,y
        sta zpBoardActionPtr+0
        sta zpBoardLayoutPtr+0
        lda boardActionOffsetHb,y
        sta zpBoardActionPtr+1
        lda boardLayoutOffsetHb,y
        sta zpBoardLayoutPtr+1
        lda #$00

;================ end duplicate code ===============

        .align $100

shapeShiftTables
la000   ; shift 0 pixels, left
        .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
        .byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f
        .byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
        .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f
        .byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f
        .byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
        .byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f
        .byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f
        .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
        .byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f
        .byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
        .byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
        .byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
        .byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df
        .byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
        .byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff
la100   ; shift 0 pixels, right
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
la200   ; shift 2 pixels, left
        .byte $00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$02,$03,$03,$03,$03
        .byte $04,$04,$04,$04,$05,$05,$05,$05,$06,$06,$06,$06,$07,$07,$07,$07
        .byte $08,$08,$08,$08,$09,$09,$09,$09,$0a,$0a,$0a,$0a,$0b,$0b,$0b,$0b
        .byte $0c,$0c,$0c,$0c,$0d,$0d,$0d,$0d,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f
        .byte $10,$10,$10,$10,$11,$11,$11,$11,$12,$12,$12,$12,$13,$13,$13,$13
        .byte $14,$14,$14,$14,$15,$15,$15,$15,$16,$16,$16,$16,$17,$17,$17,$17
        .byte $18,$18,$18,$18,$19,$19,$19,$19,$1a,$1a,$1a,$1a,$1b,$1b,$1b,$1b
        .byte $1c,$1c,$1c,$1c,$1d,$1d,$1d,$1d,$1e,$1e,$1e,$1e,$1f,$1f,$1f,$1f
        .byte $20,$20,$20,$20,$21,$21,$21,$21,$22,$22,$22,$22,$23,$23,$23,$23
        .byte $24,$24,$24,$24,$25,$25,$25,$25,$26,$26,$26,$26,$27,$27,$27,$27
        .byte $28,$28,$28,$28,$29,$29,$29,$29,$2a,$2a,$2a,$2a,$2b,$2b,$2b,$2b
        .byte $2c,$2c,$2c,$2c,$2d,$2d,$2d,$2d,$2e,$2e,$2e,$2e,$2f,$2f,$2f,$2f
        .byte $30,$30,$30,$30,$31,$31,$31,$31,$32,$32,$32,$32,$33,$33,$33,$33
        .byte $34,$34,$34,$34,$35,$35,$35,$35,$36,$36,$36,$36,$37,$37,$37,$37
        .byte $38,$38,$38,$38,$39,$39,$39,$39,$3a,$3a,$3a,$3a,$3b,$3b,$3b,$3b
        .byte $3c,$3c,$3c,$3c,$3d,$3d,$3d,$3d,$3e,$3e,$3e,$3e,$3f,$3f,$3f,$3f
la300   ; shift 2 pixels, right
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
        .byte $00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0,$00,$40,$80,$c0
la400   ; shift 4 pixels, left
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
        .byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
        .byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
        .byte $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
        .byte $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
        .byte $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
        .byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08
        .byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09
        .byte $0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a
        .byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
        .byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
        .byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
        .byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
        .byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
la500   ; shift 4 pixels, right
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
        .byte $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0
la600   ; shift 6 pixels, left
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
        .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
        .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
        .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
        .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
        .byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
        .byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
        .byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
        .byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
la700   ; shift 6 pixels, right
        .byte $00,$04,$08,$0c,$10,$14,$18,$1c,$20,$24,$28,$2c,$30,$34,$38,$3c
        .byte $40,$44,$48,$4c,$50,$54,$58,$5c,$60,$64,$68,$6c,$70,$74,$78,$7c
        .byte $80,$84,$88,$8c,$90,$94,$98,$9c,$a0,$a4,$a8,$ac,$b0,$b4,$b8,$bc
        .byte $c0,$c4,$c8,$cc,$d0,$d4,$d8,$dc,$e0,$e4,$e8,$ec,$f0,$f4,$f8,$fc
        .byte $00,$04,$08,$0c,$10,$14,$18,$1c,$20,$24,$28,$2c,$30,$34,$38,$3c
        .byte $40,$44,$48,$4c,$50,$54,$58,$5c,$60,$64,$68,$6c,$70,$74,$78,$7c
        .byte $80,$84,$88,$8c,$90,$94,$98,$9c,$a0,$a4,$a8,$ac,$b0,$b4,$b8,$bc
        .byte $c0,$c4,$c8,$cc,$d0,$d4,$d8,$dc,$e0,$e4,$e8,$ec,$f0,$f4,$f8,$fc
        .byte $00,$04,$08,$0c,$10,$14,$18,$1c,$20,$24,$28,$2c,$30,$34,$38,$3c
        .byte $40,$44,$48,$4c,$50,$54,$58,$5c,$60,$64,$68,$6c,$70,$74,$78,$7c
        .byte $80,$84,$88,$8c,$90,$94,$98,$9c,$a0,$a4,$a8,$ac,$b0,$b4,$b8,$bc
        .byte $c0,$c4,$c8,$cc,$d0,$d4,$d8,$dc,$e0,$e4,$e8,$ec,$f0,$f4,$f8,$fc
        .byte $00,$04,$08,$0c,$10,$14,$18,$1c,$20,$24,$28,$2c,$30,$34,$38,$3c
        .byte $40,$44,$48,$4c,$50,$54,$58,$5c,$60,$64,$68,$6c,$70,$74,$78,$7c
        .byte $80,$84,$88,$8c,$90,$94,$98,$9c,$a0,$a4,$a8,$ac,$b0,$b4,$b8,$bc
        .byte $c0,$c4,$c8,$cc,$d0,$d4,$d8,$dc,$e0,$e4,$e8,$ec,$f0,$f4,$f8,$fc

Shapes
la800
        ; line 0, left
        .byte $00,$a8,$aa,$c3,$00,$aa,$c0,$00,$08,$02,$ff,$10,$10,$10,$0c,$02
        .byte $02,$02,$0c,$64,$c9,$61,$06,$18,$61,$18,$06,$00,$00,$08,$00,$a4
        .byte $04,$00,$00,$00,$00,$08,$00,$00,$02,$02,$02,$08,$08,$41,$04,$10
        .byte $41,$10,$04,$04,$04,$44,$44,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
la868   ; line 0, right
        .byte $00,$80,$80,$00,$00,$80,$00,$00,$00,$00,$c0,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$c0,$80,$80,$00,$00,$80,$00,$00,$00,$00,$00,$00,$80
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$40,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
la8d0   ; line 1, left
        .byte $00,$a8,$aa,$c3,$ff,$aa,$c0,$00,$14,$07,$ff,$38,$38,$38,$0c,$07
        .byte $07,$07,$8c,$6e,$dd,$61,$06,$18,$61,$18,$06,$00,$00,$00,$00,$a8
        .byte $14,$04,$00,$00,$00,$1c,$00,$00,$05,$05,$05,$14,$14,$41,$04,$10
        .byte $41,$10,$04,$04,$84,$44,$44,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
la938   ; line 1, right
        .byte $00,$80,$80,$00,$c0,$80,$00,$00,$00,$00,$c0,$00,$00,$00,$40,$00
        .byte $00,$00,$00,$c0,$80,$80,$00,$00,$80,$00,$00,$00,$00,$00,$00,$80
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$40,$00,$40,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
la9a0   ; line 2, left
        .byte $00,$a8,$aa,$ff,$00,$00,$ff,$00,$14,$07,$ff,$38,$38,$38,$0f,$07
        .byte $07,$07,$fc,$6e,$dd,$6d,$1e,$1e,$6d,$1e,$1e,$00,$00,$00,$00,$a8
        .byte $80,$14,$00,$00,$00,$1c,$00,$00,$05,$05,$05,$14,$14,$45,$14,$14
        .byte $51,$14,$14,$05,$d4,$44,$44,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
laa08   ; line 2, right
        .byte $00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$c0,$00,$00,$00,$c0,$00
        .byte $00,$00,$00,$c0,$80,$80,$00,$00,$80,$00,$00,$00,$00,$00,$00,$80
        .byte $80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$40,$00,$40,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
laa70   ; line 3, left
        .byte $00,$a8,$aa,$c3,$00,$3f,$c3,$00,$04,$0e,$ff,$1c,$18,$18,$4e,$03
        .byte $06,$06,$1c,$3f,$7f,$6f,$1c,$0e,$3d,$0e,$dc,$00,$08,$00,$08,$a8
        .byte $a8,$94,$04,$00,$00,$18,$00,$08,$04,$04,$04,$04,$04,$45,$14,$04
        .byte $51,$14,$10,$44,$14,$15,$15,$00,$00,$00,$00,$55,$14,$55,$55,$51
        .byte $55,$55,$55,$15,$55,$2a,$a8,$aa,$a8,$aa,$aa,$aa,$82,$20,$08,$82
        .byte $80,$82,$82,$aa,$aa,$aa,$aa,$aa,$aa,$82,$a2,$82,$82,$8a,$aa,$80
        .byte $00,$28,$28,$02,$00,$02,$00,$00
laad8   ; line 3, right
        .byte $00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$c0,$00,$00,$00,$00,$00
        .byte $00,$00,$80,$80,$00,$00,$00,$c0,$80,$00,$00,$00,$00,$00,$00,$80
        .byte $80,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$40
        .byte $00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
lab40   ; line 4, left
        .byte $00,$00,$aa,$c3,$00,$0c,$03,$00,$15,$37,$ff,$3b,$1c,$5e,$7e,$0f
        .byte $0e,$1e,$1f,$06,$18,$3c,$78,$07,$0f,$07,$78,$00,$00,$00,$00,$00
        .byte $00,$00,$14,$00,$00,$7e,$00,$00,$15,$14,$15,$05,$15,$14,$50,$05
        .byte $14,$05,$50,$54,$15,$04,$04,$00,$00,$00,$00,$41,$14,$41,$41,$51
        .byte $40,$41,$05,$11,$41,$22,$88,$82,$82,$a0,$a0,$82,$82,$20,$08,$8a
        .byte $80,$a2,$82,$8a,$82,$8a,$82,$82,$20,$82,$a2,$82,$82,$8a,$82,$a0
        .byte $00,$28,$28,$02,$00,$0a,$00,$00
laba8   ; line 4, right
        .byte $00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$c0,$00,$00,$00,$00,$c0
        .byte $00,$80,$80,$00,$00,$00,$00,$80,$00,$80,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
lac10   ; line 5, left
        .byte $00,$8a,$aa,$c3,$00,$0c,$03,$ff,$44,$6c,$ff,$cd,$3e,$7b,$0e,$5b
        .byte $1f,$37,$1c,$06,$18,$18,$d8,$06,$06,$06,$18,$00,$00,$00,$80,$8a
        .byte $8a,$8a,$15,$04,$00,$5b,$00,$00,$44,$15,$45,$15,$14,$10,$50,$04
        .byte $04,$05,$10,$04,$14,$04,$04,$00,$00,$00,$00,$41,$04,$01,$01,$51
        .byte $40,$40,$05,$11,$41,$22,$88,$80,$82,$a0,$a0,$80,$82,$20,$08,$88
        .byte $80,$aa,$a2,$8a,$82,$8a,$82,$80,$20,$82,$a2,$82,$82,$8a,$82,$28
        .byte $00,$a0,$0a,$08,$aa,$28,$00,$00
lac78   ; line 5, right
        .byte $00,$80,$80,$00,$00,$00,$00,$00,$40,$c0,$c0,$80,$00,$00,$00,$40
        .byte $00,$80,$00,$00,$00,$00,$00,$00,$00,$c0,$00,$00,$80,$00,$00,$80
        .byte $80,$80,$00,$00,$00,$40,$00,$80,$40,$00,$00,$00,$40,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$80,$00,$00,$00
lace0   ; line 6, left
        .byte $00,$8a,$aa,$c3,$00,$0c,$03,$eb,$06,$0c,$ff,$0c,$de,$18,$0e,$43
        .byte $1e,$06,$1c,$1e,$1e,$18,$18,$06,$06,$06,$18,$08,$80,$88,$00,$8a
        .byte $8a,$8a,$00,$05,$00,$18,$08,$a0,$0c,$06,$06,$4c,$0c,$10,$10,$04
        .byte $04,$04,$10,$0e,$1c,$0f,$1e,$00,$80,$00,$00,$41,$04,$55,$15,$55
        .byte $55,$40,$05,$55,$55,$aa,$aa,$80,$82,$a8,$a8,$80,$aa,$28,$0a,$a8
        .byte $80,$aa,$aa,$82,$aa,$82,$aa,$aa,$20,$82,$a2,$82,$28,$aa,$08,$0a
        .byte $00,$a0,$0a,$08,$aa,$a0,$00,$00
lad48   ; line 6, right
        .byte $00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$c0,$00,$00,$00,$00,$00
        .byte $c0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80
        .byte $80,$80,$00,$00,$00,$40,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$80,$00,$00,$00
ladb0   ; line 7, left
        .byte $00,$8a,$aa,$ff,$00,$0c,$c3,$eb,$15,$0e,$ff,$1c,$38,$3c,$1b,$07
        .byte $07,$0f,$36,$36,$1b,$78,$38,$0e,$07,$07,$1c,$02,$20,$00,$00,$8a
        .byte $8a,$8a,$8a,$95,$04,$1c,$22,$40,$0e,$07,$0f,$1c,$1e,$78,$38,$0e
        .byte $0f,$0e,$38,$1b,$36,$0d,$36,$00,$80,$00,$14,$45,$04,$40,$01,$01
        .byte $05,$55,$14,$41,$05,$82,$82,$a0,$a2,$80,$80,$8a,$a2,$28,$0a,$aa
        .byte $a0,$82,$aa,$82,$a0,$82,$a8,$0a,$28,$a2,$a2,$aa,$28,$28,$28,$0a
        .byte $00,$a0,$0a,$20,$aa,$a0,$00,$00
lae18   ; line 7, right
        .byte $00,$80,$80,$00,$00,$00,$00,$00,$00,$00,$c0,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$40,$00,$80,$80,$80
        .byte $80,$80,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$80,$00,$00,$80,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$80,$00,$00,$00
lae80   ; line 8, left
        .byte $00,$8a,$aa,$c3,$00,$aa,$ff,$eb,$15,$7b,$ff,$37,$3c,$66,$1b,$0d
        .byte $0f,$19,$76,$36,$1b,$d8,$6c,$1b,$06,$0d,$36,$20,$0a,$80,$00,$8a
        .byte $8a,$8a,$8a,$80,$14,$36,$48,$4a,$7b,$0f,$19,$1e,$33,$d8,$6c,$1b
        .byte $0d,$1b,$6c,$1b,$76,$0d,$36,$80,$80,$14,$55,$45,$04,$40,$01,$01
        .byte $05,$45,$10,$41,$05,$82,$82,$a0,$a2,$80,$80,$8a,$a2,$28,$0a,$a2
        .byte $a0,$82,$8a,$82,$a0,$82,$a8,$0a,$28,$a2,$aa,$aa,$82,$28,$80,$28
        .byte $28,$a0,$0a,$20,$aa,$28,$00,$00
laee8   ; line 8, right
        .byte $00,$80,$80,$00,$00,$80,$00,$00,$40,$00,$c0,$80,$00,$00,$80,$80
        .byte $00,$80,$00,$00,$00,$00,$00,$00,$c0,$80,$00,$40,$40,$00,$00,$80
        .byte $80,$80,$80,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$00
        .byte $80,$00,$00,$80,$00,$80,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$80,$00,$00,$00
laf50   ; line 9, left
        .byte $00,$8a,$aa,$c3,$00,$aa,$c0,$ff,$10,$03,$ff,$30,$0e,$63,$18,$0d
        .byte $1c,$31,$06,$36,$1b,$d8,$6c,$1b,$06,$0d,$36,$09,$21,$20,$00,$8a
        .byte $8a,$8a,$8a,$8a,$15,$36,$50,$11,$03,$1c,$31,$07,$31,$d8,$6c,$1b
        .byte $0d,$1b,$6c,$18,$06,$0d,$36,$80,$80,$55,$55,$45,$15,$45,$41,$01
        .byte $05,$45,$10,$41,$05,$8a,$82,$a2,$a2,$80,$80,$82,$a2,$28,$8a,$a2
        .byte $a0,$82,$82,$82,$a0,$88,$a2,$8a,$28,$a2,$28,$a2,$82,$28,$8a,$a0
        .byte $28,$28,$28,$80,$00,$0a,$00,$00
lafb8   ; line 9, right
        .byte $00,$80,$80,$00,$00,$80,$00,$00,$00,$00,$c0,$00,$00,$00,$00,$80
        .byte $00,$80,$00,$00,$00,$00,$00,$00,$c0,$80,$00,$00,$40,$00,$00,$80
        .byte $80,$80,$80,$80,$00,$00,$80,$00,$00,$00,$80,$00,$80,$00,$00,$00
        .byte $80,$00,$00,$00,$00,$80,$00,$80,$80,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00
lb020   ; line 10, left
        .byte $00,$00,$00,$c3,$00,$00,$c0,$00,$10,$03,$ff,$30,$0c,$03,$38,$0d
        .byte $0c,$30,$07,$06,$18,$b0,$6c,$1b,$03,$0d,$36,$01,$09,$02,$00,$00
        .byte $00,$00,$00,$00,$00,$36,$10,$11,$03,$0c,$30,$06,$01,$b0,$0c,$1b
        .byte $06,$1b,$6c,$38,$07,$0c,$06,$00,$00,$00,$00,$55,$15,$55,$55,$01
        .byte $55,$55,$10,$55,$05,$8a,$aa,$aa,$a8,$aa,$80,$aa,$a2,$28,$aa,$a2
        .byte $aa,$82,$82,$aa,$a0,$a2,$a2,$aa,$28,$aa,$20,$82,$82,$28,$aa,$80
        .byte $28,$28,$28,$80,$00,$02,$00,$00
lb088   ; line 10, right
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$c0,$00,$00,$00,$00,$80
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$40,$80,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00
        .byte $80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00

lb0f0   ; (filler bytes?)
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

        .align $100

demoGameSequence
lb100
        ; byte 1 - move direction
        ;          high nibble le/ri: 1:left 3:right 4:dig right 5:dig left 6:none
        ;          low nibble up/do:  0:up, 2:down, 6:none
        ; byte 2 - move duration

        ; demo board 0

        .byte $16,$4c                   ; left
        .byte $66,$02                   ; no move
        .byte $55,$01                   ; dig left
        .byte $66,$02                   ; no move
        .byte $36,$18                   ; right
        .byte $55,$01                   ; dig left
        .byte $44,$01                   ; dig right
        .byte $66,$14                   ; no move
        .byte $36,$0d                   ; right
        .byte $30,$17                   ; right/up
        .byte $60,$08                   ; up
        .byte $66,$03                   ; no move
        .byte $16,$16                   ; left
        .byte $66,$04                   ; no move
        .byte $36,$23                   ; right
        .byte $32,$01                   ; right down
        .byte $62,$01                   ; down
        .byte $55,$01                   ; dig left
        .byte $66,$20                   ; no move
        .byte $16,$07                   ; left
        .byte $66,$02                   ; no move
        .byte $36,$25                   ; right
        .byte $30,$14                   ; right/up
        .byte $60,$0e                   ; up
        .byte $10,$11                   ; left/up
        .byte $16,$25                   ; left
        .byte $10,$08                   ; left/up
        .byte $16,$23                   ; left
        .byte $10,$06                   ; left/up
        .byte $60,$02                   ; up
        .byte $30,$0f                   ; right/up
        .byte $36,$17                   ; right
        .byte $66,$02                   ; no move
        .byte $16,$07                   ; left
        .byte $55,$01                   ; dig left
        .byte $66,$1e                   ; no move
        .byte $16,$38                   ; left
        .byte $44,$01                   ; dig right
        .byte $16,$05                   ; left
        .byte $44,$01                   ; dig right
        .byte $16,$07                   ; left
        .byte $44,$01                   ; dig right
        .byte $36,$07                   ; right
        .byte $55,$01                   ; dig left
        .byte $36,$04                   ; right
        .byte $55,$01                   ; dig left
        .byte $16,$03                   ; left
        .byte $55,$01                   ; dig left
        .byte $16,$03                   ; left
        .byte $36,$0b                   ; right
        .byte $55,$01                   ; dig left
        .byte $16,$03                   ; left
        .byte $36,$0e                   ; right
        .byte $44,$01                   ; dig right
        .byte $66,$01                   ; no move
        .byte $60,$0c                   ; up
        .byte $30,$29                   ; right/up
        .byte $60,$02                   ; up
        .byte $44,$01                   ; dig right
        .byte $16,$2b                   ; left
        .byte $10,$04                   ; left/up
        .byte $60,$05                   ; up
        .byte $30,$01                   ; right/up
        .byte $36,$67                   ; right
        .byte $32,$01                   ; right down
        .byte $44,$01                   ; dig right
        .byte $66,$2b                   ; no move
        .byte $36,$0c                   ; right
        .byte $30,$15                   ; right/up
        .byte $36,$12                   ; right
        .byte $55,$01                   ; dig left
        .byte $16,$03                   ; left
        .byte $55,$01                   ; dig left
        .byte $36,$05                   ; right
        .byte $55,$01                   ; dig left
        .byte $16,$03                   ; left
        .byte $36,$08                   ; right
        .byte $66,$02                   ; no move
        .byte $16,$4a                   ; left
        .byte $10,$04                   ; left/up
        .byte $60,$07                   ; up
        .byte $30,$09                   ; right/up
        .byte $36,$15                   ; right
        .byte $66,$0a                   ; no move
        .byte $16,$0d                   ; left
        .byte $44,$01                   ; dig right
        .byte $66,$02                   ; no move
        .byte $16,$04                   ; left
        .byte $44,$01                   ; dig right
        .byte $16,$02                   ; left
        .byte $44,$06                   ; dig right
        .byte $16,$04                   ; left
        .byte $44,$01                   ; dig right
        .byte $16,$02                   ; left
        .byte $62,$15                   ; down
        .byte $36,$31                   ; right
        .byte $66,$01                   ; no move
        .byte $62,$04                   ; down
        .byte $12,$06                   ; left/down
        .byte $44,$01                   ; dig right
        .byte $66,$37                   ; no move
        .byte $36,$01                   ; right
        .byte $30,$1d                   ; right/up
        .byte $60,$33                   ; up

        ; demo board 1

        .byte $36,$32                   ; right
        .byte $66,$03                   ; no move
        .byte $16,$01                   ; left
        .byte $10,$1b                   ; left/up
        .byte $60,$05                   ; up
        .byte $36,$28                   ; right
        .byte $44,$01                   ; dig right
        .byte $66,$1f                   ; no move
        .byte $36,$14                   ; right
        .byte $44,$01                   ; dig right
        .byte $55,$01                   ; dig left
        .byte $66,$2d                   ; no move
        .byte $36,$01                   ; right
        .byte $30,$12                   ; right/up
        .byte $60,$25                   ; up
        .byte $66,$01                   ; no move
        .byte $55,$01                   ; dig left
        .byte $16,$0d                   ; left
        .byte $66,$02                   ; no move
        .byte $36,$09                   ; right
        .byte $30,$0a                   ; right/up
        .byte $36,$04                   ; right
        .byte $44,$01                   ; dig right
        .byte $36,$03                   ; right
        .byte $44,$01                   ; dig right
        .byte $36,$03                   ; right
        .byte $16,$22                   ; left
        .byte $44,$01                   ; dig right
        .byte $16,$07                   ; left
        .byte $44,$04                   ; dig right
        .byte $16,$03                   ; left
        .byte $44,$01                   ; dig right
        .byte $16,$27                   ; left
        .byte $12,$0e                   ; left/down
        .byte $16,$1e                   ; left
        .byte $55,$01                   ; dig left
        .byte $66,$19                   ; no move
        .byte $36,$01                   ; right
        .byte $30,$03                   ; right/up
        .byte $60,$07                   ; up
        .byte $10,$1f                   ; left/up
        .byte $60,$07                   ; up
        .byte $30,$09                   ; right/up
        .byte $36,$33                   ; right
        .byte $66,$04                   ; no move
        .byte $10,$09                   ; left/up
        .byte $16,$08                   ; left
        .byte $12,$01                   ; left/down
        .byte $62,$0c                   ; down
        .byte $32,$01                   ; right down
        .byte $36,$32                   ; right
        .byte $44,$01                   ; dig right
        .byte $16,$0b                   ; left
        .byte $44,$01                   ; dig right
        .byte $16,$09                   ; left
        .byte $44,$01                   ; dig right
        .byte $10,$2c                   ; left/up
        .byte $60,$04                   ; up
        .byte $30,$03                   ; right/up
        .byte $36,$0a                   ; right
        .byte $44,$01                   ; dig right
        .byte $16,$05                   ; left
        .byte $44,$01                   ; dig right
        .byte $36,$03                   ; right
        .byte $44,$01                   ; dig right
        .byte $36,$03                   ; right
        .byte $44,$01                   ; dig right
        .byte $66,$03                   ; no move
        .byte $36,$03                   ; right
        .byte $55,$01                   ; dig left
        .byte $36,$08                   ; right
        .byte $55,$01                   ; dig left
        .byte $66,$4c                   ; no move
        .byte $16,$09                   ; left
        .byte $10,$15                   ; left/up
        .byte $44,$01                   ; dig right
        .byte $10,$2f                   ; left/up
        .byte $16,$09                   ; left
        .byte $12,$03                   ; left/down
        .byte $16,$12                   ; left
        .byte $66,$02                   ; no move
        .byte $36,$06                   ; right
        .byte $66,$2d                   ; no move
        .byte $55,$01                   ; dig left
        .byte $16,$03                   ; left
        .byte $10,$1c                   ; left/up
        .byte $55,$01                   ; dig left
        .byte $16,$03                   ; left
        .byte $44,$01                   ; dig right
        .byte $36,$03                   ; right
        .byte $32,$15                   ; right down
        .byte $36,$0b                   ; right
        .byte $30,$0b                   ; right/up
        .byte $60,$0c                   ; up
        .byte $44,$01                   ; dig right
        .byte $62,$0d                   ; down
        .byte $12,$02                   ; left/down
        .byte $16,$0d                   ; left
        .byte $44,$01                   ; dig right
        .byte $66,$20                   ; no move
        .byte $36,$04                   ; right
        .byte $30,$17                   ; right/up
        .byte $36,$1e                   ; right
        .byte $44,$01                   ; dig right
        .byte $36,$2f                   ; right
        .byte $30,$08                   ; right/up
        .byte $60,$03                   ; up
        .byte $10,$22                   ; left/up
        .byte $16,$1b                   ; left
        .byte $66,$26                   ; no move
        .byte $55,$07                   ; dig left
        .byte $16,$03                   ; left
        .byte $55,$01                   ; dig left
        .byte $66,$1d                   ; no move
        .byte $16,$02                   ; left
        .byte $10,$85                   ; left/up
        .byte $60,$02                   ; up
        .byte $30,$03                   ; right/up
        .byte $36,$03                   ; right
        .byte $32,$0f                   ; right down
        .byte $36,$03                   ; right
        .byte $30,$0c                   ; right/up

        ; demo board 2

        .byte $36,$20                   ; right
        .byte $66,$01                   ; no move
        .byte $16,$0a                   ; left
        .byte $60,$06                   ; up
        .byte $66,$02                   ; no move
        .byte $36,$08                   ; right
        .byte $30,$05                   ; right/up
        .byte $60,$02                   ; up
        .byte $66,$02                   ; no move
        .byte $16,$08                   ; left
        .byte $10,$01                   ; left/up
        .byte $60,$06                   ; up
        .byte $66,$01                   ; no move
        .byte $36,$08                   ; right
        .byte $30,$04                   ; right/up
        .byte $60,$03                   ; up
        .byte $66,$01                   ; no move
        .byte $16,$08                   ; left
        .byte $10,$02                   ; left/up
        .byte $60,$03                   ; up
        .byte $30,$01                   ; right/up
        .byte $36,$08                   ; right
        .byte $30,$03                   ; right/up
        .byte $60,$03                   ; up
        .byte $16,$09                   ; left
        .byte $10,$02                   ; left/up
        .byte $60,$03                   ; up
        .byte $30,$03                   ; right/up
        .byte $36,$07                   ; right
        .byte $30,$03                   ; right/up
        .byte $60,$02                   ; up
        .byte $10,$02                   ; left/up
        .byte $16,$08                   ; left
        .byte $10,$01                   ; left/up
        .byte $60,$02                   ; up
        .byte $30,$02                   ; right/up
        .byte $36,$0a                   ; right
        .byte $30,$02                   ; right/up
        .byte $60,$02                   ; up
        .byte $10,$03                   ; left/up
        .byte $16,$04                   ; left
        .byte $10,$03                   ; left/up
        .byte $60,$05                   ; up
        .byte $30,$02                   ; right/up
        .byte $36,$07                   ; right
        .byte $66,$16                   ; no move
        .byte $36,$02                   ; right
        .byte $66,$33                   ; no move
        .byte $55,$01                   ; dig left
        .byte $36,$05                   ; right
        .byte $55,$01                   ; dig left
        .byte $36,$04                   ; right
        .byte $55,$01                   ; dig left
        .byte $36,$03                   ; right
        .byte $55,$01                   ; dig left
        .byte $36,$03                   ; right
        .byte $55,$01                   ; dig left
        .byte $66,$a9                   ; no move
        .byte $62,$0c                   ; down
        .byte $66,$07                   ; no move
        .byte $60,$0f                   ; up
        .byte $55,$01                   ; dig left
        .byte $66,$18                   ; no move (last move in crt version)
        .byte $16,$2a                   ; left
        .byte $55,$01                   ; dig left
        .byte $16,$03                   ; left
        .byte $66,$01                   ; no move
        .byte $60,$07                   ; up
        .byte $66,$03                   ; no move
        .byte $36,$03                   ; right
        .byte $30,$1b                   ; right/up
        .byte $36,$08                   ; right
        .byte $44,$01                   ; dig right
        .byte $66,$18                   ; no move
        .byte $36,$0f                   ; right
        .byte $66,$08                   ; no move
        .byte $44,$01                   ; dig right
        .byte $66,$38                   ; no move
        .byte $30,$0e                   ; right/up
        .byte $66,$11                   ; no move
        .byte $60,$04                   ; up
        .byte $66,$49                   ; no move


lb368                                   ; garbage: part of demo board 2
        .byte                         $37,$03,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$33,$07,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$37,$03,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$33,$07,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$37,$03,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$33,$07,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$37,$03,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$09,$00,$33,$07,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$30,$11,$11,$11,$11,$11,$11,$11,$11,$03,$00,$30

        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00

lb400
.enc "high"

        .text "QU $1C35",$8d
        .text "ytable EQU $1C51",$8d
        .text "bytable EQU $1C62",$8d
        .text "bitable EQU $1C7E",$8d
        .text "xbytable EQU $1C9A",$8d
        .text "xbitable EQU $1D26",$8d
        .text "boot EQU $1DB2",$8d
        .text "scorebuf EQU $1F00",$8d
        .text "chardata EQU $AD00",$8d
        .text $8d
        .text "rwtsparm EQU $B7E8",$8d
        .text "rwtsvolm EQU $B7EB",$8d
        .text "rwtstrck EQU $B7EC",$8d
        .text "rwtssect EQU $B7ED",$8d
        .text "rwtsbuff EQU $B7F0",$8d
        .text "rwtscmn"

        .align $100

demoGameBoards
lb500
        ; demo board 0

        .byte $06,$00,$00,$07,$00,$00,$07,$00,$00,$06,$00,$00,$03,$06
        .byte $13,$11,$11,$11,$11,$11,$11,$11,$11,$03,$00,$00,$03,$06
        .byte $03,$00,$00,$00,$00,$00,$00,$07,$00,$43,$44,$44,$03,$76
        .byte $11,$11,$11,$11,$31,$11,$11,$11,$11,$01,$00,$00,$13,$11
        .byte $11,$11,$11,$11,$33,$00,$00,$00,$00,$00,$00,$00,$03,$00
        .byte $11,$11,$11,$31,$43,$44,$44,$44,$03,$00,$00,$00,$03,$00
        .byte $71,$70,$11,$33,$00,$00,$00,$08,$03,$70,$00,$08,$03,$70
        .byte $11,$11,$31,$03,$00,$00,$13,$11,$21,$22,$11,$11,$13,$11
        .byte $00,$00,$30,$00,$00,$00,$03,$00,$00,$00,$00,$00,$03,$00
        .byte $00,$00,$30,$00,$00,$00,$03,$00,$00,$00,$00,$00,$03,$00
        .byte $00,$00,$38,$00,$70,$00,$43,$44,$44,$44,$44,$44,$03,$70
        .byte $13,$11,$11,$11,$11,$11,$03,$70,$00,$00,$70,$00,$13,$11
        .byte $03,$00,$00,$00,$00,$00,$03,$11,$11,$11,$11,$01,$03,$00
        .byte $03,$00,$00,$00,$00,$00,$03,$00,$00,$00,$00,$00,$03,$00
        .byte $03,$00,$00,$70,$00,$00,$03,$00,$90,$70,$00,$00,$03,$00
        .byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11

        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00

lb600
        ; demo board 1

        .byte $11,$11,$61,$11,$11,$11,$11,$11,$11,$11,$11,$01,$11,$11
        .byte $06,$00,$61,$01,$44,$44,$44,$04,$00,$00,$00,$00,$00,$10
        .byte $16,$61,$60,$01,$71,$00,$10,$13,$00,$00,$00,$00,$80,$10
        .byte $06,$11,$11,$01,$11,$11,$11,$13,$11,$11,$07,$31,$11,$11
        .byte $61,$00,$00,$47,$44,$11,$11,$13,$11,$11,$11,$31,$01,$11
        .byte $11,$11,$11,$01,$00,$01,$10,$13,$11,$10,$11,$31,$01,$11
        .byte $00,$00,$00,$80,$00,$00,$10,$13,$11,$10,$01,$31,$71,$10
        .byte $13,$11,$11,$11,$13,$73,$10,$13,$11,$10,$01,$31,$11,$11
        .byte $03,$00,$00,$00,$13,$11,$11,$13,$11,$07,$01,$31,$11,$10
        .byte $03,$00,$00,$00,$00,$11,$11,$13,$11,$11,$01,$31,$11,$17
        .byte $03,$00,$00,$00,$01,$00,$00,$03,$00,$00,$00,$30,$00,$10
        .byte $13,$11,$13,$11,$31,$21,$21,$21,$21,$21,$21,$31,$21,$21
        .byte $13,$11,$13,$11,$31,$11,$11,$11,$00,$11,$11,$31,$21,$21
        .byte $03,$00,$03,$00,$30,$11,$11,$11,$01,$10,$11,$31,$00,$00
        .byte $13,$11,$11,$11,$31,$11,$11,$11,$11,$07,$11,$11,$11,$31
        .byte $93,$00,$00,$00,$30,$70,$10,$11,$11,$01,$80,$00,$00,$30

        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00

lb700
        ; demo board 2

        .byte $00,$00,$00,$30,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $80,$07,$00,$30,$00,$00,$00,$00,$00,$00,$00,$00,$70,$08
        .byte $13,$81,$07,$30,$00,$00,$00,$00,$00,$00,$00,$70,$18,$31
        .byte $03,$10,$01,$30,$11,$11,$36,$11,$11,$31,$00,$10,$01,$30
        .byte $03,$00,$00,$00,$00,$00,$33,$07,$00,$30,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$37,$03,$00,$30,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$33,$07,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$37,$03,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$33,$07,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$37,$03,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$33,$07,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$37,$03,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$33,$07,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$00,$00,$37,$03,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$00,$00,$09,$00,$33,$07,$00,$00,$00,$00,$00,$30
        .byte $03,$00,$30,$11,$11,$11,$11,$11,$11,$11,$11,$03,$00,$30

        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00

