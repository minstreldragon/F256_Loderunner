
exitLaddersX                            ; $1200
notAllExitLaddersDrawn = exitLaddersX   ; 0: all exit ladders drawn, 0xff: couldn't draw one or more ladder segments
        .fill $30

exitLaddersY                            ; $1230
        .fill $30

enemiesX                                ; $1260
        .fill $08

enemiesY                                ; $1268
        .fill $08

enemiesActionCtr                        ; special action counter: ($1270)
        .fill $08                       ; <0: remaining time until enemy drops gold (increasing)
                                        ; >0: remaining time enemy is trapped (decreasing)

enemiesStepX                            ; $1278
        .fill $08

enemiesStepY                            ; $1280
        .fill $08

enemiesAnimPhase                        ; enemy animation phase
        .fill $08                       ; $1288

enemiesOrientation                      ; $ff: enemy facing left, $01: facing right
        .fill $08                       ; $1290

enemiesRespawnCtr                       ; time left until enemy respawns
        .fill $08                       ; $1298

holesPosX
        .fill $20                       ; $12a0

holesPosY
        .fill $20                       ; $12c0

holesOpenCtr                            ; array: time until hole closes
        .fill $20                       ; $12c0

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


        .align $100

boardAction                             ; current board with replacments for player, enemies, holes
        .fill $200                      ; $0800

boardLayout                             ; current board w/o replacments for player, enemies, holes
        .fill $200                      ; $0a00

spriteDefinitions                       ; sprite definitions for player and enemies
        .fill $400                      ; $0c00

boardPacked                             ; current board, packed by combining two shapes in one byte
        .fill $100                      ; $1000

highScoreData                           ; high score data (disk only)
        .fill $100                      ; $1100


