; Source Files may overrides the following definitions

.include "common/api.asm"
.include "common/f256jr.asm"
.include "common/f256_tiles.asm"
.include "common/f256_sprites.asm"
.include "common/f256_rtc.asm"
.include "common/f256_via.asm"
.include "common/f256_irq.asm"

CRACK :?= true
BUGFIX :?= true
STRIP_UNUSED :?= true

TIMER_EVENT_MAX = 1

; The music timer interval is calculated from:
; 6522 VIA clock (Apple II):  1.023 MHz
; 6522 VIA Clock (Foenix):   25.125 MHz / 4
; Timer interval (Apple II):  16666 ($411a)
; Timer interval (Foenix) = (VIA clock Foenix / VIA clock Apple) * Timer interval Apple
; Since timer interval is larger than 16 bit value, half the interval is used
MUSIC_TIMER_INTERVAL = 51165


; architecture specific constants

KEY_ENTER = $8d
;KEY_NORTH = $c0                 ; '@' (North)
;KEY_SOUTH = $af                 ; '/' (South)
;KEY_EAST = $bb                  ; ';' (East)
;KEY_WEST = $ba                  ; ':' (West)
KEY_NORTH = $b8                 ; '@' (North) F256Jr
KEY_NORTH_ALT = $90             ; '@' (North) F256K
KEY_SOUTH = $b2                 ; '/' (South) F256Jr
KEY_SOUTH_ALT = $8e             ; '/' (South) F256K
KEY_EAST = $b6                  ; ';' (East)  F256Jr
KEY_EAST_ALT = $86              ; ';' (East)  F256K
KEY_WEST = $b4                  ; ':' (West)  F256Jr
KEY_WEST_ALT = $82              ; ':' (West)  F256K
KEY_RETURN = $8d                ; return key
;KEY_BACKSPACE = $ab             ; Key combination: C= + Q
KEY_BACKSPACE = $88             ; Key combination: C= + Q
KEY_ESCAPE = $df                ; RUN/STOP key
KEY_ARROW_UP = KEY_NORTH

spritesF256 = $a000

.comment
; kernel structures

event   .dstruct    kernel.event.event_t

; zero page registers

zpSrc   .word ?                         ; a pointer to data to read
zpDst   .word ?                         ; a pointer to data to write


zpTargetTileLon                 ; $02
        .byte ?

zpDstPtr                        ; intro
        .word ?

zpTargetTileLat                 ; $03
        .byte ?
zpBufferPtrCopy                 ; $04
zpTargetTileLonDelta
zpDx
        .byte ?
zpTargetTileLatDelta            ; $05
zpDy
        .byte ?
zpColMemPtrIntro                ; $06 ; intro
zpBmpDstPtr                     ; intro
        .word ?
zpTileCenter                    ; $09
        .byte ?
zpTileNorth                     ; $0a
        .byte ?
zpTileSouth                     ; $0b
        .byte ?
zpTileEast                      ; $0c
        .byte ?
zpTileWest                      ; $0d
        .byte ?
zpPlayerTile                    ; $0e
        .byte ?

zpWrapTile                      ; $0f
        .byte ?
zpDuration                      ; $15
zpSwap
        .byte ?
zpPitch                         ; $16
        .byte ?
zpVoicePtr                      ; $1a
        .word ?                 ; voice 0
        .word ?                 ; voice 1
        .word ?                 ; voice 2
zpVolume                        ; $20
        .byte ?
zpWindDir                       ; $21
zpPlaylistPos
        .byte ?
zpMainLoopTimer                 ; $22
        .byte ?
zpDungeonLevel                  ; $23
        .byte ?
zpDungeonHeading                ; $24
        .byte ?
zpDunTilePos                    ; $25 ; dungeon tile position relative to party
        .byte ?

zpXStart                        ; $26
zpX1
        .byte ?

zpYStart                        ; $27
zpX0
        .byte ?

zpYEnd                          ; $28
zpY0
        .byte ?

zpXDraw                                 ; F256 specific
        .byte ?

zpYDraw                                 ; F256 specific
        .byte ?

zpXInc                          ; $29
zpY1
zpXEnd
        .byte ?

zpSlopeCeil                     ; $2a
zpXi
        .byte ?

zpSlopeCeilCtr                  ; $2b
zpYi
        .byte ?

zpSlopeFloor                    ; $2c
zpLineIter
        .byte ?

zpSlopeFloorCtr                 ; $2d
zpLineDiff
        .byte ?

zpXWidth                        ; $2e
zpCoordsOffset
        .byte ?

zpProjectile                    ; $2f
zpLadderUpDown
zpBmpByte
        .byte ?

zpDirectoryPtr                  ; $33/$34 ; directory pointer
zpColorMemPtr2
zpColor                         ; color used for wall drawing in dungeon (F256)
        .word ?

zpBufferPtr                     ; $35/$36 ; read / write buffer
        .word ?

zpNegateTimeTimer               ; $4b
        .byte ?

zpTorchTimer                    ; $4c
        .byte ?

zpCombatNpcIndex                ; $4d
zpTimePassedFlag
zpIndicatePlrCtr
        .byte ?

zpCombatNpcTile                 ; $4e
zpBribeNpcIndex
zpNpcIndex2
        .byte ?

zpMonIndex                      ; $50
zpOffering
zpPrice
zpSpellMP
zpFoodRation
zpDamage
zpInputBcd
zpNpcPlrShortestDist
zpHealAmount
zpMarkMask
zpExodusHitCtr
zpPaid
zpInputAmount
zpItem
zpDemoTimer
        .byte ?

zpPartyMember1                  ; $51
zpDngInputCtr1
zpSumGoldHb
zpPriceHb
zpAbsDistLon
zpAbsDistTotal
zpDigit1e1
zpAttributeMax
        .byte ?

zpSumGoldLb                     ; $52
zpPriceLb
zpPartyMember2
zpDngInputCtr2
zpAbsDistLat
zpDigit1e0
        .byte ?

zpDexterity                     ; $53
zpInputBinary                   ; $53
        .byte ?

zpXpEarned                      ; $54
zpCannonRange
zpSndBuzzDuration
zpAttrPointsLeft
zpNameIdx
        .byte ?

zpPartyMemberId                 ; $55
        .byte ?

zpStoredPartyMbrId              ; $56
zpRosterEntryBcd
        .byte ?

zpIndex                         ; $57
zpSpellId
zpWeaponId
zpArmourId
zpNameIndex
        .byte ?

zpMusicRequest                  ; $58
        .byte ?
zpMusicSequel                   ; $59
        .byte ?
zpMusicCurrent                  ; $5a
        .byte ?
zpMusicDataPtr                  ; $5b
        .word ?
zpPartyStats                    ; $60
zpSurfacePlayerTile
        .byte ?
zpPartySize                     ; $61
        .byte ?
zpEnvironment                   ; $62 ; 0 = outdoors, 1 = dungeon, 2 = shrine
        .byte ?
zpSurfaceLon                    ; $63
        .byte ?
zpSurfaceLat                    ; $64
        .byte ?
zpPartyFormed                   ; $65
        .byte ?
zpPartySlot0                    ; $66
        .byte ?
zpPartySlot1                    ; $67
        .byte ?
zpPartySlot2                    ; $68
        .byte ?
zpPartySlot3                    ; $69
        .byte ?

zpMoves1e6                      ; $6a
        .byte ?
zpMoves1e4                      ; $6b
        .byte ?
zpMoves1e2                      ; $6c
        .byte ?
zpMoves1e0                      ; $6d
        .byte ?

zpMultiplier                    ; $70
zpAddParam
zpCardMask
zpItemId
zpAmountDelta
zpChar
zpHalfNameLen
zpPrice1
zpAmount2
zpSoundEffect
zpMapLon
        .byte ?

zpNpcTargetTileLon              ; $71
zpCardId
zpScreenX
zpDestLine
        .byte ?


zpNpcTargetTileLat              ; $72
zpInventoryId
zpScreenY
zpStatsId
        .byte ?

zpAmount                        ; $73
zpInvertLineCnt
zpWhirlpoolPitch
zpCheckMoveTile
zpRange
zpSndCntr
zpSndBasePitch
zpByteCntr
zpUpdateTilePos
        .byte ?

zpBitmapLine                    ; $74
        .byte ?

zpTargetDistLon                 ; $75
zpMapWinLon                     ; $75
zpPixelX                        ; $75
        .byte ?

zpTargetDistLat                 ; $76
zpMapWinLat                     ; $76
zpPixelY                        ; $76
        .byte ?

zpNpcTargetLon                  ; $77
zpMapOffLon                     ; $77
zpMapPixelX                     ; $77
        .byte ?

zpNpcTargetLat                  ; $78
zpMapOffLat                     ; $78
zpMapPixelY                     ; $78
        .byte ?

zpCursorCol                     ; $79
        .byte ?
zpCursorRow                     ; $7a
        .byte ?

zpNpcIndex                      ; $7b
zpInvertLineNum
zpBooty
zpNpcPathLen
zpPartyMember3
zpTrapType
zpInventoryId2
zpNpcFireRange
zpEnteredLocation
zpSoundLen
zpPathProbePos
        .byte ?

zpBmpPtr1                       ; $7c
zpPartyMemPtr2
zpSrcBmpPtr
zpPlayerPtr
zpAnimTilePtr
zpCoordsPtr
zpMapPtr2
zpTileMapPtr
        .word ?

zpStringPtr                     ; $7e/$7f
zpMapPtr
zpMapWinPtr
zpPartyMemberPtr
zpJmpPtr
zpBmpPtr
zpDataPtr
zpColMemPtr
zpColorMemPtr
zpTilePtr
zpRosterPtr
        .word ?

zpSrcPtr                        ; $f0/$f1
        .word ?

zpLongitude                     ; $f7
        .byte ?

zpLatitude                      ; $f8
        .byte ?

zpDataPtrDos
        .word ?

zpFileSize                      ; File size (to save)
        .word ?

.endcomment

; system variables

IRQVEC = VIRQ
USRCMD = $032e                  ; User-Defined Vector

; system registers

SidBase = SID_MONO
SidVoice1FreqLb = SidBase+$00
SidVoice1FreqHb = SidBase+$01
SidVoice1PulseWidthLb = SidBase+$02
SidVoice1PulseWidthHb = SidBase+$03
SidVoice1CtrlReg = SidBase+$04
SidVoice1AttackDecay = SidBase+$05
SidVoice1SustainRelease = SidBase+$06
SidVoice2FreqLb = SidBase+$07
SidVoice2FreqHb = SidBase+$08
SidVoice2PulseWidthLb = SidBase+$09
SidVoice2PulseWidthHb = SidBase+$0a
SidVoice2CtrlReg = SidBase+$0b
SidVoice2AttackDecay = SidBase+$0c
SidVoice2SustainRelease = SidBase+$0d
SidVoice3FreqLb = SidBase+$0e
SidVoice3FreqHb = SidBase+$0f
SidVoice3PulseWidthLb = SidBase+$10
SidVoice3PulseWidthHb = SidBase+$11
SidVoice3CtrlReg = SidBase+$12
SidVoice3AttackDecay = SidBase+$13
SidVoice3SustainRelease = SidBase+$14
SidFilterCutoffLb = SidBase+$15
SidFilterCutoffHb = SidBase+$16
SidFilterCtrlReg = SidBase+$17
SidFilterModeVolume = SidBase+$18
SidAdPaddle1 = SidBase+$19
SidAdPaddle2 = SidBase+$1a
SidVoice3Output = SidBase+$1b
SidVoice3AdsrOutput = SidBase+$1c

customIsrAddr = $fe00
