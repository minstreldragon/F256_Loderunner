
initF256                                ; initialize F256

        ; init events
        lda #<event
        sta kernel.args.events+0
        lda #>event
        sta kernel.args.events+1

        ; setup tile graphics

        lda #$00                        ; Set the I/O page to #1
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)

        lda #$2c                        ; enable Sprite, Graphics, Bitmap engines. Disable Text
        sta VKY_MSTR_CTRL_0             ; save that to VICKY master control register 0
        lda #$01                        ; set CLK70, for 320x200 display size
        sta VKY_MSTR_CTRL_1             ; save that to VICKY master control register 0

        lda #$10                        ; Layer 0: Bitmap 0, Layer 1: unused
        sta VKY_LAYER_CTRL_0
        lda #$01                        ; Layer 2: unused
        sta VKY_LAYER_CTRL_1

        lda #$01
        sta VKY_BRDR_CTRL               ; enable border
        lda #$00
        sta VKY_BRDR_COL_B              ; border color: black
        sta VKY_BRDR_COL_G
        sta VKY_BRDR_COL_R
        sta VKY_BRDR_HORI               ; border width = 0
        sta VKY_BRDR_VERT               ; border height = 0

        sta VKY_BKG_COL_B               ; background color: black
        sta VKY_BKG_COL_G
        sta VKY_BKG_COL_R

        ; setup Bitmap 0

        lda #$01
        sta VKY_BM0_CTRL                ; enable bitmap 0
        sta VKY_BM1_CTRL                ; enable bitmap 1
        lda #$00
        sta VKY_BM2_CTRL                ; disable bitmap 2


        lda #$00                        ; set bitmap pointers
        sta VKY_BM0_ADDR_L
        sta VKY_BM1_ADDR_L
        sta VKY_BM0_ADDR_M
        sta VKY_BM1_ADDR_M
        lda #$01
        sta VKY_BM1_ADDR_H              ; displayed bitmap 1: $010000
        lda #$02
        sta VKY_BM0_ADDR_H              ; displayed bitmap 0: $020000

.comment
        ; set tile set #0

        lda #$00                        ; displayed tileset at $010000
        sta $d283                       ; tile format: vertical
        sta VKY_TS0_ADDR_L
        sta VKY_TS0_ADDR_M
        lda #$01
        sta VKY_TS0_ADDR_H

        ; set tile set #1

        lda #$00                        ; buffer tileset at $020000
        sta $d287                       ; tile format: vertical
        sta VKY_TS1_ADDR_L
        sta VKY_TS1_ADDR_M
        lda #$02
        sta VKY_TS1_ADDR_H

        ; set up tile map #0 - 16x16 tiles (foreground)

        lda #$01                        ; 16x16 tiles, enable
        sta VKY_TM0_CTRL
        lda #22                         ; 2 tiles wider than screen allows scrolling
        sta VKY_TM0_SIZE_X
        lda #14                         ; 1.5 tiles higher than screen allows scrolling
        sta VKY_TM0_SIZE_Y

        lda #$00                        ; no scrolling
        sta VKY_TM0_POS_X_L             ; (first tile column in left border)
        sta VKY_TM0_POS_X_H
        sta VKY_TM0_POS_Y_L             ; (first tile row in upper border)
        sta VKY_TM0_POS_Y_H

        lda #<tile_map_tile_window
        sta VKY_TM0_ADDR_L
        lda #>tile_map_tile_window
        sta VKY_TM0_ADDR_M
        lda #$00
        sta VKY_TM0_ADDR_H
.endcomment

        jsr setColorLut                 ; setup color LUT
;        jsr clearBm0
        lda #$00                        ; fill value: black (transparent)
        sta fillValue
        jsr clearBitmap0F256
        lda #$00                        ; fill value: black (transparent)
        sta fillValue
        jsr clearBitmap1F256

        jsr initSwapAreaC000            ; prepare $c000 for being uses as swap area
        jsr initSpritesF256
        jsr testSprites                 ; EXPERIMENTAL: test sprites

.enc "high"
.comment
        lda #$05
        sta zpCursorRow
        lda #$00
        sta zpCursorCol
        jsr printString
        .text "LODE RUNNER",$00

        lda #$07
        sta zpCursorRow
        lda #$01
        sta zpCursorCol
        jsr printString
        .text "LODE RUNNER",$00

        lda #$00
        sta zpCursorRow
        lda #$00
        sta zpCursorCol
        lda #$01
        jsr replaceTileBm0

        lda #$01
        sta zpCursorRow
        lda #$00
        sta zpCursorCol
        lda #$02
        jsr replaceTileBm0

        lda #$02
        sta zpCursorRow
        lda #$00
        sta zpCursorCol
        lda #$03
        jsr replaceTileBm0

        lda #$03
        sta zpCursorRow
        lda #$00
        sta zpCursorCol
        lda #$04
        jsr replaceTileBm0
.endcomment

        jsr setupLodeRunnerIrq

        rts


handleEvents
        ; Peek at the event queue to see if anything is pending
        lda kernel.args.events.pending      ; negated count
        bpl _done

        ; get the next event
        jsr kernel.NextEvent
        bcs _done

        ; handle the event
        jsr dispatchEvent
        jmp handleEvents                    ; continue until the queue is drained

_done
        rts

dispatchEvent
        ; get the event's type
        lda event.type
        ldy #$ff
_dispatchEventL
        iny
        lda tblEventKinds,y
        beq _exit                       ; ignore anything not handled
        cmp event.type                  ; matches current event type?
        bne _dispatchEventL
        tya                             ; call the appropriate handler
        asl
        tay
        lda tblEventHandlers+1,y
        pha
        lda tblEventHandlers+0,y
        pha
_exit
        rts                             ; dispatch event or exit


event_timer
        jmp event_timer_handler

event_joystick
        lda event.joystick.joy1         ; joystick 0 value
        eor #$ff                        ; convert to C64 convention
        sta joystickCode
        rts

event_key_pressed
;        jsr printKbdEvent               ; TODO EXPERIMENTAL TESTING
        lda event.key.ascii
        bne _key_pressed_j1
        bit event.key.flags             ; modifier event?
        bpl _exit                       ; not a modifier
        lda event.key.raw
        cmp #$02
        bcc _exit
        cmp #$03+1
        bcs _exit
        lda #$80
        sta ctrl_active
        jmp _exit

_key_pressed_j1
        cmp #$60                        ; upper case?
        bcc _key_pressed_j2             ; yes ->
        sec
        sbc #$20                        ; to upper case
_key_pressed_j2
        bit ctrl_active                 ; <CTRL> active?
        bpl _key_pressed_j3             ; no ->
        clc
        adc #KEY_CODE_CTRL+KEY_CODE_A-1 ; convert to <CTRL>-letter for game
_key_pressed_j3


.comment
        ora #$80                        ; Ultima III expects uppermost bit set
        cmp #KEY_NORTH_ALT
        beq _key_pressed_north
        cmp #KEY_SOUTH_ALT
        beq _key_pressed_south
        cmp #KEY_EAST_ALT
        beq _key_pressed_east
        cmp #KEY_WEST_ALT
        beq _key_pressed_west
        bne _key_pressed_j2
_key_pressed_north
        lda #KEY_NORTH
        bne _key_pressed_j2
_key_pressed_south
        lda #KEY_SOUTH
        bne _key_pressed_j2
_key_pressed_east
        lda #KEY_EAST
        bne _key_pressed_j2
_key_pressed_west
        lda #KEY_WEST
        bne _key_pressed_j2

_key_pressed_j2
.endcomment
        sta keyboardCode
        ; TODO: do something with key code
_exit
        rts

event_key_released
        lda event.key.ascii
        bne _exit
        bit event.key.flags             ; modifier event?
        bpl _exit                       ; not a modifier
        lda event.key.raw
        cmp #$02
        bcc _exit
        cmp #$03+1
        bcs _exit
        lda #$00
        sta ctrl_active
_exit
        rts

event_file_opened
        inc file_opened
        rts

event_file_closed
        inc file_closed
        rts

event_file_not_found
;        jsr print
;        .text "File not found!",$ff,$00
_error_loop
        jmp _error_loop

event_file_data
        lda event.file.data.read
        sta kernel.args.buflen
        pha
        cmp #250
        beq _event_file_data_j1
;        jsr print
;        .text "End of File!",$ff,$00

_event_file_data_j1
        lda zpBufferPtr+0
        sta kernel.args.buf+0
        lda zpBufferPtr+1
        sta kernel.args.buf+1

        jsr kernel.ReadData

        pla
        clc
        adc zpBufferPtr+0
        sta zpBufferPtr+0
        bcc _file_data_j1
        inc zpBufferPtr+1
_file_data_j1
        inc file_next_chunk
;;;        jmp handleEvents        ; retrieve EOF event if EOF has been reached
        rts

event_file_eof
        inc file_eof
;        jsr print
;        .text "EOF!",$ff,$00
        rts


event_file_wrote
        sec
        lda zpFileSize+0                ; decrease remaining file size
        sbc event.file.wrote.wrote      ; by number of bytes written
        sta zpFileSize+0
        bcs _event_file_wrote_j1
        dec zpFileSize+1
_event_file_wrote_j1
        clc
        lda zpBufferPtr+0               ; increase source buffer pointer
        adc event.file.wrote.wrote      ; by number of bytes written
        sta zpBufferPtr+0
        bcc _event_file_wrote_j2
        inc zpBufferPtr+1
_event_file_wrote_j2
        inc file_next_chunk             ; indicate that chunk has been written
        rts


KBD
        .byte $00

ctrl_active
        .byte $00

file_opened
        .byte $00
file_closed
        .byte $00
file_eof
        .byte $00
file_next_chunk
        .byte $00

timer_events
        .fill TIMER_EVENT_MAX+1,$00

tblEventKinds
        .byte kernel.event.JOYSTICK
        .byte kernel.event.key.PRESSED
        .byte kernel.event.key.RELEASED   ; TODO ONLY FOR TESTING
        .byte kernel.event.file.NOT_FOUND
        .byte kernel.event.file.ERROR
        .byte kernel.event.file.OPENED
        .byte kernel.event.file.DATA
        .byte kernel.event.file.EOF
        .byte kernel.event.file.CLOSED
        .byte kernel.event.file.WROTE
        .byte kernel.event.timer.EXPIRED
        .byte $00

tblEventHandlers
        .word event_joystick-1
        .word event_key_pressed-1
        .word event_key_released-1
        .word event_file_not_found-1
        .word event_file_not_found-1
        .word event_file_opened-1
        .word event_file_data-1
        .word event_file_eof-1
        .word event_file_closed-1
        .word event_file_wrote-1
        .word event_timer-1

setColorLut
        lda #$01                        ; Set the I/O page to #1
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)

        ; main clut (and player sprites)

        lda #<clut_start-1
        sta zpSrc+0
        lda #>clut_start-1
        sta zpSrc+1

        lda #<VKY_GR_CLUT_0-1
        sta zpDst+0
        lda #>VKY_GR_CLUT_0-1
        sta zpDst+1

        ldy #<clut_end - clut_start
        jsr SmallMemCopy

        ; clut enemy (enemy sprites)

        lda #<clut_enemy-1
        sta zpSrc+0
        lda #>clut_enemy-1
        sta zpSrc+1

        lda #<VKY_GR_CLUT_1-1
        sta zpDst+0
        lda #>VKY_GR_CLUT_1-1
        sta zpDst+1

        ldy #<clut_enemy_end - clut_enemy
        jsr SmallMemCopy

        lda #$00                        ; Set the I/O page to #0
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)

        rts


SmallMemCopy
        lda (zpSrc),y
        sta (zpDst),y
        dey
        bne SmallMemCopy
        rts


clut_start
        ; order: blue, green, red, (reserved)
        .byte $00,$00,$00,$00    ; transparent
        .byte $ff,$ff,$ff,$00    ; white
        .byte $27,$23,$6d,$00    ; red
        .byte $f8,$fe,$a0,$00    ; cyan
        .byte $c4,$62,$ba,$00    ; purple
        .byte $4d,$ac,$56,$00    ; green
        .byte $9b,$2c,$2e,$00    ; blue
        .byte $71,$f1,$ed,$00    ; yellow
        .byte $4d,$78,$ba,$00    ; orange
        .byte $00,$38,$55,$00    ; brown
        .byte $71,$6c,$c4,$00    ; light red
        .byte $37,$37,$37,$00    ; dark grey
        .byte $86,$86,$86,$00    ; mid grey
        .byte $9f,$ff,$a9,$00    ; light green
        .byte $f8,$77,$7a,$00    ; light blue
        .byte $e0,$e0,$e0,$00    ; light grey
        .byte $00,$00,$00,$00    ; black (solid)
clut_end

clut_enemy
        ; order: blue, green, red, (reserved)
        .byte $00,$00,$00,$00    ; transparent
        .byte $f8,$fe,$a0,$00    ; cyan
clut_enemy_end


clearBm0
        ; loop over eight banks $010000 - $01ffff
        ; Fill the whole area with #$00

        lda #$10                ; start at phys. memory $010000
        sta _physBank
_clearBmTilesetL1
        jsr setSwapAreaA000
        lda #$a0                ; start filling at logical $a000
        sta _dest+2             ; high byte of destination
        ldx #$20                ; for all banks but the last fill $20 pages
        lda _physBank
        cmp #$1e
        bne _notLastBank
        ldx #$1a                ; for the last bank, only need to fill $1a pages
_notLastBank
        lda #$00                ; fill value: $00
_clearBmTilesetL2
;        txa                     ; TODO: TESTING ONLY
;        and #$0f                ; TODO: TESTING ONLY fill tile with color
        ldy #$00
_dest
        sta $a000,y
        iny
        bne _dest

        inc _dest+2             ; inc high byte of destination
        dex
        bne _clearBmTilesetL2

        inc _physBank
        inc _physBank
        lda _physBank
        cmp #$20
        bne _clearBmTilesetL1

        jsr resetSwapAreaA000   ; restore swap area at $a000
        rts

_physBank
        .byte $00

clearBitmap0F256
        lda #$10
        jmp clearBitmap

clearBitmap1F256
        lda #$20
        jmp clearBitmap

clearBitmap
        ; loop over eight banks $010000 - $01ffff
        ; Fill the whole area with #$00

;;;        lda #$10                ; start at phys. memory $010000
        sta _physBank                   ; parameter: physical memory area ($10 / $20)
_clearBmTilesetL1
        lda _physBank
        jsr setSwapAreaA000
        lda #$a0                ; start filling at logical $a000
        sta _dest+2             ; high byte of destination
        ldx #$20                ; for all banks but the last fill $20 pages
        lda _physBank
        and #$0f
        cmp #$0e
        bne _notLastBank
        ldx #$1a                ; for the last bank, only need to fill $1a pages
_notLastBank
;;;        lda #$00                ; fill value: $00
        lda fillValue           ; fill value: $00
_clearBmTilesetL2
;        txa                     ; TODO: TESTING ONLY
;        and #$0f                ; TODO: TESTING ONLY fill tile with color
        ldy #$00
_dest
        sta $a000,y
        iny
        bne _dest

        inc _dest+2             ; inc high byte of destination
        dex
        bne _clearBmTilesetL2

        inc _physBank
        inc _physBank
        lda _physBank
        and #$0f
;;;        cmp #$20
        bne _clearBmTilesetL1

        jsr resetSwapAreaA000   ; restore swap area at $a000
        rts

_physBank
        .byte $00

fillValue
        .byte $00

drawDivider
        lda #0
        sta zpCursorCol
        lda #BOARD_HEIGHT
        sta zpCursorRow
        jsr getBitmapPtrF256
        lda #$04
        sta zpShapeRowIter              ; iterator over rows in shape
_drawDividerL1
        ldy #$00
        lda #COL_RED
_drawDividerL2
        sta (zpBmpPtr),y
        iny
        bne _drawDividerL2

        inc zpBmpPtr+1
_drawDividerL3
        sta (zpBmpPtr),y
        iny
        cpy #24
        bne _drawDividerL3
        dec zpBmpPtr+1

        lda zpBmpPtr+0
        clc
        adc #<320
        sta zpBmpPtr+0
        lda zpBmpPtr+1
        adc #>320
        sta zpBmpPtr+1
        dec zpShapeRowIter
        bne _drawDividerL1

        jsr resetBigSwapArea
        rts


getBitmapPtrF256
        ldx zpCursorCol                 ; calculate x-offset in bitmap
        lda pixelPosByBoardCol,x
        asl
        sta zpBmpPtr+0
        lda #$00
        rol
        sta zpBmpPtr+1

        lda zpCursorRow
        asl
        tay
        lda TblBmpLinePtr+0,y           ; Bitmap start position (from Row)
        clc
        adc zpBmpPtr+0
        sta zpBmpPtr+0
        lda TblBmpLinePtr+1,y
        adc zpBmpPtr+1
        ora #$a0
        sta zpBmpPtr+1

        ldy zpCursorRow
        lda TblBmpLinePage,y
        asl
        clc
        adc #$10
        jsr setBigSwapArea
        rts

replaceTileBm0
        asl                             ; shape id as 16 bit pointer
        tay
        lda TblShapePtr+0,y             ; low byte of shape definition
        sta _copyShapeToBitmapL2+1      ; self modify code (speed)
        lda TblShapePtr+1,y             ; high byte of shape definition
        sta _copyShapeToBitmapL2+2      ; self modify code (speed)

        jsr getBitmapPtrF256

        lda #SHAPE_HEIGHT
        sta zpShapeRowIter              ; iterator over rows in shape
        ldx #$00                        ; index into shifted shape hopper
_copyShapeToBitmapL1
        ldy #$00
_copyShapeToBitmapL2
        lda $c0de,X                     ; self-modified source pointer
        sta (zpBmpPtr),y
        inx
        iny
        cpy #SHAPE_WIDTH
        bne _copyShapeToBitmapL2

        lda zpBmpPtr+0
        clc
        adc #<320
        sta zpBmpPtr+0
        lda zpBmpPtr+1
        adc #>320
        sta zpBmpPtr+1
        dec zpShapeRowIter
        bne _copyShapeToBitmapL1

        jsr resetBigSwapArea
        rts

TblShapePtr
        .word range(NUM_SHAPES) * SHAPE_WIDTH * SHAPE_HEIGHT + shapesF256
        .fill NUM_SHAPES * 2

TblBmpLinePtr
        .word ($00 * SHAPE_HEIGHT * 320) & $1fff
        .word ($01 * SHAPE_HEIGHT * 320) & $1fff
        .word ($02 * SHAPE_HEIGHT * 320) & $1fff
        .word ($03 * SHAPE_HEIGHT * 320) & $1fff
        .word ($04 * SHAPE_HEIGHT * 320) & $1fff
        .word ($05 * SHAPE_HEIGHT * 320) & $1fff
        .word ($06 * SHAPE_HEIGHT * 320) & $1fff
        .word ($07 * SHAPE_HEIGHT * 320) & $1fff
        .word ($08 * SHAPE_HEIGHT * 320) & $1fff
        .word ($09 * SHAPE_HEIGHT * 320) & $1fff
        .word ($0a * SHAPE_HEIGHT * 320) & $1fff
        .word ($0b * SHAPE_HEIGHT * 320) & $1fff
        .word ($0c * SHAPE_HEIGHT * 320) & $1fff
        .word ($0d * SHAPE_HEIGHT * 320) & $1fff
        .word ($0e * SHAPE_HEIGHT * 320) & $1fff
        .word ($0f * SHAPE_HEIGHT * 320) & $1fff
        .word ($10 * SHAPE_HEIGHT * 320) & $1fff
        .word (($10 * SHAPE_HEIGHT + 5) * 320) & $1fff

TblBmpLinePage
        .byte ($00 * SHAPE_HEIGHT * 320) >> 13
        .byte ($01 * SHAPE_HEIGHT * 320) >> 13
        .byte ($02 * SHAPE_HEIGHT * 320) >> 13
        .byte ($03 * SHAPE_HEIGHT * 320) >> 13
        .byte ($04 * SHAPE_HEIGHT * 320) >> 13
        .byte ($05 * SHAPE_HEIGHT * 320) >> 13
        .byte ($06 * SHAPE_HEIGHT * 320) >> 13
        .byte ($07 * SHAPE_HEIGHT * 320) >> 13
        .byte ($08 * SHAPE_HEIGHT * 320) >> 13
        .byte ($09 * SHAPE_HEIGHT * 320) >> 13
        .byte ($0a * SHAPE_HEIGHT * 320) >> 13
        .byte ($0b * SHAPE_HEIGHT * 320) >> 13
        .byte ($0c * SHAPE_HEIGHT * 320) >> 13
        .byte ($0d * SHAPE_HEIGHT * 320) >> 13
        .byte ($0e * SHAPE_HEIGHT * 320) >> 13
        .byte ($0f * SHAPE_HEIGHT * 320) >> 13
        .byte ($10 * SHAPE_HEIGHT * 320) >> 13          ; divider line
        .byte (($10 * SHAPE_HEIGHT + 5) * 320) >> 13    ; score line

.comment
        .word ?                         ; align tile_map_tile to word
tile_map_tile_window
        .fill 22*14*2, [$24,$00]        ; use tile set 0
.endcomment


resetSwapAreaA000               ; swap area at $a000
        lda #$0a
setSwapAreaA000
        lsr                     ; convert address [24..31] to MMU bank
        pha
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL
        pla
        sta MMU_MEM_BANK_5      ; MMU Edit Register for bank 5 ($A000 - $BFFF)

        lda #$03
        sta MMU_MEM_CTRL

.comment
        lda MMU_IO_CTRL                 ; ($c000-$dfff is I/O)
        and #%11111011                  ; enable I/O
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)
.endcomment
        rts

initSwapAreaC000
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL
        lda MMU_MEM_BANK_6      ; MMU Edit Register for bank 6 ($C000 - $DFFF)
        sta defaultBank6        ; store the default configuration for later use
        lda #$03
        sta MMU_MEM_CTRL
        rts

setSwapAreaC000
        lsr                     ; convert address [24..31] to MMU bank
        pha
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL
;;;        lda MMU_MEM_BANK_6      ; MMU Edit Register for bank 6 ($C000 - $DFFF)
;;;        sta defaultBank6
        pla
        sta MMU_MEM_BANK_6      ; MMU Edit Register for bank 6 ($C000 - $DFFF)

        lda #$03
        sta MMU_MEM_CTRL

        lda MMU_IO_CTRL                 ; ($c000-$dfff is I/O)
        ora #%00000100                  ; disable I/O
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)
        rts

resetSwapAreaC000               ; swap area at $c000
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL

        lda defaultBank6
        sta MMU_MEM_BANK_6      ; MMU Edit Register for bank 6 ($C000 - $DFFF)

        lda #$03
        sta MMU_MEM_CTRL

        lda MMU_IO_CTRL                 ; ($c000-$dfff is I/O)
        and #%11111011                  ; enable I/O
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)

        rts


setBigSwapArea
        lsr                     ; convert address [24..31] to MMU bank
        pha
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL
        lda MMU_MEM_BANK_6      ; MMU Edit Register for bank 6 ($C000 - $DFFF)
        sta defaultBank6
        pla
        sta MMU_MEM_BANK_5      ; MMU Edit Register for bank 5 ($A000 - $BFFF)
        clc
        adc #$01
        sta MMU_MEM_BANK_6      ; MMU Edit Register for bank 6 ($C000 - $DFFF)

        lda #$03
        sta MMU_MEM_CTRL

        lda MMU_IO_CTRL                 ; ($c000-$dfff is I/O)
        ora #%00000100                  ; disable I/O
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)
        rts

resetBigSwapArea
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL

        lda #$08 >> 1
        sta MMU_MEM_BANK_5      ; MMU Edit Register for bank 5 ($A000 - $BFFF)
        lda defaultBank6
        sta MMU_MEM_BANK_6      ; MMU Edit Register for bank 6 ($C000 - $DFFF)

        lda #$03
        sta MMU_MEM_CTRL

        lda MMU_IO_CTRL                 ; ($c000-$dfff is I/O)
        and #%11111011                  ; enable I/O
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)

        rts

defaultBank6
        .byte $00

testSprites
        ; A = sprite index (0 = player, 1..4 = enemies)
        ; X = pixel pos X / 2
        ; Y = pixel pos Y
        lda #3
        sta zpPlayerAnimPhase           ; player animation phase
        lda #0                          ; player
        ldx #140
        ldy #100
        jsr printSpriteF256

        lda #5
        sta zpEnemyAnimPhase
        lda #1                          ; player
        ldx #20
        ldy #10
        jsr printSpriteF256

        rts


testKeyboard
        jsr handleEvents
        jmp testKeyboard

printByte
        pha
        lsr
        lsr
        lsr
        lsr
        jsr printDigit
        pla
        and #$0f
        jsr printDigit
        jsr printString
        .text "  ",$00
        rts

printKbdEvent
        lda #$00
        sta zpCursorRow
        sta zpCursorCol
        lda event.type
        jsr printByte
        lda event.key.raw
        jsr printByte
        lda event.key.ascii
        jsr printByte
        lda event.key.flags
        jsr printByte
        rts

printJoyEvent
        lda #$02
        sta zpCursorRow
        lda #$00
        sta zpCursorCol
        lda event.joystick.joy0
        jsr printByte
        lda event.joystick.joy1
        jsr printByte
        rts

.comment
        lda #<spritesF256
        sta VKY_SP0_AD_L
        lda #>spritesF256
        sta VKY_SP0_AD_M
        lda #$00
        sta VKY_SP0_AD_H
        lda #<32
        sta VKY_SP0_POS_X_L
        lda #>32
        sta VKY_SP0_POS_X_H
        lda #<32
        sta VKY_SP0_POS_Y_L
        lda #>32
        sta VKY_SP0_POS_Y_H
        lda #%01000001          ; size: 16x16, layer 0, LUT 0, Enable
        sta VKY_SP0_CTRL
.endcomment

        rts

delayF256
        lda #$1
        sta _delay
_delayL1
        ldx #$00
        ldy #$00
_delayL2
        dex
        bne _delayL2
        dey
        bne _delayL2
        ldy _delay
        dey
        sty _delay
        bne _delayL1
        rts

_delay
        .byte $00


initSpritesF256
        lda #$00                        ; Set the I/O page to #0
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)

        ldy #$00                        ; iterator
_initSpriteL
        lda #<spritesF256
        sta VKY_SP0_AD_L,y
        lda #>spritesF256
        sta VKY_SP0_AD_M,y
        lda #$00
        sta VKY_SP0_AD_H,y

        sta VKY_SP0_POS_X_L,y           ; initially hide sprite in the border
        sta VKY_SP0_POS_X_H,y
        sta VKY_SP0_POS_Y_L,y
        sta VKY_SP0_POS_Y_H,y

        lda #%01000000                  ; size: 16x16, layer 0, LUT 0, Disable
        cpy #$00
        beq _initSpritePlayerLUT
        lda #%01000010                  ; size: 16x16, layer 0, LUT 1, Disable
_initSpritePlayerLUT
        sta VKY_SP0_CTRL,y              ; TODO: use LUT 1 for enemy sprites

        tya
        clc
        adc #$08
        tay
        cmp #$30
        bne _initSpriteL

_exit
        rts


printSpriteF256
        ; A = sprite index (0 = player, 1..4 = enemies)
        ; X = pixel pos X / 2
        ; Y = pixel pos Y
        asl                             ; * 8 (offset to sprite register block)
        asl
        asl
        sta _spriteRegisterOffset

        lda #$00                        ; Set the I/O page to #0
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)

        tya
        clc
        adc #32                         ; start offset of sprite
        ldy _spriteRegisterOffset
        sta VKY_SP0_POS_Y_L,y
        lda #$00
        sta VKY_SP0_POS_Y_H,y

        txa
        clc
        adc #(32/2)
        asl                             ; calculate actual X position (lb)
        sta VKY_SP0_POS_X_L,y
        lda #$00
        rol
        sta VKY_SP0_POS_X_H,y

        ; set sprite pointer from animation phase
        ldy _spriteRegisterOffset
        bne _printSpriteEnemy
_printSpritePlayer
        lda zpPlayerAnimPhase           ; player animation phase
        jmp _printSpriteJ1
_printSpriteEnemy
        ldx zpEnemyAnimPhase
        lda _tabEnemyToPlayerPhase,x

_printSpriteJ1
        clc
        adc #>spritesF256
        sta VKY_SP0_AD_M,y

        rts

_spriteRegisterOffset
        .byte $00

_tabEnemyToPlayerPhase
        .byte $00,$01,$02,$03,$04,$05,$07
        .byte $08,$09,$0a,$0b,$0c,$0d,$0f
        .byte $10,$11


enableSpriteF256
        ; A = sprite index (0 = player, 1..4 = enemies)
        asl                             ; * 8 (offset to sprite register block)
        asl
        asl
        tay
        cpy #$00
        beq _enableSpritePlayerLUT
_enableSpriteEnemyLut
        lda #%01000011                  ; size: 16x16, layer 0, LUT 1, Enable
        bne _enableSpriteJ1
_enableSpritePlayerLut
        lda #%01000001                  ; size: 16x16, layer 0, LUT 0, Enable
_enableSpriteJ1
        sta VKY_SP0_CTRL,y
        rts

disableSpriteF256
        ; A = sprite index (0 = player, 1..4 = enemies)
        asl                             ; * 8 (offset to sprite register block)
        asl
        asl
        tay
        cpy #$00
        beq _disableSpritePlayerLUT
_disableSpriteEnemyLut
        lda #%01000010                  ; size: 16x16, layer 0, LUT 1, Disable
        bne _disableSpriteJ1
_disableSpritePlayerLut
        lda #%01000000                  ; size: 16x16, layer 0, LUT 0, Disable
_disableSpriteJ1
        sta VKY_SP0_CTRL,y
        rts

enableSpritesF256
        ; A = max sprite
        tax
        lda #$00                        ; start with sprite 0
_enableSpritesL
        tay
        cpy #$00
        beq _enableSpritePlayerLUT
        lda #%01000011                  ; size: 16x16, layer 0, LUT 1, Enable
        bne _enableSpriteJ1
_enableSpritePlayerLut
        lda #%01000001                  ; size: 16x16, layer 0, LUT 0, Enable
_enableSpriteJ1
        sta VKY_SP0_CTRL,y
        tya
        clc
        adc #$08
        dex
        bpl _enableSpritesL
        rts

disableSpritesF256
        ; A = max. sprite
        tax
        lda #$00                        ; start with sprite 0
_disableSpritesL
        tay
        cpy #$00
        beq _disableSpritePlayerLUT
_disableSpriteEnemyLut
        lda #%01000010                  ; size: 16x16, layer 0, LUT 1, Disable
        bne _disableSpriteJ1
_disableSpritePlayerLut
        lda #%01000000                  ; size: 16x16, layer 0, LUT 0, Disable
_disableSpriteJ1
        lda VKY_SP0_CTRL,y
        and #%11111110                  ; Disable Sprite
        sta VKY_SP0_CTRL,y
        tya
        clc
        adc #$08
        dex
        bpl _disableSpritesL
        rts

loadBoardF256
        ; A = Level to load (0-based)
        pha
        lsr                             ; one bank ($2000 byte) can hold $20 levels
        lsr
        lsr
        lsr
        lsr
        asl                             ; calc bank address (also clears carry)
        adc #$30
        jsr setSwapAreaA000             ; bank in level data

        pla
        and #$1f
        clc
        adc #$a0
        sta _loadBoardL+2               ; set board offset within bank

        ldy #$00
_loadBoardL
        lda $a000,y                     ; self modified address
        sta boardPacked,y
        iny
        bne _loadBoardL

        jsr resetSwapAreaA000
        rts


.comment
resetSwapArea8000               ; swap area at $8000
        lda #$08
setSwapArea8000
        lsr                     ; convert address [24..31] to MMU bank
        pha
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL
        pla
        sta MMU_MEM_BANK_4      ; MMU Edit Register for bank 4 ($8000 - $9FFF)

        lda #$03
        sta MMU_MEM_CTRL
        rts


delayGameShort
        lda #2
        bne delayGameFrames

delayGame
        lda #10
        bne delayGameFrames

delayGameFrames
        ldx #$00
        jsr setTimer
_delayGameL
        jsr handleEvents
        lda timer_events+0
        beq _delayGameL
        rts

setTimer
        ; A: Timer delay (in frames)
        ; X: Event ID (Cookie)

        sta _delay
        stx _cookie

        ; clear game event associated with this timer
        lda #$00
        sta timer_events,x

        ; First, query the current FRAMES timer
        lda #kernel.args.timer.FRAMES | kernel.args.timer.QUERY
        sta kernel.args.timer.units
        jsr kernel.Clock.SetTimer

        ; Set the timer
        clc
        adc _delay
        sta kernel.args.timer.absolute
        lda #kernel.args.timer.FRAMES
        sta kernel.args.timer.units
        lda _cookie
        sta kernel.args.timer.cookie

        jsr kernel.Clock.SetTimer
        rts

_delay
        .byte ?
_cookie
        .byte ?

.endcomment

event_timer_handler
        ldx event.timer.cookie
        cpx #TIMER_EVENT_MAX+1
        bcs _exit

        lda #$01
        sta timer_events,x
_exit
        rts

.comment

clearMapAreaDungeon
        ; loop over four banks $018000 - $01ffff
        ; Fill the whole area with #$00 (make all tiles in tileset black)

        lda #$18                ; start at phys. memory $018000
        sta _physBank
_clearBmTilesetL1
        jsr setSwapAreaA000
        lda #$a0                ; start filling at logical $a000
        sta _dest+2             ; high byte of destination
        ldx #$20                ; for all banks but the last fill $20 pages
        lda _physBank
        cmp #$1e
        bne _notLastBank
        ldx #$1a                ; for the last bank, only need to fill $1a pages
_notLastBank
        lda #$00                ; fill value: $00
_clearBmTilesetL2
;;        txa                     ; TODO: TESTING ONLY
;;        and #$0f                ; TODO: TESTING ONLY fill tile with color
        ldy #$00
_dest
        sta $a000,y
        iny
        bne _dest

        inc _dest+2             ; inc high byte of destination
        dex
        bne _clearBmTilesetL2

        inc _physBank
        inc _physBank
        lda _physBank
        cmp #$20
        bne _clearBmTilesetL1

        jsr resetSwapAreaA000   ; restore swap area at $a000
        rts

_physBank
        .byte $00


clearDemoView
        ; TODO: implement
        pha
        ldx #5
_clearDemoViewL1
        clc
        lda tile_map_line_ptr_lb,x
        adc #<tile_map_tile_window
        sta zpDstPtr+0
        lda tile_map_line_ptr_hb,x
        adc #>tile_map_tile_window
        sta zpDstPtr+1

        ldy #2*22-1
_clearDemoViewL2
        lda #$00
        sta (zpDstPtr),y
        dey
        pla                             ; restore "empty tile" id
        sta (zpDstPtr),y
        pha
        dey
        bpl _clearDemoViewL2
        clc
        inx
        cpx #11
        bne _clearDemoViewL1
        pla                             ; discard "empty tile" id from stack
        rts


clearTileMap
        pha                             ; "empty tile" id

        lda #<tile_map_tile_window
        sta zpDstPtr+0

        lda #>tile_map_tile_window
        sta zpDstPtr+1
        ldx #14
clearTilemapL2
        ldy #2*22-1
clearTilemapL1
        lda #$00
        sta (zpDstPtr),y
        dey
        pla                             ; restore "empty tile" id
        ;;lda #11*11
        sta (zpDstPtr),y
        pha
        dey
        bpl clearTilemapL1
        clc
        lda zpDstPtr+0
        adc #2*22
        sta zpDstPtr+0
        lda zpDstPtr+1
        adc #$00
        sta zpDstPtr+1
        dex
        bne clearTilemapL2
        pla                             ; discard "empty tile" id from stack
        rts


setupTiBiMap

        ; re-organize the tilemap: tile_map_tile_window
        ; <row of empty tiles>
        ; <empty>, <empty>, 0,1,2,3,4,5,6,7,8,9,10, <empty>...
        ; <empty>, <empty>, 11,12,13, ....

        ; clear tilemap using tile #11*11
        lda #11*11
        jsr clearTileMap

;;;.comment
        lda #<tile_map_tile_window
        sta zpDstPtr+0
        lda #>tile_map_tile_window
        sta zpDstPtr+1
        ldx #14
_clearTilemapL2
        ldy #2*22-1
_clearTilemapL1
        lda #$00
        sta (zpDstPtr),y
        dey
        lda #11*11
        sta (zpDstPtr),y
        dey
        bpl _clearTilemapL1
        clc
        lda zpDstPtr+0
        adc #2*22
        sta zpDstPtr+0
        lda zpDstPtr+1
        adc #$00
        sta zpDstPtr+1
        dex
        bne _clearTilemapL2
;;;.endcomment


        lda #$00                ; start tile matrix with ID 0
        sta currentTile
        ldx #$00                ; index: row of tiles
_setBmTilemapL1
        clc
        lda tile_map_line_ptr_lb,x
        adc #<tile_map_tile_window
        sta zpDstPtr+0

        lda tile_map_line_ptr_hb,x
        adc #>tile_map_tile_window
        sta zpDstPtr+1

        ldy #$00                ; index: column of tiles
_setMbTilemapL2
        lda currentTile
        sta (zpDstPtr),y
        inc currentTile
        iny
        iny
        cpy #11*2
        bne _setMbTilemapL2

        inx
        cpx #11
        bne _setBmTilemapL1     ; repeat for all lines of map area

        ; switch the tileset to $018000 for pseudo-bitmap display

        lda #$00                ; tileset at $018000
        sta VKY_TS0_ADDR_L
        lda #$80
        sta VKY_TS0_ADDR_M
        lda #$01
        sta VKY_TS0_ADDR_H

        rts

currentTile
        .byte $00


setupSurfaceMap
        ; clear tilemap using tile #$24
        lda #$24
        jsr clearTileMap

        ; set tile set #0

        lda #$00                        ; tileset at $012000
        sta VKY_TS0_ADDR_L
        lda #$20
        sta VKY_TS0_ADDR_M
        lda #$01
        sta VKY_TS0_ADDR_H
        lda #$00                        ; tile set image is vertical
        sta $d283

        rts


showIntroExodus
        ; clear tilemap using tile #11*11
        lda #$24                        ; empty tile in tile set 0
        jsr clearTileMap

        lda #$00                        ; start tile matrix with ID 0
        sta currentTile
        ldx #$00                        ; index: row of tiles
_setBmTilemapL1
        clc
        lda tile_map_line_ptr_lb,x
        adc #<tile_map_tile_window
        sta zpDstPtr+0

        lda tile_map_line_ptr_hb,x
        adc #>tile_map_tile_window
        sta zpDstPtr+1

        ldy #$00                        ; index: column of tiles
_setMbTilemapL2
        lda currentTile
        sta (zpDstPtr),y                ; set tile number
        inc currentTile
        iny
        lda #$02                        ; tileset: 2 ("EXODUS" bitmap)
        sta (zpDstPtr),y                ; update tile attributes
        iny
        cpy #19*2
        bne _setMbTilemapL2

        inx
        cpx #5
        bne _setBmTilemapL1             ; repeat for all lines of map area
        rts
.endcomment


moveKernelToRam
        ; copy Kernel to RAM ($03e000-$03ffff)

        lda #$3e                        ; start at phys. memory $02e000
        jsr setSwapAreaA000             ; make physical memory $2e000 available at $a000
        lda #$e0                        ; source: $e000 (phys: $e000 ROM)
        ldy #$a0                        ; dest:   $a000 (phys: $e000 RAM)
        ldx #$20                        ; number of pages to copy
        jsr copyMemory

        jsr resetSwapAreaA000           ; restore original configuration for $a000

        ldx #$3                         ; update all 4 MLUTs
_moveKernelToRamL1
        lda _mmuMemCtrlValues,x
        sta MMU_MEM_CTRL
        lda #$3e >> 1                   ; convert address [24..31] to MMU bank
        sta MMU_MEM_BANK_7              ; MMU Edit Register for bank 7 ($E000 - $FFFF)
        dex
        bpl _moveKernelToRamL1

        lda #$03                        ; restore MLUT 3
        sta MMU_MEM_CTRL

        rts

_mmuMemCtrlValues
        .byte $b3,$a3,$93,$83


installCustomIsr
        ldx #customIsrEnd-(CustomIsr+1)
_copyL
        lda customIsr,x
        sta customIsrAddr,x
        dex
        bpl _copyL

        lda IRQVEC+0
        sta vIrqOriginal+0              ; original Kernel IRQ handler
        lda IRQVEC+1
        sta vIrqOriginal+1              ; original Kernel IRQ handler

        lda #<customIsrAddr
        sta IRQVEC+0
        lda #>customIsrAddr
        sta IRQVEC+1

        rts



customIsr
.logical customIsrAddr
        pha                     ; save registers a,x,y
        txa
        pha
        tya
        pha

        lda MMU_IO_CTRL         ; store current MMU I/O control register
        pha
        lda MMU_MEM_CTRL        ; store current MMU configuration
        sta _restoreMmuConfig+1 ; can't use pha since zero page may be paged

        lda #$03                ; switch to MLUT3 containing the user code
        sta MMU_MEM_CTRL
        lda #$00                ; enable I/O page #0 (Interrupt controller, SID)
        sta MMU_IO_CTRL

        lda #$40
        bit VIA_IFR
        beq _irqMusicSkip       ; if it's zero, exit the handler

        ; save current MMU configuration for banks 4 and 5
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL
        lda MMU_MEM_BANK_4      ; MMU Edit Register for bank 4 ($8000 - $9FFF)
        pha
        lda MMU_MEM_BANK_5      ; MMU Edit Register for bank 5 ($A000 - $BFFF)
        pha

        ; set standard user MLUT banks
        lda #$08>>1
        sta MMU_MEM_BANK_4      ; MMU Edit Register for bank 4 ($8000 - $9FFF)
        lda #$0a>>1
        sta MMU_MEM_BANK_5      ; MMU Edit Register for bank 5 ($A000 - $BFFF)
        lda #$03                ; switch to MLUT3 containing the user code
        sta MMU_MEM_CTRL

        dec viaDelay
        bpl _skipIrq
        lda #$01
        sta viaDelay
        jsr irqHandler          ; handle Lode Runner IRQ

_skipIrq
        ; restore MMU configuration for banks 4 and 5
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL
        pla
        sta MMU_MEM_BANK_5      ; MMU Edit Register for bank 5 ($A000 - $BFFF)
        pla
        sta MMU_MEM_BANK_4      ; MMU Edit Register for bank 4 ($8000 - $9FFF)
        lda #$03                ; switch to MLUT3 containing the user code
        sta MMU_MEM_CTRL

        lda VIA_T1C_L           ; clear timer1 interrupt flag
        lda #INT15_VIA          ; VIA interrupt flag
        sta INT_PEND_1          ; clear the flag for the VIA IRQ

_irqMusicSkip
_restoreMmuConfig
        lda #$42                ; restore previous MMU configuration (self modified)
        sta MMU_MEM_CTRL
        pla                     ; restore previous MMU I/O control register
        sta MMU_IO_CTRL
        pla                     ; restore registers y,x,a
        tay
        pla
        tax
        pla
vIrqOriginal = * + 1
        jmp $c0de               ; chain original IRQ-routine

viaDelay
        .byte $00
.endlogical
customIsrEnd


copyMemory
        sta zpSrcPtr+1          ; source page: A
        sty zpDstPtr+1          ; dest page: Y
        lda #$00                ; #pages: X
        sta zpSrcPtr+0
        sta zpDstPtr+0
        ldy #$00
_copyMemoryL1
        lda (zpSrcPtr),y
        sta (zpDstPtr),y
        iny
        bne _copyMemoryL1
        inc zpSrcPtr+1
        inc zpDstPtr+1
        dex                     ; copy all pages
        bne _copyMemoryL1
        rts


initViaTimer
        lda #$40                        ; T1 Timer: Continuous Interrupts
        sta VIA_ACR                     ; VIA: Auxiliary Control Register
        lda #<VIA_60_HZ_TIMER_INTERVAL  ; timer value, low byte
        sta VIA_T1C_L                   ; VIA: T1 Low-Order Counter/Latches
        lda #>VIA_60_HZ_TIMER_INTERVAL  ; timer value, high byte
        sta VIA_T1C_H                   ; VIA: T1 High-Order Counter - kick off the timer
        rts

enableViaIrq
        ; also enable VIA0 interrupt on Interrupt Controller
        lda INT_MASK_1
        and #~INT15_VIA                 ; unmask the VIA0 interrupt
        sta INT_MASK_1

        lda #INT15_VIA                  ; clear pending VIA interrupts
        sta INT_PEND_1
        rts

enableViaTimerIrq
        lda #$7f                        ; disable all VIA interrupts
        sta VIA_IER
        lda #$c0                        ; enable Timer1 interrupts
        sta VIA_IER                     ; VIA: Interrupt Enable Register
        lda VIA_T1C_L                   ; EXPERIMENTAL: clear timer1 interrupt flag
        lda #$7f
        sta VIA_IFR                     ; EXPERIMENTAL: clear interrupt flag
        lda #INT15_VIA                  ; EXPERIMENTAL: VIA interrupt flag
        sta INT_PEND_1                  ; EXPERIMENTAL: clear the flag for the VIA IRQ
        rts

setupLodeRunnerIrq
        jsr initViaTimer
        sei
        jsr moveKernelToRam
        jsr installCustomIsr
        jsr enableViaIrq
        jsr enableViaTimerIrq
        cli
        rts


; ZX7 decompressor
; by Peter Ferrie (peter.ferrie@gmail.com)

; Parameters:
;   zpSrcPtr: source address
;   zpDstPtr: destination address
;
; Used Variables
;   zpEcx = $e4                         ; 16 bit
;   zpLastZx7
;   zpTmpZx7

; unpack zx7
unpack_zx7
    lda #$00
    sta zpLastZx7
    sta zpEcx+0
    sta zpEcx+1

dzx7s_copy_byte_loop
    jsr getput                          ; fetch src byte, put to dst buffer
dzx7s_main_loop
    jsr dzx7s_next_bit                  ; read next source bit
    bcc dzx7s_copy_byte_loop            ; if bit==0: copy next byte verbatim

    sty zpTmpZx7                        ; zpTmpZx7 = 0 (counter)
_dzx7s_len_size_loop
    inc zpTmpZx7                        ; count successive '0' bits (following '1' bit)
    jsr dzx7s_next_bit                  ; read next source bit
    bcc _dzx7s_len_size_loop
    bcs dzx7s_len_value_skip

dzx7s_next_bit
    asl zpLastZx7
    bne dzx7s_next_bit_ret
    jsr dzx7FetchByte                   ; fetch byte from src pointer
    sec                                 ; mark 'zpLastZx7' byte as containing bits
    rol
    sta zpLastZx7

dzx7s_next_bit_ret
    rts

dzx7s_len_value_loop
    jsr dzx7s_next_bit                  ; read next source bit

dzx7s_len_value_skip
    rol zpEcx+0                         ; ecx = 1 << zpTmpZx7
    rol zpEcx+1
    bcs dzx7s_next_bit_ret              ; EOF: 16 zero-bits read ->
    dec zpTmpZx7
    bne dzx7s_len_value_loop
    inc zpEcx+0                         ; minimum for repetition: 2
    bne _skip_inc_ecx
    inc zpEcx+1
_skip_inc_ecx
    jsr dzx7FetchByte                   ; fetch byte from src pointer
    rol                                 ; 7 or 12 bit address?
    sta zpTmpZx7                        ; low byte offset
    tya                                 ; (A = 0)
    bcc _dzx7s_offset_end               ; 7 bit address ->
    lda #$10                            ; read additional 4 bits for hb address

_dzx7s_rld_next_bit
    pha
    jsr dzx7s_next_bit                  ; read next source bit
    pla
    rol
    bcc _dzx7s_rld_next_bit
    tax
    inx
    txa
    lsr

_dzx7s_offset_end
    sta zpTmpZx7+1                      ; high byte offset
    ror zpTmpZx7+0                      ; rotate highest bit into address lb
    lda zpSrcPtr+1                      ; temporarily store src pointer to stack
    pha
    lda zpSrcPtr+0
    pha

    lda zpDstPtr+0                      ; set local (new) src pointer
    sbc zpTmpZx7+0                      ; (relative to current address)
    sta zpSrcPtr+0
    lda zpDstPtr+1
    lda dstPage
    sbc zpTmpZx7+1
    sta zpSrcPtr+1

; set source bank
    jsr dzx7SetSourceBank

another_getput
    jsr getput2                         ; fetch src byte, put to dst buffer
    jsr dececx
    ora zpEcx+1
    bne another_getput
    pla                                 ; restore src pointer from stack
    sta zpSrcPtr+0
    pla
    sta zpSrcPtr+1
    bne dzx7s_main_loop

dececx
    ldx zpEcx+0
    bne _skip_ecx_dec
    dec zpEcx+1
_skip_ecx_dec
    dex
    stx zpEcx+0
    txa
    rts

getput                                  ; fetch src byte, put to dst buffer
    jsr dzx7FetchByte                   ; fetch byte from src pointer
    jsr dzx7StoreByte                   ; store byte in dst pointer
    rts

getput2
    jsr dzx7FetchDstByte
    jsr dzx7StoreByte                   ; store byte in dst pointer
    rts

dzx7SetSourceBank
    ; set source bank
    lsr
    lsr
    lsr
    lsr
    and #$0e
    ora #$10
    sta bitmapBank2
    jsr setSwapAreaA000
; adjust source ptr
    lda zpSrcPtr+1
    and #$1f
    adc #$a0
    sta zpSrcPtr+1
    rts

dzx7FetchDstByte
    ldy #$00
    lda (zpSrcPtr),y                    ; read byte from source pointer
    inc zpSrcPtr+0                      ; increment source pointer
    bne _end
    pha
    inc zpSrcPtr+1
    lda zpSrcPtr+1
    cmp #$c0
    bne _noBankChange
    lda #$a0
    sta zpSrcPtr+1
    inc bitmapBank2
    inc bitmapBank2
    lda bitmapBank2
    jsr setSwapAreaA000
_noBankChange
    pla
_end
    rts

dzx7FetchByte
    ldy #$00
    lda (zpSrcPtr),y                    ; read byte from source pointer
    inc zpSrcPtr+0                      ; increment source pointer
    bne _end
    inc zpSrcPtr+1
_end
    rts

dzx7StoreByte
    sta (zpDstPtr),y                    ; store byte in dst pointer
    inc zpDstPtr+0
    bne _skip_inc_dst
    pha
    inc dstPage                         ; increment destination page (0-based)
    inc zpDstPtr+1                      ; increment dst pointer (hb)
    lda zpDstPtr+1
    cmp #$e0
    bne _noBankChange
    lda #$c0
    sta zpDstPtr+1                      ; reset pointer for next bank
    inc bitmapBank
    inc bitmapBank
    lda bitmapBank
    php
    jsr setSwapAreaC000
    plp
_noBankChange
    pla
_skip_inc_dst
    rts


displayTitleScreenF256
    lda #<titleScreenPacked             ; set source pointer (packed bitmap data)
    sta zpSrcPtr+0
    lda #>titleScreenPacked
    sta zpSrcPtr+1

    lda #$00                            ; set destination pointer (bitmap)
    sta zpDstPtr+0
    sta dstPage
    lda #$c0                            ; bitmap pointer
    sta zpDstPtr+1

    lda #$10
    sta bitmapBank
    jsr setSwapAreaC000

    jsr unpack_zx7

    jsr resetSwapAreaA000
    jsr resetSwapAreaC000

    rts

bitmapBank
    .byte $00

bitmapBank2
    .byte $00

dstPage
    .byte $00
