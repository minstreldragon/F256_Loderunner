
initF256                                ; initialize F256

        ; init events
        lda #<event
        sta kernel.args.events+0
        lda #>event
        sta kernel.args.events+1

        ; setup tile graphics

        lda #$00                        ; Set the I/O page to #1
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)

        lda #$14                        ; enable Graphics and Tile engines. Disable Text
        sta VKY_MSTR_CTRL_0             ; save that to VICKY master control register 0
        lda #$01                        ; set CLK70, for 320x200 display size
        sta VKY_MSTR_CTRL_1             ; save that to VICKY master control register 0

        lda #$04                        ; Layer 0: Tile map 0, Layer 1: unused
        sta VKY_LAYER_CTRL_0
        lda #$00                        ; Layer 2: unused
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

        jsr setColorLut                 ; setup color LUT

        rts


handleEvents
        ; Peek at the event queue to see if anything is pending
        lda kernel.args.events.pending      ; negated count
        bpl _done

        ; get the next event
        jsr kernel.NextEvent
        bcs _done

        ; handle the event
        jsr _dispatch
        jmp handleEvents                    ; continue until the queue is drained

_done
        rts

_dispatch
        ; get the event's type
        lda event.type

        ; call the appropriate handler
        cmp #kernel.event.key.PRESSED
        beq event_key_pressed
        cmp #kernel.event.file.NOT_FOUND
        beq event_file_not_found
        cmp #kernel.event.file.ERROR
        beq event_file_not_found
        cmp #kernel.event.file.OPENED
        beq event_file_opened
        cmp #kernel.event.file.DATA
        beq event_file_data
        cmp #kernel.event.file.EOF
        beq event_file_eof
        cmp #kernel.event.file.CLOSED
        beq event_file_closed
        cmp #kernel.event.file.WROTE
        beq event_file_wrote
        cmp #kernel.event.timer.EXPIRED
        beq event_timer
        rts                                 ; ignore anything not handled

event_timer
        jmp event_timer_handler

event_key_pressed
        lda event.key.ascii
        beq _exit                       ; suppress unmapped keys (Shift)
        cmp #$60                        ; upper case?
        bcc _key_pressed_j1
        sec
        sbc #$20                        ; to lower case
_key_pressed_j1
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
        sta KBD
        ; TODO: do something with key code
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


setColorLut
        lda #$01                        ; Set the I/O page to #1
        sta MMU_IO_CTRL                 ; ($c000-$dfff is I/O)

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
        .byte $00,$00,$00,$00    ; black (text)
clut_end


        .word ?                         ; align tile_map_tile to word
tile_map_tile_window
        .fill 22*14*2, [$24,$00]        ; use tile set 0



.comment
.include "it.asm"

resetSwapArea                   ; swap area at $a000
        lda #$0a
setSwapArea
        lsr                     ; convert address [24..31] to MMU bank
        pha
        lda #$b3                ; active MLUT = 3, Edit MLUT #3
        sta MMU_MEM_CTRL
        pla
        sta MMU_MEM_BANK_5      ; MMU Edit Register for bank 5 ($A000 - $BFFF)

        lda #$03
        sta MMU_MEM_CTRL
        rts

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
        lda #5
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
        jsr setSwapArea
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

        jsr resetSwapArea       ; restore swap area at $a000
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

.comment
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
.endcomment


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


moveKernelToRam
        ; copy Kernel to RAM ($02e000-$02ffff)

        lda #$2e                        ; start at phys. memory $02e000
        jsr setSwapArea                 ; make physical memory $2e000 available at $a000
        lda #$e0                        ; source: $e000 (phys: $e000 ROM)
        ldy #$a0                        ; dest:   $a000 (phys: $e000 RAM)
        ldx #$20                        ; number of pages to copy
        jsr copyMemory

        jsr resetSwapArea               ; restore original configuration for $a000

        ldx #$3                         ; update all 4 MLUTs
_moveKernelToRamL1
        lda _mmuMemCtrlValues,x
        sta MMU_MEM_CTRL
        lda #$2e >> 1                   ; convert address [24..31] to MMU bank
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

        jsr irqMusicJ1          ; play back music

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

.endcomment
