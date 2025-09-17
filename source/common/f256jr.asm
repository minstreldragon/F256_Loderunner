;;;
;;; I/O register address definitions
;;;

;;
;; MMU Registers
;;

MMU_MEM_CTRL = $0000            ; MMU Memory Control Register
MMU_IO_CTRL = $0001             ; MMU I/O Control Register
MMU_IO_PAGE_0 = $00
MMU_IO_PAGE_1 = $01
MMU_IO_TEXT = $02
MMU_IO_COLOR = $03
MMU_MEM_BANK_0 = $0008          ; MMU Edit Register for bank 0 ($0000 - $1FFF)
MMU_MEM_BANK_1 = $0009          ; MMU Edit Register for bank 1 ($2000 - $3FFF)
MMU_MEM_BANK_2 = $000A          ; MMU Edit Register for bank 2 ($4000 - $5FFF)
MMU_MEM_BANK_3 = $000B          ; MMU Edit Register for bank 3 ($6000 - $7FFF)
MMU_MEM_BANK_4 = $000C          ; MMU Edit Register for bank 4 ($8000 - $9FFF)
MMU_MEM_BANK_5 = $000D          ; MMU Edit Register for bank 5 ($A000 - $BFFF)
MMU_MEM_BANK_6 = $000E          ; MMU Edit Register for bank 6 ($C000 - $DFFF)
MMU_MEM_BANK_7 = $000F          ; MMU Edit Register for bank 7 ($E000 - $FFFF)

;;
;; Vicky Registers
;;

VKY_MSTR_CTRL_0 = $D000         ; Vicky Master Control Register 0
VKY_MSTR_CTRL_1 = $D001         ; Vicky Master Control Register 1

VKY_LAYER_CTRL_0 = $D002        ; Vicky Layer Control Register 0
VKY_LAYER_CTRL_1 = $D003        ; Vicky Layer Control Register 1

VKY_BRDR_CTRL = $D004           ; Vicky Border Control Register
VKY_BRDR_COL_B = $D005          ; Vicky Border Color -- Blue
VKY_BRDR_COL_G = $D006          ; Vicky Border Color -- Green
VKY_BRDR_COL_R = $D007          ; Vicky Border Color -- Red
VKY_BRDR_HORI = $D008           ; Vicky Border Horizontal Thickness in pixels
VKY_BRDR_VERT = $D009           ; Vicky Border vertical thickness in pixels

VKY_BKG_COL_B = $D00D           ; Vicky Graphics Background Color Blue Component
VKY_BKG_COL_G = $D00E           ; Vicky Graphics Background Color Green Component
VKY_BKG_COL_R = $D00F           ; Vicky Graphics Background Color Red Component

VKY_CRSR_CTRL = $D010           ; Vicky Text Cursor Control
VKY_CRSR_CHAR = $D012
VKY_CRSR_X_L = $D014            ; Cursor X position
VKY_CRSR_X_H = $D015
VKY_CRSR_Y_L = $D016            ; Cursor Y position
VKY_CRSR_Y_H = $D017

VKY_LINE_CTRL = $D018           ; Control register for the line interrupt
VKY_LINE_ENABLE = $01
VKY_LINE_NBR_L = $D019          ; Line number target low byte
VKY_LINE_NBR_H = $D01A          ; Line number target high byte


;;
;; Bitmap Registers
;;

VKY_BM0_CTRL = $D100            ; Bitmap #0 Control Register
VKY_BM0_ADDR_L = $D101          ; Bitmap #0 Address bits 7..0
VKY_BM0_ADDR_M = $D102          ; Bitmap #0 Address bits 15..8
VKY_BM0_ADDR_H = $D103          ; Bitmap #0 Address bits 17..16

VKY_BM1_CTRL = $D108            ; Bitmap #1 Control Register
VKY_BM1_ADDR_L = $D109          ; Bitmap #1 Address bits 7..0
VKY_BM1_ADDR_M = $D10A          ; Bitmap #1 Address bits 15..8
VKY_BM1_ADDR_H = $D10B          ; Bitmap #1 Address bits 17..16

VKY_BM2_CTRL = $D110            ; Bitmap #2 Control Register
VKY_BM2_ADDR_L = $D111          ; Bitmap #2 Address bits 7..0
VKY_BM2_ADDR_M = $D112          ; Bitmap #2 Address bits 15..8
VKY_BM2_ADDR_H = $D113          ; Bitmap #2 Address bits 17..16

VKY_TXT_FGLUT = $D800           ; Text foreground CLUT
VKY_TXT_BGLUT = $D840           ; Text background CLUT

;;
;; Color Lookup Tables (I/O Page 1)
;;

VKY_GR_CLUT_0 = $D000           ; Graphics LUT #0
VKY_GR_CLUT_1 = $D400           ; Graphics LUT #1
VKY_GR_CLUT_2 = $D800           ; Graphics LUT #2
VKY_GR_CLUT_3 = $DC00           ; Graphics LUT #3

;;
;; Buzzer and Status LEDs
;;

VKY_SYS0 = $D6A0
VKY_SYS1 = $D6A1

SYS_SID_ST = $08
SYS_PSG_ST = $04

;;
;; Software Reset
;;
VKY_RST0 = $D6A2 	;R/W  Set to 0xDE to enable software reset
VKY_RST1 = $D6A3 	;R/W  Set to 0xAD to enable software reset

;;
;; Random Numbers
;;
VKY_SEEDL 	 = $D6A4    ; SEED[7. . . 0]
VKY_RNDL 	 = $D6A4    ; RND[7. . . 0]
VKY_SEEDH 	 = $D6A5    ; SEED[15. . . 0]
VKY_RNDH 	 = $D6A5    ; RND[15. . . 0]
VKY_RND_CTRL = $D6A6 	; SEED_LD=$2 ENABLE=$1
VKY_RND_STAT = $D6A6 	; DONE=$80

;;
;; Machine ID and Version
;;
VKY_MID = $D6A7			; Machine ID
VKY_PCBID0 = $D6A8		; "B"
VKY_PCBID1 = $D6A9      ; "0"
VKY_CHSV0 = $D6AA       ; TinyVicky subversion BCD (low)
VKY_CHSV1 = $D6AB  		; TinyVicky subversion in BCD (high)
VKY_CHV0 = $D6AC  		; TinyVicky version in BCD (low)
VKY_CHV1 = $D6AD  		; TinyVicky version in BCD (high)
VKY_CHN0 = $D6AE  		; TinyVicky number in BCD (low)
VKY_CHN1 = $D6AF  		; TinyVicky number in BCD (high)
VKY_PCBMA = $D6EB  		; PCB Major Rev (ASCII)
VKY_PCBMI = $D6EC  		; PCB Minor Rev (ASCII)
VKY_PCBD = $D6ED  		; PCB Day (BCD)
VKY_PCBM = $D6EE  		; PCB Month (BCD)
VKY_PCBY = $D6EF  		; PCB Year (BCD)

MID_C256_FMX    = %00000
MID_C256_U      = %00001
MID_F256        = %00010
MID_F256_K      = %10010
MID_A2560_DEV   = %00011
MID_GEN_X       = %00100
MID_C256_U_PLUS = %00101
MID_A2560_X = %01000 
MID_A2560_U = %01001 
MID_A2560_M = %01010 
MID_A2560_K = %01011 

;;
;; Sound Generators
;;
VKY_PSG0 = $D600
VKY_PSG1 = $D610

VKY_PSG_BOTH = $D608

SID_LEFT = $D400        ; Base address of left SID chip
SID_MONO = $D480        ; Base address, writes to both SID chips
SID_RIGHT = $D500       ; Base address right SID chip


;;
;; CODEC
;;
CODEC_LO         = $D620
CODEC_HI         = $D621
CODEC_CTRL       = $D622


