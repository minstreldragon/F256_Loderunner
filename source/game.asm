; .include "common/api.asm"
; .include "common/f256jr.asm"
; .include "common/f256_tiles.asm"
; .include "common/f256_rtc.asm"
; .include "constants.h"

        * = $0080                       ; zero page variables

.include "zp.asm"

.include "archdep_f256.h"

        * = $0300                       ; target address is $0300

.include "it.asm"                       ; actual lode runner game

.include "archdep.asm"                  ; architecture dependent code

.include "variables.asm"                ; game variables

shapesF256
.binary "../assets/lr_shapes.bin"                 ; shapes

