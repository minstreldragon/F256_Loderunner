;
; Integer Math Coprocessor
;

MULU_A_L = $DE00 ;R/W  Unsigned A Low Byte
MULU_A_H = $DE01 ;R/W  Unsigned A High Byte
MULU_B_L = $DE02 ;R/W  Unsigned B Low Byte
MULU_B_H = $DE03 ;R/W  Unsigned B High Byte
MULU_LL = $DE10 ;R  ? x ? (unsigned) byte 0
MULU_LH = $DE11 ;R  ? x ? (unsigned) byte 1
MULU_HL = $DE12 ;R  ? x ? (unsigned) byte 2
MULU_HH = $DE13 ;R  ? x ? (unsigned) byte 3
DIVU_DEN_L = $DE04 ;R/W  Unsigned Denominator Low Byte
DIVU_DEN_H = $DE05 ;R/W  Unsigned Denominator High Byte
DIVU_NUM_L = $DE06 ;R/W  Unsigned Numerator Low Byte
DIVU_NUM_H = $DE07 ;R/W  Unsigned Numerator High Byte
QUOU_LL = $DE14 ;R  Quotient of NUM/DEN (unsigned) low byte
QUOU_LH = $DE15 ;R  Quotient of NUM/DEN (unsigned) high byte
REMU_HL = $DE16 ;R  Remainder of NUM/DEN (unsigned) low byte
REMU_HH = $DE17 ;R  Remainder of NUM/DEN (unsigned) low byte
ADD_A_LL = $DE08 ;R/W  Unsigned A byte 0
ADD_A_LH = $DE09 ;R/W  Unsigned A byte 1
ADD_A_HL = $DE0A ;R/W  Unsigned A byte 2
ADD_A_HH = $DE0B ;R/W  Unsigned A byte 3
ADD_B_LL = $DE0C ;R/W  Unsigned B byte 0
ADD_B_LH = $DE0D ;R/W  Unsigned B byte 1
ADD_B_HL = $DE0E ;R/W  Unsigned B byte 2
ADD_B_HH = $DE0F ;R/W  Unsigned B byte 3
ADD_R_LL = $DE18 ;R  ? + ? (unsigned) byte 0
ADD_R_LH = $DE19 ;R  ? + ? (unsigned) byte 1
ADD_R_HL = $DE1A ;R  ? + ? (unsigned) byte 2
ADD_R_HH = $DE1B ;R  ? + ? (unsigned) byte 3
