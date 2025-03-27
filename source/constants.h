GAME_MODE_TITLE_SCREEN = 0
GAME_MODE_DEMO = 1
GAME_MODE_PLAY_GAME = 2
GAME_MODE_PLAY_FROM_EDITOR = 3
GAME_MODE_EDITOR = 5

GAME_DELAY_MIN = 3
GAME_DELAY_DEFAULT = 6
GAME_DELAY_MAX = 8

; Lode Runner Shapes, see:
; https://www.mocagh.org/broderbund/loderunner-manual.pdf, page 7

SHAPE_BLANK         = $00
SHAPE_FLOOR_DIG     = $01
SHAPE_FLOOR_UNDIG   = $02
SHAPE_LADDER        = $03
SHAPE_BAR           = $04
SHAPE_TRAP_DOOR     = $05
SHAPE_EXIT_LADDER   = $06
SHAPE_GOLD_CHEST    = $07
SHAPE_ENEMY         = $08
SHAPE_PLAYER        = $09
SHAPE_INVALID       = $0a               ; invalid shape: $0a..$0f
SHAPE_CURSOR        = $0a               ; solid cursor

; NUM_SHAPES          = $4c               ; number of distinct shapes
NUM_SHAPES          = $68               ; number of distinct shapes

ANIM_PHASE_RUN_LEFT_0   = $00
ANIM_PHASE_RUN_LEFT_1   = $01
ANIM_PHASE_RUN_LEFT_2   = $02
ANIM_PHASE_BAR_LEFT_0   = $03
ANIM_PHASE_BAR_LEFT_1   = $04
ANIM_PHASE_BAR_LEFT_2   = $05
ANIM_PHASE_FIRE_LEFT    = $06
ANIM_PHASE_FALL_LEFT    = $07
ANIM_PHASE_RUN_RIGHT_0  = $08
ANIM_PHASE_RUN_RIGHT_1  = $09
ANIM_PHASE_RUN_RIGHT_2  = $0a
ANIM_PHASE_BAR_RIGHT_0  = $0b
ANIM_PHASE_BAR_RIGHT_1  = $0c
ANIM_PHASE_BAR_RIGHT_2  = $0d
ANIM_PHASE_FIRE_RIGHT   = $0e
ANIM_PHASE_FALL_RIGHT   = $0f
ANIM_PHASE_CLIMB_0      = $10
ANIM_PHASE_CLIMB_1      = $11

EN_ANIM_PHASE_RUN_LEFT_0    = $00
EN_ANIM_PHASE_RUN_LEFT_1    = $01
EN_ANIM_PHASE_RUN_LEFT_2    = $02
EN_ANIM_PHASE_BAR_LEFT_0    = $03
EN_ANIM_PHASE_BAR_LEFT_1    = $04
EN_ANIM_PHASE_BAR_LEFT_2    = $05
EN_ANIM_PHASE_FALL_LEFT     = $06
EN_ANIM_PHASE_RUN_RIGHT_0   = $07
EN_ANIM_PHASE_RUN_RIGHT_1   = $08
EN_ANIM_PHASE_RUN_RIGHT_2   = $09
EN_ANIM_PHASE_BAR_RIGHT_0   = $0a
EN_ANIM_PHASE_BAR_RIGHT_1   = $0b
EN_ANIM_PHASE_BAR_RIGHT_2   = $0c
EN_ANIM_PHASE_FALL_RIGHT    = $0d
EN_ANIM_PHASE_CLIMB_0       = $0e
EN_ANIM_PHASE_CLIMB_1       = $0f

DIG_ANIM_PHASE_LEFT_MIN     = $00
DIG_ANIM_PHASE_LEFT_MID     = $06
DIG_ANIM_PHASE_LEFT_MAX     = $0b
DIG_ANIM_PHASE_RIGHT_MIN    = $0c
DIG_ANIM_PHASE_RIGHT_MID    = $12
DIG_ANIM_PHASE_RIGHT_MAX    = $17

ENEMY_MOVE_NONE         = $00
ENEMY_MOVE_LEFT         = $01
ENEMY_MOVE_RIGHT        = $02
ENEMY_MOVE_UP           = $03
ENEMY_MOVE_DOWN         = $04

;ANIM_CLOSE_HOLE_0       = $39
ANIM_CLOSE_HOLE_0       = $37
;ANIM_CLOSE_HOLE_1       = $3a
ANIM_CLOSE_HOLE_1       = $38
;ANIM_RESPAWN_0          = $3b
ANIM_RESPAWN_0          = $39
;ANIM_RESPAWN_1          = $3c
ANIM_RESPAWN_1          = $3a

BOARD_WIDTH  = 28                       ; $1c
BOARD_HEIGHT = 16                       ; $10
BOARD_SIZE = BOARD_WIDTH * BOARD_HEIGHT
BOARD_SIZE_PACKED = BOARD_SIZE / 2      ; 224

BOARD_MAXIMUM = 149                     ; $95

SHAPE_WIDTH = 10                        ; shape width: 10 pixels
SHAPE_HEIGHT = 11                       ; shape height: 11 pixels

MAX_HOLES_OPEN = 30
HOLE_OPEN_CTR_INIT = 180                ; time until hole closes

MAX_NUM_ENEMIES = $05

MAX_NUM_EXIT_LADDERS = 45               ; ($2d)

ENEMY_RESPAWN_CTR_INIT = 20             ; time until enemy respawns

ENEMY_PIT_CTR_WIGGLE_START = $0d        ; below this value, enemy tries to break free
ENEMY_PIT_CTR_ESCAPE_START = $07        ; below this value, enemy climbs out of hole

PLAYER_DEAD  = $00
PLAYER_ALIVE = $01

STEP_MIN = $00
STEP_MIDDLE = $02
STEP_MAX = $04

PLAYER_FACING_LEFT  = $ff               ; player facing left
PLAYER_FACING_RIGHT = $01               ; player facing right

SCORE_PICK_UP_GOLD = $0250              ; score for picking up a gold chest
SCORE_TRAP_ENEMY = $0075                ; score for trapping an enemy
SCORE_KILL_ENEMY = $0075                ; score for burying an enemy

DISK_MASTER = $01
DISK_USER = $ff
DISK_UNKNOWN = $00

DISK_CMD_LOAD = $01
DISK_CMD_SAVE = $02
DISK_CMD_INIT = $04

IRIS_RADIUS_MIN = 1                     ; ($01)
IRIS_RADIUS_MAX = 164                   ; ($a4)
IRIS_CENTER_X = 160                     ; ($a0)
IRIS_CENTER_Y = 88                      ; ($58)

IRIS_MAX_COL_X = 39                     ; $27
IRIS_MAX_POS_Y = 175                    ; $af
