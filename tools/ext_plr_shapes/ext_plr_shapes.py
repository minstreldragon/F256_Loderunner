from PIL import Image, ImageDraw, ImageColor
import inspect
import sys
import math

TILE_WIDTH = 10
TILE_HEIGHT = 11

NUM_SHAPES = 0x68

sprite_tiles = [
    0x0b,0x0c,0x0d,0x18,0x19,0x1a,0x0f,0x13,
    0x09,0x10,0x11,0x15,0x16,0x17,0x25,0x14,
    0x0e,0x12 ]

def extract_player_shapes(charset):
    # create output buffer
    plr_shapes = bytearray()

    for tile in sprite_tiles:
        print(tile)
        for y in range(TILE_HEIGHT):
            #plr_shapes.append(charset[tile+NUM_SHAPES*2*y:tile+NUM_SHAPES*2*y+2])
            plr_shapes.append(charset[tile+NUM_SHAPES*2*y])             # left byte
            plr_shapes.append(charset[tile+NUM_SHAPES*(2*y+1)])         # right byte
            
    return plr_shapes


if __name__ == '__main__':

    # open and read C64 tile set file
    file = open("lr_shapes", "rb")
    charset = file.read()
    file.close()

    outdata = extract_player_shapes(charset)

    # open the output file
    with open ("plr_shapes", mode = 'wb') as outfile:
        outfile.write(outdata)

