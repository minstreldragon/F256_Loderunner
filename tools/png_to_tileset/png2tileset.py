from PIL import Image, ImageDraw, ImageColor
import inspect
import sys
import os
import argparse
import vic2

TILE_WIDTH = 10
TILE_HEIGHT = 11
NUM_TILES = 0x66

COL_BLACK = 0x0
COL_WHITE = 0x1
COL_RED = 0x2
COL_CYAN = 0x3
COL_PURPLE = 0x4
COL_GREEN = 0x5
COL_BLUE = 0x6
COL_YELLOW = 0x7
COL_ORANGE = 0x8
COL_BROWN = 0x9
COL_LIGHTRED = 0xa
COL_DARKGREY = 0xb
COL_MIDGREY = 0xc
COL_LIGHTGREEN = 0xd
COL_LIGHTBLUE = 0xe
COL_LIGHTGREY = 0xf


def parse_arguments():
    parser = argparse.ArgumentParser(description='image to bitmap data converter')
    parser.add_argument('fn_input', help="input file name")

    args = parser.parse_args()
    return args

def hexdump(data):
    msg = ""
    for b in data:
        msg += "{:02x} ".format(b)
    print (msg.strip())


def matchPixel(pixel, palette):
    best = (0, 3 * 256*256 + 1)
    for idx, color in enumerate(palette):
        dist = sum([(pixel[i] - color[i]) ** 2 for i in range(3)])
        if dist < best[1]:
            best = (idx, dist)
    #print(best)
    return best[0]


def getColor(image, x, y, palette):
    pixel = image.getpixel((x,y))
    #color = color_dict.get(pixel, None)
    color = matchPixel(pixel, palette)
    return color

def convertPngToBin(img, palette, width, height):
    # convert image into a tilemap
    outdata = bytearray()
    tile = 0
    for ty in range(0, height, TILE_HEIGHT):         # increase by tiles (vertically)
        for tx in range(0, width, TILE_WIDTH):       # increase by tiles (horizontally)
            for dy in range(TILE_HEIGHT):
                for dx in range(TILE_WIDTH):
                    px = getColor(img, tx + dx, ty + dy, palette)
                    outdata.append(px)
            tile = tile + 1
            if tile >= NUM_TILES:
                return outdata


if __name__ == '__main__':
    args = parse_arguments()
    print(args)

    #open the image file
    image = Image.open(args.fn_input).convert('RGB')
    width, height = image.size

    image.show()

    print("width:", width, "height:", height)

    # prepare writing
    nameparts = os.path.splitext(args.fn_input.lower())
        
    palette = vic2.makePalette()
    tilemap = convertPngToBin(image, palette, width, height)
    outname = nameparts[0] + "_vic2.bin"
    print("outname:", outname)
    with open(outname, "wb") as f:
        f.write(tilemap) 
