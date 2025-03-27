# Lode Runner - Boards Extractor

import argparse
import os
import glob
import subprocess

LEVELS_NUM = 150
START_TRACK = 3
SECTORS_PER_TRACK = 21
BOARDS_PER_TRACK = 16
BYTES_PER_SECTOR = 256
BOARD_START_OFFSET = 1

def parse_arguments():
    parser = argparse.ArgumentParser(description='Lode Runner boards extractor')
    parser.add_argument('fn_input', help="d64 input file name")

    args = parser.parse_args()
    return args

def pack_file(filename):

    npcNumber = int(filename.split('_')[-1], 16)
    print('NPC number: {:02x}'.format(npcNumber))

    #tmpFileName = 'tmp.zx7'
    tmpFileName = filename +'.zx7'
    unpackedSize = os.path.getsize(filename)
    print('Processing file:', filename, 'size:', unpackedSize)

    # call "zx7 -b <input> <output>" in order to compress the file
    toolsdir = os.path.dirname(os.path.realpath(__file__))
    cmdzx7 = os.path.join(toolsdir, 'zx7')
    subprocess.run([cmdzx7, '-b', filename, tmpFileName])

    # add 2 to packedSize to account for 'unpacked size' header
    packedSize = os.path.getsize(tmpFileName) + 2
    print('packed file size:', packedSize)

    chunk = bytearray()
    chunk.append(npcNumber)
    chunk.extend(packedSize.to_bytes(2, 'little'))
    chunk.extend(unpackedSize.to_bytes(2, 'little'))

    with open(tmpFileName, 'rb') as f:
        tlk_file = f.read()    
        chunk.extend(tlk_file)

    os.remove(tmpFileName)
    return chunk


def get_board(diskdata, level):
    track = START_TRACK + level // BOARDS_PER_TRACK
    sector = level % BOARDS_PER_TRACK
    offset = ((track-1) * SECTORS_PER_TRACK + sector) * BYTES_PER_SECTOR + BOARD_START_OFFSET
    return diskdata[offset:offset+BYTES_PER_SECTOR]


def extract_boards(diskdata, outfile):
    for level in range(LEVELS_NUM):
        board = get_board(diskdata, level)
        outfile.write(board)

if __name__ == "__main__":
    # find location from argument, convert to string
    args = parse_arguments()

    # read the disk data into buffer lr_disk
    with open (args.fn_input, mode = 'rb') as infile:
        diskdata = infile.read()

    # open the output file
    with open ("boards", mode = 'wb') as outfile:
        extract_boards(diskdata, outfile)

