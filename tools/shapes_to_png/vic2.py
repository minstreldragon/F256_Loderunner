from PIL import Image, ImageDraw, ImageColor
import math

# color calculation based on:
# https://www.pepto.de/projects/colorvic/

colStrings = ("Black", "White", "Red", "Cyan",
              "Purple", "Green", "Blue", "Yellow",
              "Orange", "Brown", "Lt.Red", "Dk.Grey",
              "Md.Grey", "Lt.Green", "Lt.Blue", "Lt.Grey")

colLevels = [0, 32, 8, 24, 16, 16, 8, 24, 16, 8, 16, 8, 16, 24, 16, 24]
colAngles = [None, None, 4, 12, 2, 10, 15, 7, 5, 6, 4, None, None, 10, 15, None]

def compose(index, revision, brightness, contrast, saturation):
    # constants
    sector = 360/16
    origin = sector/2
    radian = math.pi/180
    screen = 1/5

    # normalize
    brightness -= 50
    contrast /= 100
    saturation *= 1 - screen

    # construct
    components = { "u": 0, "v": 0 }  # monochrome (chroma switched off)

    if colAngles[index] is not None: 
        angle = (origin + colAngles[index] * sector) * radian
        components["u"] = math.cos(angle) * saturation
        components["v"] = math.sin(angle) * saturation

    components["y"] = 8 * colLevels[index] + brightness

    for key in components:
        components[key] *= contrast + screen

    return components

def convert(components):
    # matrix transformation
    color = {}

    color["r"] = components["y"]                           + 1.140 * components["v"]
    color["g"] = components["y"] - 0.396 * components["u"] - 0.581 * components["v"]
    color["b"] = components["y"] + 2.029 * components["u"]

    # gamma correction
    source = 2.8 # PAL
    target = 2.2 # sRGB

    for i in color:
        color[i] = max(min(color[i], 255), 0)
        color[i] = 255 ** (1 - source) * color[i] ** source
        color[i] = 255 ** (1 - 1/target) * color[i] ** (1/target)
        color[i] = round(color[i])

    colstr = "#" + "".join(["{:02x}".format(color[i]) for i in "rgb"])
    return colstr

def makePalette():
    palette = []
    for i in range(16):
        colstr = convert(compose(i, "mc", 50, 100, 50))
        palette.append(ImageColor.getrgb(colstr))
    return palette
