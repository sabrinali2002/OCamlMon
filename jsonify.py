"""
Python script for converting image files (png) into OCaml arrays of hexademical
color values, to be used in conjunction with the OCaml Graphics library to
generate sprites for OCamlMon.

Authors: CJ Lee, Kimmy Lin, Sabrina Li
"""

from PIL import Image
import os

PATH = "json_images"

def rgb_to_hex(r, g, b, a):
    if a == 0:
        hex = '"transp"'
    else:
        hex = '"0x{:02x}{:02x}{:02x}"'.format(r, g, b)
    return hex

def list_to_array(lst):
    arr = ['[', '\n']
    for row in lst:
        arr.append('[')
        for col in row:
            arr.append(col)
            arr.append(', ') if col != '"transp"' else arr.append('  , ')
        arr[-1] = ""
        arr.append('],\n')
    arr[-1] = "]\n"
    arr.append(']')
    return "".join(arr)

if __name__ == "__main__":
    input = input("Full image file name: ")
    try:
        print(f"Opening image {input}...")
        img = Image.open(input)
        
        pixels = img.convert('RGBA')
        size = img.size

        print("Converting image to JSON...")
        lst = []
        for row in range(size[0]):
            lst.append([])
            for col in range(size[1]):
                lst[-1].append(rgb_to_hex(*(pixels.getpixel((row, col)))))
        lst = list(map(list, zip(*lst)))
        result = list_to_array(lst)

        print("Creating output file...")
        if not os.path.exists(PATH):
            print(f"Creating directory /{PATH}...")
            os.mkdir(PATH)

        output = open(PATH+f"/{os.path.splitext(input)[0]}", "w")
        output.write(result)
    except:
        print("Invalid image file")
