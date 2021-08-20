#!/usr/bin/env python3
import re
import sys

# This script replaces each instance of the placeholder string 
# below in the given file with an opcode (formatted as a hex string)

placeholder = "OPCODE TBD"

def as_hex(n):
    return "0x{:02x}".format(n)

# check args
if len(sys.argv) != 2:
    print(f"usage: {sys.argv[0]} FILE")
    sys.exit(1)

with open(sys.argv[1], "r+") as file:
    contents = file.read()
    split_by_opcodes = contents.split(placeholder)

    # ensure there is at least one placeholder to replace
    if len(split_by_opcodes) == 1:
        print(f"no instances of the placeholder `{placeholder}` found!")
        sys.exit(1)

    with_opcodes = ""
    num_opcodes = len(split_by_opcodes) - 1

    for i in range(len(split_by_opcodes)):
        with_opcodes += split_by_opcodes[i]

        # write an opcode if this is not the last line
        if i < num_opcodes:
            with_opcodes += as_hex(i)

    # write the contents of the file with the opcodes added
    file.seek(0)
    file.write(with_opcodes)
    file.truncate()

    print(f"{num_opcodes} opcodes assigned! (0x00-{as_hex(num_opcodes-1)})")