# Stew-3000
[![Build status](https://github.com/stew2003/Stew-3000/actions/workflows/workflow.yml/badge.svg)](https://github.com/stew2003/Stew-3000/actions/workflows/workflow.yml)

This repository holds all the code behind the Stew-3000: A homemade 8-bit breadboard computer heavily inspired by Ben Eater.

![Stew 3000 diagram](https://raw.githubusercontent.com/wiki/stew2003/Stew-3000/images/full_diagram.jpeg)

## Project Structure

Here's a quick guide to how things are laid out:

- `display/`: For programming the displays used on the 3000
- `EEPROM_programmer_library/`: For programming the EEPROMs used on the 3000
- `lang/`: Implementations of the compiler, assembler, emulator
- `microcode/`: Microcode for instruction set
- `programs/`: C and assembly programs for running on the 3000
- `RAM_loader/`: For loading programs onto the 3000 via the Arduino
- `scripts/`: Several scripts we've used as shortcuts while working with the 3000

## Documentation

To learn more about the Stew-3000, check out [the wiki](https://github.com/stew2003/Stew-3000/wiki)!
