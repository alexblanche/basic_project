# Basic Project

## Goals

In this project I aim at developing tools related to Casio calculators. The targetted model is Casio Graph 35+/75.

(Everything is work in progress.)

- A Casio Basic compiler and emulator: the compiler reads the content of a G1M file (programs, pictures, captures, strings, lists and matrices) and converts it into an abstract code that can be executed by the emulator
- A lexer that converts Casio Basic code written in text mode (with spaces and indentation) into a G1M file that runs on a Casio calculator, and that can be executed with the emulator
- A "decompiler" that generates indented code in a text file from a G1M file containing Casio Basic code (which uses keywords for operators and commands, such as "If", "List", "F-line"...)
- A picture editor, to draw monochromatic 64*128 pictures, or convert existing BMP images into pictures, and write them in a G1M file

## What works:
- The emulator runs text-mode Basic programs (that use Strings, Locate)
  Graphic display (F-line, DrawStat) coming soon
- The graphical interface to draw and modify monochromatic images
- BMP (RGB) image conversion to monochromatic image through the Floyd-Steinberg dithering algorithm or the simple Threshold algorithm
- Reading/writing G1M/G2M files: extraction/generation of all types of objects (programs, lists, matrices, pictures, captures, strings)

### To launch the emulator:

For convenience during development, the project is currently interpreted. It will be converted to compiled at a later date, but the main.ml program is temporarily used to launch all the subprograms.
To launch a Casio program in G1M or G2M format:
```
$ ocaml
#use "main.ml";;
play "[filename].g1m";;
```

To extract a G1M file from a Casio Basic program on a Casio calculator, see [FA-124](https://www.planet-casio.com/Fr/logiciels/voir_un_logiciel_casio.php?showid=16).

- The emulated Casio models are fx-9750GII (Graph 35+) / fx-9860GII (Graph 75). More modern models are not handled, and the compatibility with older models is not guaranteed.
- The graphic display (F-line, DrawStat) is not implemented yet. Only the text mode display is implemented so far (Strings, Locate).
- Only the main functionalities are implemented, some programs may not be supported or run as expected.


## To do next:
Emulation:
- Code the graphic display (F-line, DrawStat)
- Code the remaining functions and commands

Compilers and decompilers:
- Code a converter from abstract Basic code to readable "Custom Basic" code
- Code a lexer from Custom Basic code to list of lexemes (which can then be handled by the compiler)

Interface:
- Code an interface that opens G1M files and displays its content
- Code a PRGM menu that displays the available programs, and launches the emulator
- Code a Picture Edition general interface, that opens BMP files and writes G1M files
- Code an interface that displays all the buttons of the calculator, in order to interact with the programs, and a keybinds interface (for more convenient gameplay)
