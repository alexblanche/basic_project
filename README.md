# Basic Project

## Goals

In this project I aim at developing tools related to Casio calculators. The targetted model is Casio Graph 35+/75.

(Everything is work in progress.)

- A Casio Basic compiler and emulator: the compiler reads the content of a G1M file (programs, pictures, captures, strings, lists and matrices) and converts it into  an abstract code that can be executed by the emulator
- A lexer that converts Casio Basic code written in text mode (with spaces and indentation) into a G1M file that runs on a Casio calculator, and that can be executed with the emulator
- A "decompiler" that generates indented code in text mode from a G1M file containing Casio Basic code
- A picture editor, to draw monochromatic 64*128 pictures or convert existing BMP images into pictures and write them in a G1M file

I refer to "text mode" as opposed to code written on a calculator or on FA-124, which uses keywords for operators and functions ("If", "Then", "List", "F-line"...).


## What works:
- The graphical interface to draw and modify monochromatic images
- BMP (RGB) image conversion to monochromatic image through the Floyd-Steinberg dithering algorithm or the simple Threshold algorithm
- Reading/writing G1M/G2M files: extraction/generation of all types of objects (programs, lists, matrices, pictures, captures, strings)


## To do next:
Compilers and decompilers:
- Code a parser to convert a program to an abstract "Basic code" type
- Code a converter from Basic tree to readable "Custom Basic" code
- Code a lexer from Custom Basic code to list of lexemes (which can then be handled by the compiler)

Emulation:
- Code Getkey
- Implement List/Mat arithmetic
- Code all operators and functions

Interface:
- Code an interface that opens G1M files and displays its content
- Code a PRGM menu that displays the available programs, and launches the emulator
- Code a Picture Edition general interface, that opens BMP files and writes G1M files
- Code an interface that displays all the buttons of the calculator, in order to interact with the programs, and a keybinds interface (for more convenient gameplay)
