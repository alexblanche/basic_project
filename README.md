# Basic Project

## Goals

In this project I aim at developing tools related to Casio calculators. The targetted model is Casio Graph 35+/75.

(Everything is work in progress.)

- A Casio Basic compiler and emulator: the compiler reads the content of a G1M file (programs, pictures, captures, strings, lists and matrices) and converts it into  an abstract code that can be executed by the emulator
- A lexer that converts Casio Basic code written in text mode (with spaces and indentation) into a G1M file that runs on a Casio calculator, and that can be executed with the emulator
- A "decompiler" that generates indented code in text mode from a G1M file containing Casio Basic code
- A picture editor, to draw monochromatic 64*128 pictures or convert existing BMP images into pictures and write them in a G1M file

I refer to "text mode" as opposed to code written on a calculator or on FA-124, which uses keywords for operators and functions ("If", "Then", "List", "F-line"...).

## Files

The repository contains the following files.
In basic_parsing:
- basic_encoding.ml: contains lists of the encoding (in one or two bytes) of most of the commands and all the characters that exist on Casio calculator (Graph 35+), and the functions that generate two hash tables, containing the visual representation of the characters in text mode and graphic mode respectively
- basic_type.ml: contains the types for the abstract representation of Casio Basic code used for the emulator
- file_reader.ml: various file reading functions
- g1m_reader.ml: reading functions for extraction of the content of a G1M/G2M file and binary interpretation functions
- g1m_writer.ml: generation of G1M/G2M files
- arithmetic_parsing.ml: lexing and evaluation of arithmetic expressions (involving left and right-associative binary operators, left and right unary operators, functions)
- float_repr.ml: conversions from OCaml floats to Casio Basic numbers

In data:
- char1 ... char7.bmp, gphchar1.bmp, gphchar2.bmp: files that represent all the characters in text mode and graphic mode, whose representations are extracted and stored in the two hash tables (see basic_encoding.ml)

In picture_editor:
- bmp_reader.ml: simplified BMP file reader
- picture_drawer.ml: contains the graphic interface to draw and edit pictures
- picture_creator.ml: contains the functions and interface to convert RGB images to monochromatic 64*128 images through the Floyd-Steinberg dithering algorithm or the simple Threshold algorithm


## To do next
Compilers and decompilers:
- Code a lexer and a parser to convert G1M files to an intermediate abstract "Basic code" type
- Code a converter from Basic tree to readable "Custom Basic" code
- Code a lexer from Custom Basic code to list of lexemes (which can then be handled by the compiler
- Code a converter from Basic code type to binary, to write it into a G1M file

Emulation:
- Code all operators and functions
- Code all graphic functions: PlotOn, F-Line, DrawStat
- Code text display
- Code memory management (variables, lists, Str, ...)

Interface:
- Code an interface that opens G1M files
- Code a Picture viewer
- Code a PRGM menu that displays the available programs, and launches the emulator
- Code a Picture Edition general interface, that opens BMP files and writes G1M files
- Code an interface that displays all the buttons of the calculator, in order to interact with the programs, and a keybinds interface (for more convenient gameplay)
