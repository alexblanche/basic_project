# Basic Project

## Goals

In this project I aim at developing tools related to Casio calculators: (everything is work in progress)
- A Casio Basic interpreter (or compiler), which reads G1M files, including Pictures, matrices, ...
- A "compiler" that converts Casio Basic code written in text mode (with spaces and indentation) into a G1M file that runs on a Casio calculator (targetted model: Graph 35+)
- A "decompiler" that generates indented code in text mode from a G1M file containing Casio Basic code
- A picture editor, to draw monochromatic 64*128 pictures or convert existing BMP images into pictures and write them in a G1M file

I refer to "text mode" as opposed to code written on a calculator or on FA-124, which uses keywords for operators and functions ("If", "Then", "List", "F-line"...).

## Files

The repository contains the following files:
- bmp_reader.ml: simplified BMP file reader
- picture_drawer.ml: contains the graphic interface to draw and edit pictures
- picture_creator.ml: contains the functions and interface to convert RGB images to monochromatic 64*128 images through the Floyd-Steinberg dithering algorithm or the simple Threshold algorithm
- file_to_string.ml: various file reading functions

### To do next
Compilers and decompilers:
- Code a lexer and a parser with Lex, Yacc to convert G1M files to an intermediate abstract "Basic tree" type
- Code a converter from Basic tree to readable Basic code (custom format ".cb")
- Code a converter from readable Basic code to Basic tree type (using Lex, Yacc)
- Code a converter from Basic tree type to binary, and writes it into a G1M file

Picture conversions:
- Code a converter from boolean matrix to G1M picture file
- Code a converter from G1M picture file to boolean matrix

Emulation:
- Code all operators and functions: <-, Display, If, While, Goto, And, Or, ...
- Code all graphic functions: PlotOn, F-Line, DrawStat
- Encode all the characters, in text and graphic fonts
- Code text display
- Code memory management (variables, lists, Str, ...)

Interface:
- Code an interface that opens G1M files
- Code a Picture viewer
- Code a PRGM menu that displays the available programs, and launches the emulator
- Code a Picture Edition general interface, that opens BMP files and writes G1M files