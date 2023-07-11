# Basic Project

## Goals

In this project I aim at developing tools related to Casio calculators: (everything is work in progress)
- A Casio Basic interpreter, which reads G1M files including Pictures
- A "compiler" that converts Casio Basic code written in text mode (with spaces and indentation) into a G1M file that runs on a Casio calculator (targetted model: Graph 35+)
- A picture editor, to draw monochromatic 64*128 pictures or convert existing BMP images into pictures and write them in a G1M file

## Files

The repository contains the following files:
- picture_editor.ml: contains the graphic interface to draw and modify pictures
- bmp_reader.ml: simplified BMP file reader
- floyd-steinberg.ml: (TODO) implementation of the Floyd-Steinberg dithering algorithm, to convert RGB images to monochromatic 64*128 pictures
