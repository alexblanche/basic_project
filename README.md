# Casio Basic emulator

This project contains an emulator of Casio Basic programs written on a Casio fx-9750GII (Graph 35+) / fx-9860GII (Graph 75). It aims at emulating the main functionalities of these systems.

Video demo on YouTube, on games I coded in high school:  

<!-- [![](https://markdown-videos-api.jorgenkh.no/youtube/lWjqz2yUfag)](https://youtu.be/lWjqz2yUfag) -->
<div align="center">
	<a href="http://www.youtube.com/watch?feature=player_embedded&v=lWjqz2yUfag" target="_blank">
        <img src="https://github.com/alexblanche/basic_project/blob/main/data/screen.png" alt="View on YouTube" width="80%" />
    </a>
</div>


<!-- ![Screen](https://github.com/alexblanche/basic_project/blob/main/data/screen.png) -->

## How it works

The program receives the name of a G1M or G2M file containing a project (made up of programs, pictures, captures, strings, lists and matrices) as argument. It extracts the content of the project and compiles the programs into an abstract code. The user can then select which programs to run via a menu.

### How to launch the emulator

- The project can be compiled with the provided makefile:
```
$ make
$ ./basic_emulator filename.g1m
```
Alternatively, use ```./basic_emulator --verbose filename.g1m``` to display compilation errors and runtime warnings.

- The project can also be interpreted and run at toplevel (REPL). The ```main.ml``` program launches all the subprograms.
```
$ ocaml
#use "src/main.ml";;
run "[filename].g1m";;
```
Alternatively, use ```run_verbose [filename].g1m``` to display compilation errors and runtime warnings.   

The project requires the library [OCamlSDL2](https://github.com/fccm/OCamlSDL2) in its latest version (as of october 2023), [OCamlSDL2_TTF](https://github.com/fccm/OCamlSDL2_TTF) and [SDL2](https://www.libsdl.org/) installed.  
To extract a G1M file from a Casio Basic program on a Casio calculator, see [FA-124](https://www.planet-casio.com/Fr/logiciels/voir_un_logiciel_casio.php?showid=16).

### Keybinds

The keybinds are as follows:
| Calculator | Keyboard |
| :-------- | :------- |
| ```0 ... 9```, ```+```, ```-```, ```*```, ```/```, ```Enter```| Keypad |
| ```↑ ↓ ← →``` | keyboard arrows |
| ```EXE```| Enter or keypad Enter |
| ```Shift``` | Left Shift |
| ```Alpha``` | Left Ctrl |
| ```A ... Z```, ```Space```, ```(```, ```)```, ```,``` | letters A-Z, Space, (, ), "," |
| ```F1 ... F6``` | F1-F6 |
| ```DEL``` | Backspace |
| ```MENU``` | Right Shift |
| ```EXIT``` | Delete |

Esc quits the emulator. Tab speeds up the emulation.

## Remarks
- The emulated Casio models are fx-9750GII (Graph 35+) / fx-9860GII (Graph 75). More modern models are not handled, and the compatibility with older models is not guaranteed.
- Only the main functionalities are implemented, some programs may not be supported or run as expected.
- Supported functionalities include: arithmetic (including complex numbers, list and matrix arithmetic, string functions), string display (including Locate), graphic display (PlotOn, F-Line, DrawStat), picture and capture dispay (RclPict, RclCapt, StoPict, BGPict), "?", menu.

### Games tested

The emulator was tested on the games I coded in 2011-2013, which can all be found on [Planet Casio](https://www.planet-casio.com/): [Timeless](https://www.planet-casio.com/Fr/programmes/programme2007-1-timeless-alex-1186-jeux-reflexion.html), [Timeless Remix](https://www.planet-casio.com/Fr/programmes/programme2228-last-timeless-remix-alex-1186-jeux-reflexion.html), [Super Run & Jump](https://www.planet-casio.com/Fr/programmes/programme2156-1-super-run-jump-alex-1186-jeux-actionsport.html), [Ace Combat](https://www.planet-casio.com/Fr/programmes/programme1960-1-ace-combat-alex-1186-jeux-directiontir.html), [Airwolf](https://www.planet-casio.com/Fr/programmes/programme2240-1-airwolf-alex-1186-jeux-directiontir.html), and the great [CloneLab](https://www.planet-casio.com/Fr/programmes/programme1984-1-clonelab-ne0tux-jeux-reflexion.html) by Ne0tux.

Do not hesitate to open an issue if you encounter an error when running a program (highly probable), or if the program does not open the G1M file (even more probable).


# Other tools

This project also contains:
- A graphical interface to draw and modify monochromatic images
- BMP image conversion to monochromatic image through the Floyd-Steinberg dithering algorithm or the simple threshold algorithm
<img src="https://github.com/alexblanche/basic_project/blob/main/data/mono_conv.jpg" width="95%">

- A general G1M/G2M file reader/writer: extraction/generation of all types of objects (programs, lists, matrices, pictures, captures, strings) from/into a G1M file that can then be transfered to a Casio calculator

These tools are present but no interface has been added yet. I will implement one in the future.


## To come next
- A converter from abstract Basic code to readable "Custom Basic" code
- A lexer of Custom Basic into a list of lexemes, that can then be handled by the emulator
- A Picture edition general interface, that opens BMP files and writes G1M files
