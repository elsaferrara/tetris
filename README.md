# Tetris

SPARK tetris example for Raspberry Pico Display 

# Build and try the demo

Download the tool to convert ELF file to uf2 file
```console
$ git clone https://github.com/raspberrypi/pico-sdk.git
$ pico-sdk/tools/elf2uf2
$ mkdir build
$ cd build
$ cmake ..
$ make
```

Clone the repo and go to the main directory:
```console
$ git clone --recurse-submodules https://github.com/elsaferrara/tetris
$ cd tetris
```

Build with [Alire](https://alire.ada.dev) and convert to uf2 :
```console
$ alr build
$ mkdir bin
$ ~/pico-sdk/tools/elf2uf2/build/elf2uf2 -v obj/main bin/main.uf2
```

Then you have to plug the board while pressing the BOOTSEL button. After that you can put your file in the Raspberry Pico folder and play !


![image](demo.jpg)
