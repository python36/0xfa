# a0xfa
msp430 assembler
# d0xfa
msp430 hex disassembler

## Doc
See examples/example.asm

## Build
```
mkdir build
gnatmake -gnat2012 -D build/ -Igetters/ a0xfa.adb
gnatmake -gnat2012 -D build/ d0xfa.adb
```

## Test
```
  ./a0xfa examples/blink.asm examples/blink.hex
```

## Usage
```
./a0xfa [path_to_asm_file].asm [path_to_hex_file].hex

./d0xfa [path_to_hex_file].hex
```
