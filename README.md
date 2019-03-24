# a0xfa
msp430 assembler
# d0xfa
msp430 hex disassembler

## Doc
See examples/example.asm

## Build
```
mkdir build
bash build.sh
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
