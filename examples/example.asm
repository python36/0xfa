; its comment

; comma ~ space
; mov r1,r2 ~ mov,r1,r2 ~ mov r1 r2 ~ mov,r1 r2

; instructions:

.include "mcu/msp430g2553.inc"

.def var0 1 ; word type

.defs
  var1 10 ; word type
  var2 3 ; word type
.end_defs
.def var2 4 ; warning

.def var00 0
.undef var00
.undef var000 ; warning

.set var2 {2 << 4}

.macro m1(param1 param2)
  add {param1 + param2}, r4
.end_macro

m1(1 2) ; add 3 r4

; ---

; set address
10: ; or
0xa: ; or
{var1}:

; label
label:

; preproc:
; {vars operations ...} or {var}
; operations:
  ; ** (pow); highest priority
  ; * / % (mod)
  ; + -
  ; >> <<
  ; & (and)
  ; ^ (xor)
  ; | (or)

; example
add {var0 * ((2 + var2) / 4) << 3}, r4

; raw data
; example:
table: 1 2 4 8 16 32 64
mov #table, r7
add 4, r7
mov @r7, r8
msg: "hello world"
mov #msg, r10
add 4, r10
mov '!', @r10