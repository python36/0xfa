.include "boards/msp430g2553.inc"

.macro delay(cnt)
  mov #{cnt}, r8
 delay_loop:
  dec r8
  jnz delay_loop
.end_macro

{mem_code}:
  mov   #{mem_ram_end + 1}, r1 ; MUST HAVE

  mov   #{wdtctl_wdtpw + wdtctl_wdthold}, &{wdtctl} ; stop wdt

  mov.b &{cal_bc1_1mhz}, &{bcsctl1}
  mov.b &{cal_dco_1mhz}, &{dcoctl}

  bis.b #{pin6}, &{p1dir}

main_loop:
  delay(32000)
  xor.b #{pin6}, &{p1out}
  jmp main_loop

{reset_vector}: {mem_code}
