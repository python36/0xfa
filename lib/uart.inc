.macro uart_mov_uca0ctl0(sync=0, mode=0, spb=0, bit7=0, msb=0, par=0, pen=0)
  mov.b #{sync | (mode << ucactl0_ucmode_pos) | (spb << ucactl0_ucspb_pos) | (bit7 << ucactl0_uc7bit_pos) |
    (msb << ucactl0_ucmsb_pos) | (par << ucactl0_ucpar_pos) | (pen << ucactl0_ucpen_pos)}, &{uca0ctl0}
.end_macro

.macro uart_mov_uca0ctl1(swrst=1, txbrk=0, txaddr=0, dorm=0, brkie=0, rxeie=0, ssel=0)
  mov.b #{swrst | (txbrk << ucactl1_uctxbrk_pos) | (txaddr << ucactl1_uctxaddr_pos) | (dorm << ucactl1_ucdorm_pos) |
    (brkie << ucactl1_ucbrkie_pos) | (rxeie << ucactl1_ucrxeie_pos) | (ssel << ucactl1_ucssel_pos)}, &{uca0ctl1}
.end_macro

.macro uart_mov_uca0mctl(os16=0, brs=0, brf=0)
  mov.b #{os16 | (brs << ucamctl_ucbrs_pos) | (brf << ucamctl_ucbrf_pos)}, &{uca0mctl}
.end_macro

.macro uart_mov_uca0stat(busy=0, addr_idle=0, rxerr=0, brk=0, pe=0, oe=0, fe=0, listen=0)
  mov.b #{busy | (addr_idle << ucastat_ucaddr_pos) | (rxerr << ucastat_ucrxerr_pos) | (brk << ucastat_ucbrk_pos) |
    (pe << ucastat_ucpe_pos) | (oe << ucastat_ucoe_pos) | (fe << ucastat_ucfe_pos) | (listen << ucastat_uclisten_pos)}, &{uca0stat}
.end_macro