.macro spi_mov_uca0ctl0(sync=1, mode=0, mst=0, bit7=0, msb=0, ckpl=0, ckph=0)
  mov.b #{sync | (mode << ucactl0_ucmode_pos) | (mst << ucactl0_ucmst_pos) | (bit7 << ucactl0_uc7bit_pos) |
    (msb << ucactl0_ucmsb_pos) | (ckpl << ucactl0_ckpl_pos) | (ckph << ucactl0_ckph_pos)}, &{uca0ctl0}
.end_macro

.macro spi_mov_uca0ctl1(swrst=1, ssel=0)
  mov.b #{swrst | (ssel << ucactl1_ucssel_pos)}, &{uca0ctl1}
.end_macro

.macro spi_mov_uca0stat(oe=0, fe=0, listen=0)
  mov.b #{(oe << ucastat_ucoe_pos) | (fe << ucastat_ucfe_pos) | (listen << ucastat_uclisten_pos)}, &{ucb0stat}
.end_macro

.macro spi_mov_ucb0ctl0(sync=1, mode=0, mst=0, bit7=0, msb=0, ckpl=0, ckph=0)
  mov.b #{sync | (mode << ucbctl0_ucmode_pos) | (mst << ucbctl0_ucmst_pos) | (bit7 << ucbctl0_uc7bit_pos) |
    (msb << ucbctl0_ucmsb_pos) | (ckpl << ucbctl0_ckpl_pos) | (ckph << ucbctl0_ckph_pos)}, &{ucb0ctl0}
.end_macro

.macro spi_mov_ucb0ctl1(swrst=1, ssel=0)
  mov.b #{swrst | (ssel << ucbctl1_ucssel_pos)}, &{ucb0ctl1}
.end_macro

.macro spi_mov_ucb0stat(oe=0, fe=0, listen=0)
  mov.b #{(oe << ucbstat_ucoe_pos) | (fe << ucbstat_ucfe_pos) | (listen << ucbstat_uclisten_pos)}, &{ucb0stat}
.end_macro