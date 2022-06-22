.macro mov_ucb0ctl0(sync=1, mode=0, mst=0, mm=0, sla10=0, a10=0)
  mov.b #{sync | (mode << ucbctl0_ucmode_pos) | (mst << ucbctl0_ucmst_pos) |
    (mm << ucbctl0_ucmm_pos) | (sla10 << ucbctl0_ucsla10_pos) | (a10 << ucbctl0_uca10_pos)}, &{ucb0ctl0}
.end_macro

.macro mov_ucb0ctl1(swrst=1, txstt=0, txstp=0, txnack=0, tr=0, ssel=0)
  mov.b #{swrst | (txstt << ucbctl1_uctxstt_pos) | (txstp << ucbctl1_uctxstp_pos) |
    (txnack << ucbctl1_uctxnack_pos) | (tr << ucbctl1_uctr_pos) | (ssel << ucbctl1_ucssel_pos)}, &{ucb0ctl1}
.end_macro

.macro mov_ucb0stat(alifg=0, sttifg=0, stpifg=0, nackifg=0, bbusy=0, gs=0, scllow=0)
  mov.b #{alifg | (sttifg << ucbstat_ucsttifg_pos) | (stpifg << ucbstat_ucstpifg_pos) | (nackifg << ucbstat_ucnackifg_pos) |
    (bbusy << ucbstat_ucbbusy_pos) | (gs << ucbstat_ucgc_pos) | (scllow << ucbstat_ucscllow_pos)}, &{ucb0stat}
.end_macro

.macro ucb0i2cie(i2coa=0, gcen=0)
  mov.b #{i2coa | (gcen << ucbi2coa_ucgen_pos)}, &{ucb0i2cie}
.end_macro
