.macro mov_cactl1(ifg=0, ie=0, ies=0, on=0, ref=0, rsel=0, ex=0)
  mov.b #{ifg | (ie << cactl1_caie_pos) | (ies << cactl1_caies_pos) | (on << cactl1_caon_pos) |
    (ref << cactl1_caref_pos) | (rsel << cactl1_carsel_pos) | (ex << cactl1_caex_pos)}, &{cactl1}
.end_macro

.macro mov_cactl2(out=0, filter=0, p2ca0=0, p2ca123=0, p2ca4=0, short=0)
  mov.b #{out | (filter << cactl2_caf_pos) | (p2ca0 << cactl2_p2ca0_pos) | (p2ca123 << cactl2_p2ca1_pos) |
    (p2ca4 << cactl2_p2ca4_pos) | (short << cactl2_cashort_pos)}, &{cactl2}
.end_macro

.macro mov_capd(pd0=0, pd1=0, pd2=0, pd3=0, pd4=0, pd5=0, pd6=0, pd7=0)
  mov.b #{pd0 | (pd1 << capd_capd1_pos) | (pd2 << capd_capd2_pos) | (pd3 << capd_capd3_pos) |
    (pd4 << capd_capd4_pos) | (pd5 << capd_capd5_pos) | (pd6 << capd_capd6_pos) | (pd7 << capd_capd7_pos)}, &{capd}
.end_macro
