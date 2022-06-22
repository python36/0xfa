.macro mov_usictl0(swrst=1, oe=0, ge=0, mst=0, lsb=0, pe5=0, pe6=0, pe7=0)
  mov.b #{swrst | (oe << usictl0_usioe_pos) | (ge << usictl0_usige_pos) | (mst << usictl0_usimst_pos) | (lsb << usictl0_usilsb_pos) |
    (pe5 << usictl0_usipe5_pos) | (pe6 << usictl0_usipe6_pos) | (pe7 << usictl0_usipe7_pos)}, &{usictl0}
.end_macro

.macro mov_usictl1(ifg=1, sttifg=0, stp=0, al=0, ie=0, sttie=0, i2c=0, ckph=0)
  mov.b #{ifg | (sttifg << usictl1_usisttifg_pos) | (stp << usictl1_usistp_pos) | (al << usictl1_usial_pos) |
    (ie << usictl1_usiie_pos) | (sttie << usictl1_usisttie_pos) | (i2c << usictl1_usii2c_pos) | (ckph << usictl1_usickph_pos)}, &{usictl1}
.end_macro

.macro mov_usickctl(swclk=0, ckpl=0, ssel=0, div=0)
  mov.b #{swclk | (ckpl << usickctl_usickpl_pos) | (ssel << usickctl_usissel_pos) | (div << usickctl_usidiv_pos)}, &{usickctl}
.end_macro

.macro mov_usicnt(cnt=0, ifgcc=0, s16b=0, sclrel=0)
  mov.b #{cnt | (ifgcc << usicnt_usiifgcc_pos) | (s16b << usicnt_usi16b_pos) | (sclrel << usicnt_usisclrel_pos)}, &{usicnt}
.end_macro
