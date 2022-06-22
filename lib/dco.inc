.macro mov_dcoctl(mod=0, dco=0)
  mov.b #{mod | (dco << dcoctl_dco_pos)}, &{dcoctl}
.end_macro

.macro mov_bcsctl1(rsel=0, diva=0, xts=0, xt2off=0)
  mov.b #{rsel | (diva << bcsctl1_diva_pos) | (xts << bcsctl1_xts_pos) | (xt2off << bcsctl1_xt2off_pos)}, &{bcsctl1}
.end_macro

.macro mov_bcsctl2(dcor=0, divs=0, sels=0, divm=0, selm=0)
  mov.b #{dcor | (divs << bcsctl2_divs_pos) | (sels << bcsctl2_sels_pos) | (divm << bcsctl2_divm_pos) | (selm << bcsctl2_selm_pos)}, &{bcsctl2}
.end_macro

.macro mov_bcsctl3(lfxt1of=0, xt2of=0, xcap=0, lfxt1s=0, xt2s=0)
  mov.b #{lfxt1of | (xt2of << bcsctl3_xt2of_pos) | (xcap << bcsctl3_xcap_pos) | (lfxt1s << bcsctl3_lfxt1s_pos) | (xt2s << bcsctl3_xt2s_pos)}, &{bcsctl3}
.end_macro