.macro mov_ta0ctl(ifg=0, ie=0, clr=0, mc=0, id=0, ssel=0)
  mov #{ifg | (ie << tactl_taie_pos) | (clr << tactl_taclr_pos) | (mc << tactl_mc_pos) |
    (id << tactl_id_pos) | (ssel << tactl_tassel_pos)}, &{ta0ctl}
.end_macro

.macro mov_ta1ctl(ifg=0, ie=0, clr=0, mc=0, id=0, ssel=0)
  mov #{ifg | (ie << tactl_taie_pos) | (clr << tactl_taclr_pos) | (mc << tactl_mc_pos) |
    (id << tactl_id_pos) | (ssel << tactl_tassel_pos)}, &{ta1ctl}
.end_macro


.macro mov_ta0cctl0(ccifg=0, cov=0, out=0, cci=0, ccie=0, outmod=0, cap=0, scci=0, scs=0, ccis=0, cm=0)
  mov #{ccifg | (cov << tacctl_cov_pos) | (out << tacctl_out_pos) | (cci << tacctl_cci_pos) |
    (ccie << tacctl_ccie_pos) | (outmod << tacctl_outmod_pos) | (cap << tacctl_cap_pos) |
    (scci << tacctl_scci_pos) | (scs << tacctl_scs_pos) | (ccis << tacctl_ccis_pos) | (cm << tacctl_cm_pos)}, &{ta0cctl0}
.end_macro

.macro mov_ta0cctl1(ccifg=0, cov=0, out=0, cci=0, ccie=0, outmod=0, cap=0, scci=0, scs=0, ccis=0, cm=0)
  mov #{ccifg | (cov << tacctl_cov_pos) | (out << tacctl_out_pos) | (cci << tacctl_cci_pos) |
    (ccie << tacctl_ccie_pos) | (outmod << tacctl_outmod_pos) | (cap << tacctl_cap_pos) |
    (scci << tacctl_scci_pos) | (scs << tacctl_scs_pos) | (ccis << tacctl_ccis_pos) | (cm << tacctl_cm_pos)}, &{ta0cctl1}
.end_macro

.macro mov_ta0cctl2(ccifg=0, cov=0, out=0, cci=0, ccie=0, outmod=0, cap=0, scci=0, scs=0, ccis=0, cm=0)
  mov #{ccifg | (cov << tacctl_cov_pos) | (out << tacctl_out_pos) | (cci << tacctl_cci_pos) |
    (ccie << tacctl_ccie_pos) | (outmod << tacctl_outmod_pos) | (cap << tacctl_cap_pos) |
    (scci << tacctl_scci_pos) | (scs << tacctl_scs_pos) | (ccis << tacctl_ccis_pos) | (cm << tacctl_cm_pos)}, &{ta0cctl2}
.end_macro


.macro mov_ta1cctl0(ccifg=0, cov=0, out=0, cci=0, ccie=0, outmod=0, cap=0, scci=0, scs=0, ccis=0, cm=0)
  mov #{ccifg | (cov << tacctl_cov_pos) | (out << tacctl_out_pos) | (cci << tacctl_cci_pos) |
    (ccie << tacctl_ccie_pos) | (outmod << tacctl_outmod_pos) | (cap << tacctl_cap_pos) |
    (scci << tacctl_scci_pos) | (scs << tacctl_scs_pos) | (ccis << tacctl_ccis_pos) | (cm << tacctl_cm_pos)}, &{ta1cctl0}
.end_macro

.macro mov_ta1cctl1(ccifg=0, cov=0, out=0, cci=0, ccie=0, outmod=0, cap=0, scci=0, scs=0, ccis=0, cm=0)
  mov #{ccifg | (cov << tacctl_cov_pos) | (out << tacctl_out_pos) | (cci << tacctl_cci_pos) |
    (ccie << tacctl_ccie_pos) | (outmod << tacctl_outmod_pos) | (cap << tacctl_cap_pos) |
    (scci << tacctl_scci_pos) | (scs << tacctl_scs_pos) | (ccis << tacctl_ccis_pos) | (cm << tacctl_cm_pos)}, &{ta1cctl1}
.end_macro

.macro mov_ta1cctl2(ccifg=0, cov=0, out=0, cci=0, ccie=0, outmod=0, cap=0, scci=0, scs=0, ccis=0, cm=0)
  mov #{ccifg | (cov << tacctl_cov_pos) | (out << tacctl_out_pos) | (cci << tacctl_cci_pos) |
    (ccie << tacctl_ccie_pos) | (outmod << tacctl_outmod_pos) | (cap << tacctl_cap_pos) |
    (scci << tacctl_scci_pos) | (scs << tacctl_scs_pos) | (ccis << tacctl_ccis_pos) | (cm << tacctl_cm_pos)}, &{ta1cctl2}
.end_macro