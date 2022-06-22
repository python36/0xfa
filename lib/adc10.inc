.macro mov_adc10ctl0(sc=0, enc=0, ifg=0, ie=0, on=0, refon=0, ref2_5v=0, msc=0, refburst=0, refout=0, sr=0, sht=0, sref=0)
  mov #{sc | (enc << adc10ctl0_enc_pos) | (ifg << adc10ctl0_adc10ifg_pos) | (ie << adc10ctl0_adc10ie_pos) |
    (on << adc10ctl0_adc10on_pos) | (refon << adc10ctl0_refon_pos) | (ref2_5v << adc10ctl0_ref2_5v_pos) |
    (msc << adc10ctl0_msc_pos) | (refburst << adc10ctl0_refburst_pos) | (refout << adc10ctl0_refout) |
    (sr << adc10ctl0_adc10sr_pos) | (sht << adc10ctl0_adc10sht_pos) | (sref << adc10ctl0_sref_pos)} , &{adc10ctl0}
.end_macro

.macro mov_adc10ctl1(conseq=0, ssel=0, div=0, issh=0, df=0, shs=0, inch=0)
  mov #{(conseq << adc10ctl1_conseq_pos) | (ssel << adc10ctl1_adc10ssel_pos) | (div << adc10ctl1_adc10div_pos) | (issh << adc10ctl1_issh_pos) |
    (df << adc10ctl1_adc10df_pos) | (shs << adc10ctl1_shs_pos) | (inch << adc10ctl1_inch_pos)} , &{adc10ctl1}
.end_macro

.macro mov_adc10dtc0(fetch=0, ct=0, tb=0)
  mov.b #{fetch | (ct << adc10dtc0_adc10ct_pos) | (tb << adc10dtc0_adc10tb_pos)} , &{adc10dtc0}
.end_macro
