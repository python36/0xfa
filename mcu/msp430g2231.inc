.defs
; memory map
  mem_code     0xf800
  mem_code_end 0xffbf
;; info
  mem_info     0x1000
  mem_info_end 0x10ff

  mem_info_segment_a     0x10c0
  mem_info_segment_a_end 0x1100
  mem_info_segment_b     0x1080
  mem_info_segment_b_end 0x10c0
  mem_info_segment_c     0x1040
  mem_info_segment_c_end 0x1080
  mem_info_segment_d     0x1000
  mem_info_segment_d_end 0x1040
;; ram
  mem_ram      0x200
  mem_ram_end  0x27f
; pins
  pin0 1
  pin1 2
  pin2 4
  pin3 8
  pin4 16
  pin5 32
  pin6 64
  pin7 128
; sr
  c      1
  z      2
  n      4
  v      0x100
  gie    8
  cpuoff 0x10
  oscoff 0x20
  scg0   0x40
  scg1   0x80
; hardware multiplier
  ; mul_mpy    0x130
  ; mul_mpys   0x132
  ; mul_mac    0x134
  ; mul_macs   0x136
  ; mul_op2    0x138
  ; mul_reslo  0x13a
  ; mul_reshi  0x13c
  ; mul_sumext 0x13e
; interrupt vectors
  reset_vector       0xfffe ; highest priority
  nmi_vector         0xfffc
  ; timer1_a0_vector   0xfffa
  ; timer1_a1_vector   0xfff8
  ; comparatora_vector 0xfff6
  wdt_vector         0xfff4
  timer0_a0_vector   0xfff2
  timer0_a1_vector   0xfff0
  ; usciab0rx_vector   0xffee
  ; usciab0tx_vector   0xffec
  adc10_vector       0xffea
  usi_vector         0xffe8
  port2_vector       0xffe6
  port1_vector       0xffe4
  ; pass             0xffe2
  ; pass             0xffe0
  bslskey            0xffde
  ; not used         0xffda..0xffc0
; special function
  ie1 0 ; .b
    wdtie  1
    ofie   2
    nmie   {2 ** 4}
    accvie {2 ** 5}
  ; ie2 1 ; .b
  ;   uca0rxie 1
  ;   uca0txie 2
  ;   ucb0rxie 4
  ;   ucb0txie 8
  ifg1 2 ; .b
    wdtifg 1
    ofifg  2
    porifg 4
    rstifg 8
    nmiifg 16
  ; ifg2 3 ; .b
  ;   uca0rxifg 1
  ;   uca0txifg 2
  ;   ucb0rxifg 4
  ;   ucb0txifg 8
; calibration data
  tag_dco_30 0x10f6
    cal_bc1_1mhz_offset  9
    cal_dco_1mhz_offset  8
    ; cal_bc1_8mhz_offset  7
    ; cal_dco_8mhz_offset  6
    ; cal_bc1_12mhz_offset 5
    ; cal_dco_12mhz_offset 4
    ; cal_bc1_16mhz_offset 3
    ; cal_dco_16mhz_offset 2
    cal_bc1_1mhz  {tag_dco_30 + cal_bc1_1mhz_offset}
    cal_dco_1mhz  {tag_dco_30 + cal_dco_1mhz_offset}
    ; cal_bc1_8mhz  {tag_dco_30 + cal_bc1_8mhz_offset}
    ; cal_dco_8mhz  {tag_dco_30 + cal_dco_8mhz_offset}
    ; cal_bc1_12mhz {tag_dco_30 + cal_bc1_12mhz_offset}
    ; cal_dco_12mhz {tag_dco_30 + cal_dco_12mhz_offset}
    ; cal_bc1_16mhz {tag_dco_30 + cal_bc1_16mhz_offset}
    ; cal_dco_16mhz {tag_dco_30 + cal_dco_16mhz_offset}
  ; tag_adc10_1 0x10da
  ;   cal_adc_25t85_offset         0x10
  ;   cal_adc_25t30_offset         0xe
  ;   cal_adc_25vref_factor_offset 0xc
  ;   cal_adc_15t85_offset         0xa
  ;   cal_adc_15t30_offset         8
  ;   cal_adc_15vref_factor_offset 6
  ;   cal_adc_offset_offset        4
  ;   cal_adc_gain_factor_offset   2
  ;   cal_adc_25t85         {tag_adc10_1 + cal_adc_25t85_offset}
  ;   cal_adc_25t30         {tag_adc10_1 + cal_adc_25t30_offset}
  ;   cal_adc_25vref_factor {tag_adc10_1 + cal_adc_25vref_factor_offset}
  ;   cal_adc_15t85         {tag_adc10_1 + cal_adc_15t85_offset}
  ;   cal_adc_15t30         {tag_adc10_1 + cal_adc_15t30_offset}
  ;   cal_adc_15vref_factor {tag_adc10_1 + cal_adc_15vref_factor_offset}
  ;   cal_adc_offset        {tag_adc10_1 + cal_adc_offset_offset}
  ;   cal_adc_gain_factor   {tag_adc10_1 + cal_adc_gain_factor_offset}
; timers
  ;tactl
    tactl_tassel_pos 8
      tactl_tassel_0 {0 << tactl_tassel_pos}
      tactl_tassel_1 {1 << tactl_tassel_pos}
      tactl_tassel_2 {2 << tactl_tassel_pos}
      tactl_tassel_3 {3 << tactl_tassel_pos}
    tactl_id_pos     6
      tactl_id_0 {0 << tactl_id_pos}
      tactl_id_1 {1 << tactl_id_pos}
      tactl_id_2 {2 << tactl_id_pos}
      tactl_id_3 {3 << tactl_id_pos}
    tactl_mc_pos     4
      tactl_mc_0 {0 << tactl_mc_pos}
      tactl_mc_1 {1 << tactl_mc_pos}
      tactl_mc_2 {2 << tactl_mc_pos}
      tactl_mc_3 {3 << tactl_mc_pos}
    tactl_taclr_pos  2
      tactl_taclr {1 << tactl_taclr_pos}
    tactl_taie_pos   1
      tactl_taie {1 << tactl_taie_pos}
    tactl_taifg_pos  0
      tactl_taifg {1 << tactl_taifg_pos}
  ; tacctl
    tacctl_cm_pos     14
      tacctl_cm_0 {0 << tacctl_cm_pos}
      tacctl_cm_1 {1 << tacctl_cm_pos}
      tacctl_cm_2 {2 << tacctl_cm_pos}
      tacctl_cm_3 {3 << tacctl_cm_pos}
    tacctl_ccis_pos   12
      tacctl_ccis_0 {0 << tacctl_ccis_pos}
      tacctl_ccis_1 {1 << tacctl_ccis_pos}
      tacctl_ccis_2 {2 << tacctl_ccis_pos}
      tacctl_ccis_3 {3 << tacctl_ccis_pos}
    tacctl_scs_pos    11
      tacctl_scs {1 << tacctl_scs_pos}
    tacctl_scci_pos   10
      tacctl_scci {1 << tacctl_scci_pos}
    tacctl_cap_pos    8
      tacctl_cap {1 << tacctl_cap_pos}
    tacctl_outmod_pos 5
      tacctl_outmod_0 {0 << tacctl_outmod_pos}
      tacctl_outmod_1 {1 << tacctl_outmod_pos}
      tacctl_outmod_2 {2 << tacctl_outmod_pos}
      tacctl_outmod_3 {3 << tacctl_outmod_pos}
      tacctl_outmod_4 {4 << tacctl_outmod_pos}
      tacctl_outmod_5 {5 << tacctl_outmod_pos}
      tacctl_outmod_6 {6 << tacctl_outmod_pos}
      tacctl_outmod_7 {7 << tacctl_outmod_pos}
    tacctl_ccie_pos   4
      tacctl_ccie {1 << tacctl_ccie_pos}
    tacctl_cci_pos    3
      tacctl_cci {1 << tacctl_cci_pos}
    tacctl_out_pos    2
      tacctl_out {1 << tacctl_out_pos}
    tacctl_cov_pos    1
      tacctl_cov {1 << tacctl_cov_pos}
    tacctl_ccifg_pos  0
      tacctl_ccifg {1 << tacctl_ccifg_pos}
; timer0_a3
  ta0iv    0x12e
  ta0ctl   0x160
  ta0cctl0 0x162
  ta0cctl1 0x164
  ta0r     0x170
  ta0ccr0  0x172
  ta0ccr1  0x174
  ; ta0ccr2  0x176
; timer1_a3
  ; ta1iv    0x11e
  ; ta1ctl   0x180
  ; ta1cctl0 0x182
  ; ta1cctl1 0x184
  ; ta1cctl2 0x186
  ; ta1r     0x190
  ; ta1ccr0  0x192
  ; ta1ccr1  0x194
  ; ta1ccr2  0x196
; adc10
  adc10ctl0 0x1b0
    adc10ctl0_sref_pos     13
      adc10ctl0_sref_0 {0 << adc10ctl0_sref_pos}
      adc10ctl0_sref_1 {1 << adc10ctl0_sref_pos}
      adc10ctl0_sref_2 {2 << adc10ctl0_sref_pos}
      adc10ctl0_sref_3 {3 << adc10ctl0_sref_pos}
      adc10ctl0_sref_4 {4 << adc10ctl0_sref_pos}
      adc10ctl0_sref_5 {5 << adc10ctl0_sref_pos}
      adc10ctl0_sref_6 {6 << adc10ctl0_sref_pos}
      adc10ctl0_sref_7 {7 << adc10ctl0_sref_pos}
    adc10ctl0_adc10sht_pos 11
      adc10ctl0_adc10sht_0 {0 << adc10ctl0_adc10sht_pos}
      adc10ctl0_adc10sht_1 {1 << adc10ctl0_adc10sht_pos}
      adc10ctl0_adc10sht_2 {2 << adc10ctl0_adc10sht_pos}
      adc10ctl0_adc10sht_3 {3 << adc10ctl0_adc10sht_pos}
    adc10ctl0_adc10sr_pos  10
      adc10ctl0_adc10sr {1 << adc10ctl0_adc10sr_pos}
    adc10ctl0_refout_pos   9
      adc10ctl0_refout {1 << adc10ctl0_refout_pos}
    adc10ctl0_refburst_pos 8
      adc10ctl0_refburst {1 << adc10ctl0_refburst_pos}
    adc10ctl0_msc_pos      7
      adc10ctl0_msc {1 << adc10ctl0_msc_pos}
    adc10ctl0_ref2_5v_pos  6
      adc10ctl0_ref2_5v {1 << adc10ctl0_ref2_5v_pos}
    adc10ctl0_refon_pos    5
      adc10ctl0_refon {1 << adc10ctl0_refon_pos}
    adc10ctl0_adc10on_pos  4
      adc10ctl0_adc10on {1 << adc10ctl0_adc10on_pos}
    adc10ctl0_adc10ie_pos  3
      adc10ctl0_adc10ie {1 << adc10ctl0_adc10ie_pos}
    adc10ctl0_adc10ifg_pos 2
      adc10ctl0_adc10ifg {1 << adc10ctl0_adc10ifg_pos}
    adc10ctl0_enc_pos      1
      adc10ctl0_enc {1 << adc10ctl0_enc_pos}
    adc10ctl0_adc10sc_ps   0
      adc10ctl0_adc10sc {1 << adc10ctl0_adc10sc_ps}
  adc10ctl1 0x1b2
    adc10ctl1_inch_pos      12
      adc10ctl1_inch_0  {0 << adc10ctl1_inch_pos}
      adc10ctl1_inch_1  {1 << adc10ctl1_inch_pos}
      adc10ctl1_inch_2  {2 << adc10ctl1_inch_pos}
      adc10ctl1_inch_3  {3 << adc10ctl1_inch_pos}
      adc10ctl1_inch_4  {4 << adc10ctl1_inch_pos}
      adc10ctl1_inch_5  {5 << adc10ctl1_inch_pos}
      adc10ctl1_inch_6  {6 << adc10ctl1_inch_pos}
      adc10ctl1_inch_7  {7 << adc10ctl1_inch_pos}
      adc10ctl1_inch_8  {8 << adc10ctl1_inch_pos}
      adc10ctl1_inch_9  {9 << adc10ctl1_inch_pos}
      adc10ctl1_inch_10 {10 << adc10ctl1_inch_pos}
      adc10ctl1_inch_11 {11 << adc10ctl1_inch_pos}
      adc10ctl1_inch_12 {12 << adc10ctl1_inch_pos}
      adc10ctl1_inch_13 {13 << adc10ctl1_inch_pos}
      adc10ctl1_inch_14 {14 << adc10ctl1_inch_pos}
      adc10ctl1_inch_15 {15 << adc10ctl1_inch_pos}
    adc10ctl1_shs_pos       10
      adc10ctl1_shs_0 {0 << adc10ctl1_shs_pos}
      adc10ctl1_shs_1 {1 << adc10ctl1_shs_pos}
      adc10ctl1_shs_2 {2 << adc10ctl1_shs_pos}
      adc10ctl1_shs_3 {3 << adc10ctl1_shs_pos}
    adc10ctl1_adc10df_pos   9
      adc10ctl1_adc10df {1 << adc10ctl1_adc10df_pos}
    adc10ctl1_issh_pos      8
      adc10ctl1_issh {1 << adc10ctl1_issh_pos}
    adc10ctl1_adc10div_pos  5
      adc10ctl1_adc10div_0 {0 << adc10ctl1_adc10div_pos}
      adc10ctl1_adc10div_1 {1 << adc10ctl1_adc10div_pos}
      adc10ctl1_adc10div_2 {2 << adc10ctl1_adc10div_pos}
      adc10ctl1_adc10div_3 {3 << adc10ctl1_adc10div_pos}
      adc10ctl1_adc10div_4 {4 << adc10ctl1_adc10div_pos}
      adc10ctl1_adc10div_5 {5 << adc10ctl1_adc10div_pos}
      adc10ctl1_adc10div_6 {6 << adc10ctl1_adc10div_pos}
      adc10ctl1_adc10div_7 {7 << adc10ctl1_adc10div_pos}
    adc10ctl1_adc10ssel_pos 3
      adc10ctl1_adc10ssel_0 {0 << adc10ctl1_adc10ssel_pos}
      adc10ctl1_adc10ssel_1 {1 << adc10ctl1_adc10ssel_pos}
      adc10ctl1_adc10ssel_2 {2 << adc10ctl1_adc10ssel_pos}
      adc10ctl1_adc10ssel_3 {3 << adc10ctl1_adc10ssel_pos}
    adc10ctl1_conseq_pos    1
      adc10ctl1_conseq_0 {0 << adc10ctl1_conseq_pos}
      adc10ctl1_conseq_1 {1 << adc10ctl1_conseq_pos}
      adc10ctl1_conseq_2 {2 << adc10ctl1_conseq_pos}
      adc10ctl1_conseq_3 {3 << adc10ctl1_conseq_pos}
    adc10ctl1_adc10busy_pos 0
      adc10ctl1_adc10busy   {1 << adc10ctl1_adc10busy_pos}
  adc10mem  0x1b4
  adc10sa   0x1bc
  adc10dtc0 0x48 ; .b
    adc10dtc0_adc10tb_pos    3
      adc10dtc0_adc10tb    8
    adc10dtc0_adc10ct_pos    2
      adc10dtc0_adc10ct    4
    adc10dtc0_adc10b1_pos    1
      adc10dtc0_adc10b1    2
    adc10dtc0_adc10fetch_pos 0
      adc10dtc0_adc10fetch 1
  adc10dtc1 0x49 ; .b
  adc10ae0  0x4a ; .b
  ; adc10ae1  0x4b ; .b
; supervisor
  svsctl 0x56
    svsctl_vld_pos   4
      svsctl_vld_0  {0 << svsctl_vld_pos}
      svsctl_vld_1  {1 << svsctl_vld_pos}
      svsctl_vld_2  {2 << svsctl_vld_pos}
      svsctl_vld_3  {3 << svsctl_vld_pos}
      svsctl_vld_4  {4 << svsctl_vld_pos}
      svsctl_vld_5  {5 << svsctl_vld_pos}
      svsctl_vld_6  {6 << svsctl_vld_pos}
      svsctl_vld_7  {7 << svsctl_vld_pos}
      svsctl_vld_8  {8 << svsctl_vld_pos}
      svsctl_vld_9  {9 << svsctl_vld_pos}
      svsctl_vld_10 {10 << svsctl_vld_pos}
      svsctl_vld_11 {11 << svsctl_vld_pos}
      svsctl_vld_12 {12 << svsctl_vld_pos}
      svsctl_vld_13 {13 << svsctl_vld_pos}
      svsctl_vld_14 {14 << svsctl_vld_pos}
      svsctl_vld_15 {15 << svsctl_vld_pos}
    svsctlporon_pos 3
      svsctlporon {1 << svsctlporon_pos}
    svsctlsvson_pos 2
      svsctlsvson {1 << svsctlsvson_pos}
    svsctlsvsop_pos 1
      svsctlsvsop {1 << svsctlsvsop_pos}
    svsctlsvsfg_pos 0
      svsctlsvsfg {1 << svsctlsvsfg_pos}
; watchdog timer
  wdtctl  0x120
    wdtctl_wdtpw_pos 8
      wdtctl_wdtpw 0x5a00
    wdtctl_wdtpr_pos 8
      wdtctl_wdtpr  0x6900
    wdtctl_wdthold_pos  7
      wdtctl_wdthold {1 << wdtctl_wdthold_pos}
    wdtctl_wdtnmies_pos 6
      wdtctl_wdtnmies {1 << wdtctl_wdtnmies_pos}
    wdtctl_wdtnmi_pos   5
      wdtctl_wdtnmi {1 << wdtctl_wdtnmi_pos}
    wdtctl_wdttmsel_pos 4
      wdtctl_wdttmsel {1 << wdtctl_wdttmsel_pos}
    wdtctl_wdtcntcl_pos 2
      wdtctl_wdtcntcl {1 << wdtctl_wdtcntcl_pos}
    wdtctl_wdtssel_pos  2
      wdtctl_wdtssel {1 << wdtctl_wdtssel_pos}
    wdtctl_wdtis_pos    0
      wdtctl_wdtis_0 {0 << wdtctl_wdtis_pos}
      wdtctl_wdtis_1 {1 << wdtctl_wdtis_pos}
      wdtctl_wdtis_2 {2 << wdtctl_wdtis_pos}
      wdtctl_wdtis_3 {3 << wdtctl_wdtis_pos}
; not used in msp430g2553
; dma
  ; dmactl0 0x122
  ;   dmactl0_dma2tsel_pos 8
  ;     dmactl0_dma2tsel_0  {0 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_1  {1 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_2  {2 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_3  {3 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_4  {4 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_5  {5 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_6  {6 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_7  {7 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_8  {8 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_9  {9 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_10 {10 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_11 {11 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_12 {12 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_13 {13 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_14 {14 << dmactl0_dma2tsel_pos}
  ;     dmactl0_dma2tsel_15 {15 << dmactl0_dma2tsel_pos}
  ;   dmactl0_dma1tsel_pos 4
  ;     dmactl0_dma1tsel_0  {0 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_1  {1 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_2  {2 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_3  {3 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_4  {4 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_5  {5 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_6  {6 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_7  {7 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_8  {8 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_9  {9 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_10 {10 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_11 {11 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_12 {12 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_13 {13 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_14 {14 << dmactl0_dma1tsel_pos}
  ;     dmactl0_dma1tsel_15 {15 << dmactl0_dma1tsel_pos}
  ;   dmactl0_dma0tsel_pos 0
  ;     dmactl0_dma0tsel_0  {0 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_1  {1 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_2  {2 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_3  {3 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_4  {4 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_5  {5 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_6  {6 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_7  {7 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_8  {8 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_9  {9 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_10 {10 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_11 {11 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_12 {12 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_13 {13 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_14 {14 << dmactl0_dma0tsel_pos}
  ;     dmactl0_dma0tsel_15 {15 << dmactl0_dma0tsel_pos}
  ; dmactl1 0x124
  ;   dmactl1_dmaonfetch_pos 2
  ;     dmactl1_dmaonfetch {1 << dmactl1_dmaonfetch_pos}
  ;   dmactl1_roundrobin_pos 1
  ;     dmactl1_roundrobin {1 << dmactl1_roundrobin_pos}
  ;   dmactl1_ennmi_pos      0
  ;     dmactl1_ennmi      {1 << dmactl1_ennmi_pos}
  ; dmaiv   0x126
  ;   dmaiv_dma0ifg 2
  ;   dmaiv_dma1ifg 4
  ;   dmaiv_dma2ifg 6
  ; ; dmactl
  ;   dmactl_dmadt_pos       12
  ;     dmactl_dmadt_0 {0 << dmactl_dmadt_pos}
  ;     dmactl_dmadt_1 {1 << dmactl_dmadt_pos}
  ;     dmactl_dmadt_2 {2 << dmactl_dmadt_pos}
  ;     dmactl_dmadt_3 {3 << dmactl_dmadt_pos}
  ;     dmactl_dmadt_4 {4 << dmactl_dmadt_pos}
  ;     dmactl_dmadt_5 {5 << dmactl_dmadt_pos}
  ;     dmactl_dmadt_6 {6 << dmactl_dmadt_pos}
  ;     dmactl_dmadt_7 {7 << dmactl_dmadt_pos}
  ;   dmactl_dmadstincr_pos  10
  ;     dmactl_dmadstincr_0 {0 << dmactl_dmadstincr_pos}
  ;     dmactl_dmadstincr_1 {1 << dmactl_dmadstincr_pos}
  ;     dmactl_dmadstincr_2 {2 << dmactl_dmadstincr_pos}
  ;     dmactl_dmadstincr_3 {3 << dmactl_dmadstincr_pos}
  ;   dmactl_dmasrcincr_pos  8
  ;     dmactl_dmasrcincr_0 {0 << dmactl_dmasrcincr_pos}
  ;     dmactl_dmasrcincr_1 {1 << dmactl_dmasrcincr_pos}
  ;     dmactl_dmasrcincr_2 {2 << dmactl_dmasrcincr_pos}
  ;     dmactl_dmasrcincr_3 {3 << dmactl_dmasrcincr_pos}
  ;   dmactl_dstbyte_pos     7
  ;     dmactl_dstbyte {1 << dmactl_dstbyte_pos}
  ;   dmactl_srcbyte_pos     6
  ;     dmactl_srcbyte {1 << dmactl_srcbyte_pos}
  ;   dmactl_dmalevel_pos    5
  ;     dmactl_dmalevel {1 << dmactl_dmalevel_pos}
  ;   dmactl_dmaen_pos       4
  ;     dmactl_dmaen {1 << dmactl_dmaen_pos}
  ;   dmactl_dmaifg_pos      3
  ;     dmactl_dmaifg {1 << dmactl_dmaifg_pos}
  ;   dmactl_dnaie_pos       2
  ;     dmactl_dnaie {1 << dmactl_dnaie_pos}
  ;   dmactl_dmaabort_pos    1
  ;     dmactl_dmaabort {1 << dmactl_dmaabort_pos}
  ;   dmactl_dmareq_pos      0
  ;     dmactl_dmareq {1 << dmactl_dmareq_pos}
  ; dma0ctl 0x1d0
  ; dma0sa  0x1d2
  ; dma0da  0x1d6
  ; dma0sz  0x1da
  ; dma1ctl 0x1dc
  ; dma1sa  0x1de
  ; dma1da  0x1e2
  ; dma1sz  0x1e6
  ; dma2ctl 0x1e8
  ; dma2sa  0x1ea
  ; dma2da  0x1ee
  ; dma2sz  0x1f2
; flash memory
  frkey_pos  8
    frkey  0x9600
  fwkey_pos  8
    fwkey  0xa500
  fctl1 0x128
    fctl1_blkwrt_pos 7
      fctl1_blkwrt {1 << fctl1_blkwrt_pos}
    fctl1_wrt_pos    6
      fctl1_wrt {1 << fctl1_wrt_pos}
    fctl1_eeiex_pos  4
      fctl1_eeiex {1 << fctl1_eeiex_pos}
    fctl1_eei_pos    3
      fctl1_eei {1 << fctl1_eei_pos}
    fctl1_meras_pos  2
      fctl1_meras {1 << fctl1_meras_pos}
    fctl1_erase_pos  1
      fctl1_erase {1 << fctl1_erase_pos}
  fctl2 0x12a
    fctl2_fssel_pos 6
      fctl2_fssel_0 {0 << fctl2_fssel_pos}
      fctl2_fssel_1 {1 << fctl2_fssel_pos}
      fctl2_fssel_2 {2 << fctl2_fssel_pos}
      fctl2_fssel_3 {3 << fctl2_fssel_pos}
    fctl2_fn_pos    0
    .end_defs .for i in 0..63
      .def fctl2_fn_{i} {i << fctl2_fn_pos}
    .end_for .defs
  fctl3 0x12c
    fctl3_fail_pos    7
      fctl3_fail {1 << fctl3_fail_pos}
    fctl3_locka_pos   6
      fctl3_locka {1 << fctl3_locka_pos}
    fctl3_emex_pos    5
      fctl3_emex {1 << fctl3_emex_pos}
    fctl3_lock_pos    4
      fctl3_lock {1 << fctl3_lock_pos}
    fctl3_wait_pos    3
      fctl3_wait {1 << fctl3_wait_pos}
    fctl3_accvifg_pos 2
      fctl3_accvifg {1 << fctl3_accvifg_pos}
    fctl3_keyv_pos    1
      fctl3_keyv {1 << fctl3_keyv_pos}
    fctl3_busy_pos    0
      fctl3_busy {1 << fctl3_busy_pos}
  ; not used in msp430g2553
  ; fctl4 0x1be
  ;   mrg1_pos
  ;     mrg1 {1 << mrg1_pos}
  ;   mrg0_pos
  ;     mrg0 {1 << mrg0_pos}
; port p1
  p1in   0x20 ; .b
  p1out  0x21 ; .b
  p1dir  0x22 ; .b
  p1ifg  0x23 ; .b
  p1ies  0x24 ; .b
  p1ie   0x25 ; .b
  p1sel  0x26 ; .b
  p1ren  0x27 ; .b
  ; p1sel2 0x41 ; .b
; port p2
  p2in   0x28 ; .b
  p2out  0x29 ; .b
  p2dir  0x2a ; .b
  p2ifg  0x2b ; .b
  p2ies  0x2c ; .b
  p2ie   0x2d ; .b
  p2sel  0x2e ; .b
  p2ren  0x2f ; .b
  p2sel2 0x42 ; .b
; port p3
;   p3in   0x18 ; .b
;   p3out  0x19 ; .b
;   p3dir  0x1a ; .b
;   p3sel  0x1b ; .b
;   p3ren  0x10 ; .b
;   p3sel2 0x43 ; .b
; usi
  usictl0  0x78 ; .b
    usictl0_usipe7_pos   7
      usictl0_usipe7 {1 << usictl0_usipe7_pos}
    usictl0_usipe6_pos   6
      usictl0_usipe6 {1 << usictl0_usipe6_pos}
    usictl0_usipe5_pos   5
      usictl0_usipe5 {1 << usictl0_usipe5_pos}
    usictl0_usilsb_pos   4
      usictl0_usilsb {1 << usictl0_usilsb_pos}
    usictl0_usimst_pos   3
      usictl0_usimst {1 << usictl0_usimst_pos}
    usictl0_usige_pos    2
      usictl0_usige {1 << usictl0_usige_pos}
    usictl0_usioe_pos    1
      usictl0_usioe {1 << usictl0_usioe_pos}
    usictl0_usiswrst_pos 0
      usictl0_usiswrst {1 << usictl0_usiswrst_pos}
  usictl1  0x79 ; .b
    usictl1_usickph_pos   7
      usictl1_usickph {1 << usictl1_usickph_pos}
    usictl1_usii2c_pos    6
      usictl1_usii2c {1 << usictl1_usii2c_pos}
    usictl1_usisttie_pos  5
      usictl1_usisttie {1 << usictl1_usisttie_pos}
    usictl1_usiie_pos     4
      usictl1_usiie {1 << usictl1_usiie_pos}
    usictl1_usial_pos     3
      usictl1_usial {1 << usictl1_usial_pos}
    usictl1_usistp_pos    2
      usictl1_usistp {1 << usictl1_usistp_pos}
    usictl1_usisttifg_pos 1
      usictl1_usisttifg {1 << usictl1_usisttifg_pos}
    usictl1_usiifg_pos    0
      usictl1_usiifg {1 << usictl1_usiifg_pos}
  usickctl 0x7a ; .b
    usickctl_usidiv_pos   5
      usickctl_usidiv_0 {0 << usickctl_usidiv_pos}
      usickctl_usidiv_1 {1 << usickctl_usidiv_pos}
      usickctl_usidiv_2 {2 << usickctl_usidiv_pos}
      usickctl_usidiv_3 {3 << usickctl_usidiv_pos}
      usickctl_usidiv_4 {4 << usickctl_usidiv_pos}
      usickctl_usidiv_5 {5 << usickctl_usidiv_pos}
      usickctl_usidiv_6 {6 << usickctl_usidiv_pos}
      usickctl_usidiv_7 {7 << usickctl_usidiv_pos}
    usickctl_usissel_pos  2
      usickctl_usissel_0 {0 << usickctl_usissel_pos}
      usickctl_usissel_1 {1 << usickctl_usissel_pos}
      usickctl_usissel_2 {2 << usickctl_usissel_pos}
      usickctl_usissel_3 {3 << usickctl_usissel_pos}
      usickctl_usissel_4 {4 << usickctl_usissel_pos}
      usickctl_usissel_5 {5 << usickctl_usissel_pos}
      usickctl_usissel_6 {6 << usickctl_usissel_pos}
      usickctl_usissel_7 {7 << usickctl_usissel_pos}
    usickctl_usickpl_pos  1
      usickctl_usickpl {1 << usickctl_usickpl_pos}
    usickctl_usiswclk_pos 0
      usickctl_usiswclk {1 << usickctl_usiswclk_pos}
  usicnt   0x7b ; .b
    usicnt_usisclrel_pos 7
      usicnt_usisclrel {1 << usicnt_usisclrel_pos}
    usicnt_usi16b_pos    6
      usicnt_usi16b {1 << usicnt_usi16b_pos}
    usicnt_usiifgcc_pos  5
      usicnt_usiifgcc {1 << usicnt_usiifgcc_pos}
    usicnt_usicnt_pos    0
    .end_defs .for i in 0..31
      .def usicnt_usicnt_{i} {i << usicnt_usicnt_pos}
    .end_for .defs
  usisrl   0x7c ; .b
  usisrh   0x7d ; .b
  usictl   0x78
  usicctl  0x7a
  usisr    0x7c
; spi
;   ucb0mctl   0x6c ; .b

;   ucactl0_ckpl_pos 6
;     ucactl0_ckpl {1 << ucactl0_ckpl_pos}
;   ucactl0_ckph_pos 7
;     ucactl0_ckph {1 << ucactl0_ckph_pos}
;   ucactl0_ucmst_pos   3
;     ucactl0_ucmst {1 << ucactl0_ucmst_pos}
;   ucbctl0_ckpl_pos 6
;     ucbctl0_ckpl {1 << ucactl0_ckpl_pos}
;   ucbctl0_ckph_pos 7
;     ucbctl0_ckph {1 << ucactl0_ckph_pos}
;   ucbctl0_uc7bit_pos 4
;     ucbctl0_uc7bit {1 << ucbctl0_uc7bit_pos}
;   ucbctl0_ucmsb_pos  5
;     ucbctl0_ucmsb {1 << ucbctl0_ucmsb_pos}
; ; uart
;   uca0ctl0   0x60 ; .b
;   uca0ctl1   0x61 ; .b
;   uca0br0    0x62 ; .b
;   uca0br1    0x63 ; .b
;   uca0mctl   0x64 ; .b
;   uca0stat   0x65 ; .b
;   uca0rxbuf  0x66 ; .b
;   uca0txbuf  0x67 ; .b
;   uca0abctl  0x5d ; .b
;   uca0irtctl 0x5e ; .b
;   uca0irrctl 0x5f ; .b
  ; not used in msp430g2553
  ; uca1ctl0   0xd0 ; .b
  ; uca1ctl1   0xd1 ; .b
  ; uca1br0    0xd2 ; .b
  ; uca1br1    0xd3 ; .b
  ; uca1mctl   0xd4 ; .b
  ; uca1stat   0xd5 ; .b
  ; uca1rxbuf  0xd6 ; .b
  ; uca1txbuf  0xd7 ; .b
  ; uca1abctl  0xcd ; .b
  ; uca1irtctl 0xce ; .b
  ; uca1irrctl 0xcf ; .b
  ; uc1ie      6    ; .b
  ; uc1ifg     7    ; .b
  ;
  ; ucactl0
;     ucactl0_ucpen_pos  7  
;       ucactl0_ucpen {1 << ucactl0_ucpen_pos}
;     ucactl0_ucpar_pos  6
;       ucactl0_ucpar {1 << ucactl0_ucpar_pos}
;     ucactl0_ucmsb_pos  5
;       ucactl0_ucmsb {1 << ucactl0_ucmsb_pos}
;     ucactl0_uc7bit_pos 4
;       ucactl0_uc7bit {1 << ucactl0_uc7bit_pos}
;     ucactl0_ucspb_pos  3
;       ucactl0_ucspb {1 << ucactl0_ucspb_pos}
;     ucactl0_ucmode_pos 1
;       ucactl0_ucmode_0 {0 << ucactl0_ucmode_pos}
;       ucactl0_ucmode_1 {1 << ucactl0_ucmode_pos}
;       ucactl0_ucmode_2 {2 << ucactl0_ucmode_pos}
;       ucactl0_ucmode_3 {3 << ucactl0_ucmode_pos}
;     ucactl0_ucsync_pos 0
;       ucactl0_ucsync {1 << ucactl0_ucsync_pos}
;   ; ucactl1
;     ucactl1_ucssel_pos   6
;       ucactl1_ucssel_0 {0 << ucactl1_ucssel_pos}
;       ucactl1_ucssel_1 {1 << ucactl1_ucssel_pos}
;       ucactl1_ucssel_2 {2 << ucactl1_ucssel_pos}
;       ucactl1_ucssel_3 {3 << ucactl1_ucssel_pos}
;     ucactl1_ucrxeie_pos  5
;       ucactl1_ucrxeie {1 << ucactl1_ucrxeie_pos}
;     ucactl1_ucbrkie_pos  4
;       ucactl1_ucbrkie {1 << ucactl1_ucbrkie_pos}
;     ucactl1_ucdorm_pos   3
;       ucactl1_ucdorm {1 << ucactl1_ucdorm_pos}
;     ucactl1_uctxaddr_pos 2
;       ucactl1_uctxaddr {1 << ucactl1_uctxaddr_pos}
;     ucactl1_uctxbrk_pos  1
;       ucactl1_uctxbrk {1 << ucactl1_uctxbrk_pos}
;     ucactl1_ucswrst_pos  0
;       ucactl1_ucswrst {1 << ucactl1_ucswrst_pos}
;   ; ucamctl
;     ucamctl_ucbrf_pos  4
;       ucamctl_ucbrf_0  {0 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_1  {1 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_2  {2 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_3  {3 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_4  {4 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_5  {5 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_6  {6 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_7  {7 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_8  {8 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_9  {9 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_10 {10 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_11 {11 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_12 {12 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_13 {13 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_14 {14 << ucamctl_ucbrf_pos}
;       ucamctl_ucbrf_15 {15 << ucamctl_ucbrf_pos}
;     ucamctl_ucbrs_pos  1
;       ucamctl_ucbrs_0  {0 << ucamctl_ucbrs_pos}
;       ucamctl_ucbrs_1  {1 << ucamctl_ucbrs_pos}
;       ucamctl_ucbrs_2  {2 << ucamctl_ucbrs_pos}
;       ucamctl_ucbrs_3  {3 << ucamctl_ucbrs_pos}
;       ucamctl_ucbrs_4  {4 << ucamctl_ucbrs_pos}
;       ucamctl_ucbrs_5  {5 << ucamctl_ucbrs_pos}
;       ucamctl_ucbrs_6  {6 << ucamctl_ucbrs_pos}
;       ucamctl_ucbrs_7  {7 << ucamctl_ucbrs_pos}
;     ucamctl_ucos16_pos 0
;       ucamctl_ucos16 {1 << ucamctl_ucos16_pos}
;   ; ucastat
;     ucastat_uclisten_pos 7
;       ucastat_uclisten {1 << ucastat_uclisten_pos}
;     ucastat_ucfe_pos     6
;       ucastat_ucfe {1 << ucastat_ucfe_pos}
;     ucastat_ucoe_pos     5 
;       ucastat_ucoe {1 << ucastat_ucoe_pos}
;     ucastat_ucpe_pos     4
;       ucastat_ucpe {1 << ucastat_ucpe_pos}
;     ucastat_ucbrk_pos    3
;       ucastat_ucbrk {1 << ucastat_ucbrk_pos}
;     ucastat_ucrxerr_pos  2
;       ucastat_ucrxerr {1 << ucastat_ucrxerr_pos}
;     ucastat_ucaddr_pos   1
;       ucastat_ucaddr {1 << ucastat_ucaddr_pos}
;     ucastat_ucidle_pos {ucastat_ucaddr}
;       ucastat_ucidle {1 << ucastat_ucidle_pos}
;     ucastat_ucbusy_pos   0
;       ucastat_ucbusy {1 << ucastat_ucbusy_pos}
;   ; ucairtctl
;     ucairtctl_ucirtxpl_pos  2
;     .end_defs .for i in 0..63
;       .def ucairtctl_ucirtxpl_{i} {i << ucairtctl_ucirtxpl_pos}
;     .end_for .defs
;     ucairtctl_ucirtxclk_pos 1
;       ucairtctl_ucirtxclk {1 << ucairtctl_ucirtxclk_pos}
;     ucairtctl_uciren_pos    0
;       ucairtctl_uciren {1 << ucairtctl_uciren_pos}
;   ; ucairrctl
;     ucairrctl_ucirrxfl_pos 2
;     .end_defs .for i in 0..63
;       .def ucairrctl_ucirrxfl_{i} {i << ucairrctl_ucirrxfl_pos}
;     .end_for .defs
;     ucairrctl_ucirrxpl_pos 1
;       ucairrctl_ucirrxpl {1 << ucairrctl_ucirrxpl_pos}
;     ucairrctl_ucirrxfe_pos 0
;       ucairrctl_ucirrxfe {1 << ucairrctl_ucirrxfe_pos}
;   ; ucaabctl
;     ucaabctl_ucdelim_pos 4
;       ucaabctl_ucdelim_0 {0 << ucaabctl_ucdelim_pos}
;       ucaabctl_ucdelim_1 {1 << ucaabctl_ucdelim_pos}
;       ucaabctl_ucdelim_2 {2 << ucaabctl_ucdelim_pos}
;       ucaabctl_ucdelim_3 {3 << ucaabctl_ucdelim_pos}
;     ucaabctl_ucstoe_pos  3
;       ucaabctl_ucstoe {1 << ucaabctl_ucstoe_pos}
;     ucaabctl_ucbtor_pos  2
;       ucaabctl_ucbtor {1 << ucaabctl_ucbtor_pos}
;     ucaabctl_ucabden_pos 0
;       ucaabctl_ucabden {1 << ucaabctl_ucabden_pos}
; ; i2c
;   ucb0ctl0  0x68
;   ucb0ctl1  0x69
;   ucb0br0   0x6a
;   ucb0br1   0x6b
;   ucb0i2cie 0x6c
;   ucb0stat  0x6d
;   ucb0rxbuf 0x6e
;   ucb0txbuf 0x6f
;   ucb0i2coa 0x118
;   ucb0i2csa 0x11a
;   ; not used in msp430g2553
;   ; ucb1ctl0
;   ; ucb1ctl1
;   ; ucb1br0
;   ; ucb1br1
;   ; ucb1i2cie
;   ; ucb1stat
;   ; ucb1rxbuf
;   ; ucb1txbuf
;   ; ucb1i2coa
;   ; ucb1i2csa
;   ;
;   ; ucbctl0
;     ucbctl0_uca10_pos   7
;       ucbctl0_uca10 {1 << ucbctl0_uca10_pos}
;     ucbctl0_ucsla10_pos 6
;       ucbctl0_ucsla10 {1 << ucbctl0_ucsla10_pos}
;     ucbctl0_ucmm_pos    5
;       ucbctl0_ucmm {1 << ucbctl0_ucmm_pos}
;     ucbctl0_ucmst_pos   3
;       ucbctl0_ucmst {1 << ucbctl0_ucmst_pos}
;     ucbctl0_ucmode_pos  1
;       ucbctl0_ucmode_0 {0 << ucbctl0_ucmode_pos}
;       ucbctl0_ucmode_1 {1 << ucbctl0_ucmode_pos}
;       ucbctl0_ucmode_2 {2 << ucbctl0_ucmode_pos}
;       ucbctl0_ucmode_3 {3 << ucbctl0_ucmode_pos}
;     ucbctl0_ucsync_pos  0
;       ucbctl0_ucsync {1 << ucbctl0_ucsync_pos}
;   ; ucbctl1
;     ucbctl1_ucssel_pos   6
;       ucbctl1_ucssel_0 {0 << ucbctl1_ucssel_pos}
;       ucbctl1_ucssel_1 {1 << ucbctl1_ucssel_pos}
;       ucbctl1_ucssel_2 {2 << ucbctl1_ucssel_pos}
;       ucbctl1_ucssel_3 {3 << ucbctl1_ucssel_pos}
;     ucbctl1_uctr_pos     4
;       ucbctl1_uctr {1 << ucbctl1_uctr_pos}
;     ucbctl1_uctxnack_pos 3
;       ucbctl1_uctxnack {1 << ucbctl1_uctxnack_pos}
;     ucbctl1_uctxstp_pos  2
;       ucbctl1_uctxstp {1 << ucbctl1_uctxstp_pos}
;     ucbctl1_uctxstt_pos  1
;       ucbctl1_uctxstt {1 << ucbctl1_uctxstt_pos}
;     ucbctl1_ucswrst_pos  0
;       ucbctl1_ucswrst {1 << ucbctl1_ucswrst_pos}
;   ; ucbstat
;     ucbstat_ucscllow_pos  6
;       ucbstat_ucscllow {1 << ucbstat_ucscllow_pos}
;     ucbstat_ucgc_pos      5
;       ucbstat_ucgc {1 << ucbstat_ucgc_pos}
;     ucbstat_ucbbusy_pos   4
;       ucbstat_ucbbusy {1 << ucbstat_ucbbusy_pos}
;     ucbstat_ucnackifg_pos 3
;       ucbstat_ucnackifg {1 << ucbstat_ucnackifg_pos}
;     ucbstat_ucstpifg_pos  2
;       ucbstat_ucstpifg {1 << ucbstat_ucstpifg_pos}
;     ucbstat_ucsttifg_pos  1
;       ucbstat_ucsttifg {1 << ucbstat_ucsttifg_pos}
;     ucbstat_ucalifg_pos   0
;       ucbstat_ucalifg {1 << ucbstat_ucalifg_pos}
;   ; ucbi2coa
;     ucbi2coa_ucgen_pos 15
;       ucbi2coa_ucgen {1 << ucbi2coa_ucgen_pos}
;     ucbi2coa_i2coa_pos 0
;   ; ucbi2cie
;     ucbi2cie_ucnackie_pos 3
;     ucbi2cie_ucstpie_pos  2
;     ucbi2cie_ucsttie_pos  1
;     ucbi2cie_ucalie_pos   0
; basic clock system+
  dcoctl  0x56 ; .b
    dcoctl_dco_pos 5
      dcoctl_dco_0 {0 << dcoctl_dco_pos}
      dcoctl_dco_1 {1 << dcoctl_dco_pos}
      dcoctl_dco_2 {2 << dcoctl_dco_pos}
      dcoctl_dco_3 {3 << dcoctl_dco_pos}
      dcoctl_dco_4 {4 << dcoctl_dco_pos}
      dcoctl_dco_5 {5 << dcoctl_dco_pos}
      dcoctl_dco_6 {6 << dcoctl_dco_pos}
      dcoctl_dco_7 {7 << dcoctl_dco_pos}
    dcoctl_mod_pos 0
    .end_defs .for i in 0..31
      .def dcoctl_mod_{i} {i << dcoctl_mod_pos}
    .end_for .defs
  bcsctl1 0x57 ; .b
    bcsctl1_xt2off_pos 7
      bcsctl1_xt2off {1 << bcsctl1_xt2off_pos}
    bcsctl1_xts_pos    6
      bcsctl1_xts    {1 << bcsctl1_xts_pos}
    bcsctl1_diva_pos   4
      bcsctl1_diva_0  {0 << bcsctl1_diva_pos}
      bcsctl1_diva_1  {1 << bcsctl1_diva_pos}
      bcsctl1_diva_2  {2 << bcsctl1_diva_pos}
      bcsctl1_diva_3  {3 << bcsctl1_diva_pos}
    bcsctl1_rsel_pos   0
      bcsctl1_rsel_0  {0 << bcsctl1_rsel_pos}
      bcsctl1_rsel_1  {1 << bcsctl1_rsel_pos}
      bcsctl1_rsel_2  {2 << bcsctl1_rsel_pos}
      bcsctl1_rsel_3  {3 << bcsctl1_rsel_pos}
      bcsctl1_rsel_4  {4 << bcsctl1_rsel_pos}
      bcsctl1_rsel_5  {5 << bcsctl1_rsel_pos}
      bcsctl1_rsel_6  {6 << bcsctl1_rsel_pos}
      bcsctl1_rsel_7  {7 << bcsctl1_rsel_pos}
      bcsctl1_rsel_8  {8 << bcsctl1_rsel_pos}
      bcsctl1_rsel_9  {9 << bcsctl1_rsel_pos}
      bcsctl1_rsel_10 {10 << bcsctl1_rsel_pos}
      bcsctl1_rsel_11 {11 << bcsctl1_rsel_pos}
      bcsctl1_rsel_12 {12 << bcsctl1_rsel_pos}
      bcsctl1_rsel_13 {13 << bcsctl1_rsel_pos}
      bcsctl1_rsel_14 {14 << bcsctl1_rsel_pos}
      bcsctl1_rsel_15 {15 << bcsctl1_rsel_pos}
  bcsctl2 0x58 ; .b
    bcsctl2_selm_pos 6
      bcsctl2_selm_0 {0 << bcsctl2_selm_pos}
      bcsctl2_selm_1 {1 << bcsctl2_selm_pos}
      bcsctl2_selm_2 {2 << bcsctl2_selm_pos}
      bcsctl2_selm_3 {3 << bcsctl2_selm_pos}
    bcsctl2_divm_pos 4
      bcsctl2_divm_0 {0 << bcsctl2_divm_pos}
      bcsctl2_divm_1 {1 << bcsctl2_divm_pos}
      bcsctl2_divm_2 {2 << bcsctl2_divm_pos}
      bcsctl2_divm_3 {3 << bcsctl2_divm_pos}
    bcsctl2_sels_pos 3
      bcsctl2_sels   {1 << bcsctl2_sels_pos}
    bcsctl2_divs_pos 1
      bcsctl2_divs_0 {0 << bcsctl2_divs_pos}
      bcsctl2_divs_1 {1 << bcsctl2_divs_pos}
      bcsctl2_divs_2 {2 << bcsctl2_divs_pos}
      bcsctl2_divs_3 {3 << bcsctl2_divs_pos}
    bcsctl2_dcor_pos 0
      bcsctl2_dcor   {1 << bcsctl2_dcor_pos}
  bcsctl3 0x53 ; .b
    bcsctl3_xt2s_pos    6
      bcsctl3_xt2s_0  {0 << bcsctl3_xt2s_pos}
      bcsctl3_xt2s_1  {1 << bcsctl3_xt2s_pos}
      bcsctl3_xt2s_2  {2 << bcsctl3_xt2s_pos}
      bcsctl3_xt2s_3  {3 << bcsctl3_xt2s_pos}
    bcsctl3_lfxt1s_pos  4
      bcsctl3_lfxt1s_0 {0 << bcsctl3_lfxt1s_pos}
      bcsctl3_lfxt1s_1 {1 << bcsctl3_lfxt1s_pos}
      bcsctl3_lfxt1s_2 {2 << bcsctl3_lfxt1s_pos}
      bcsctl3_lfxt1s_3 {3 << bcsctl3_lfxt1s_pos}
    bcsctl3_xcap_pos    2
      bcsctl3_xcap_0  {0 << bcsctl3_xcap_pos}
      bcsctl3_xcap_1  {1 << bcsctl3_xcap_pos}
      bcsctl3_xcap_2  {2 << bcsctl3_xcap_pos}
      bcsctl3_xcap_3  {3 << bcsctl3_xcap_pos}
    bcsctl3_xt2of_pos   1
      bcsctl3_xt2of   {1 << bcsctl3_xt2of_pos}
    bcsctl3_lfxt1of_pos 0
      bcsctl3_lfxt1of {1 << bcsctl3_lfxt1of_pos}
; comparator
  cactl1 0x59 ; .b
    cactl1_caex_pos   7
      cactl1_caex   {1 << cactl1_caex_pos}
    cactl1_carsel_pos 6
      cactl1_carsel {1 << cactl1_carsel_pos}
    cactl1_caref_pos  4
      cactl1_caref  {1 << cactl1_caref_pos}
    cactl1_caon_pos   3
      cactl1_caon   {1 << cactl1_caon_pos}
    cactl1_caies_pos  2
      cactl1_caies  {1 << cactl1_caies_pos}
    cactl1_caie_pos   1
      cactl1_caie   {1 << cactl1_caie_pos}
    cactl1_caifg_pos  0
      cactl1_caifg  {1 << cactl1_caifg_pos}
  cactl2 0x5a ; .b
    cactl2_cashort_pos 7
      cactl2_cashort {1 << cactl2_cashort_pos}
    cactl2_p2ca4_pos   6
      cactl2_p2ca4   {1 << cactl2_p2ca4_pos}
    cactl2_p2ca3_pos   5
      cactl2_p2ca3   {1 << cactl2_p2ca3_pos}
    cactl2_p2ca2_pos   4
      cactl2_p2ca2   {1 << cactl2_p2ca2_pos}
    cactl2_p2ca1_pos   3
      cactl2_p2ca1   {1 << cactl2_p2ca1_pos}
    cactl2_p2ca0_pos   2
      cactl2_p2ca0   {1 << cactl2_p2ca0_pos}
    cactl2_caf_pos     1
      cactl2_caf     {1 << cactl2_caf_pos}
    cactl2_caout_pos   0
      cactl2_caout   {1 << cactl2_caout_pos}
  capd   0x5b ; .b
    capd_capd7_pos 7
      capd_capd7 {1 << capd_capd7}
    capd_capd6_pos 6
      capd_capd6 {1 << capd_capd6}
    capd_capd5_pos 5
      capd_capd5 {1 << capd_capd5}
    capd_capd4_pos 4
      capd_capd4 {1 << capd_capd4}
    capd_capd3_pos 3
      capd_capd3 {1 << capd_capd3}
    capd_capd2_pos 2
      capd_capd2 {1 << capd_capd2}
    capd_capd1_pos 1
      capd_capd1 {1 << capd_capd1}
    capd_capd0_pos 0
      capd_capd0 {1 << capd_capd0}
.end_defs