.defs
; memory map
  mem_code     0xe000
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
  mem_ram_end  0x2ff
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
  mul_mpy    0x130
  mul_mpys   0x132
  mul_mac    0x134
  mul_macs   0x136
  mul_op2    0x138
  mul_reslo  0x13a
  mul_reshi  0x13c
  mul_sumext 0x13e
; interrupt vectors
  reset_vector       0xfffe ; highest priority
  nmi_vector         0xfffc
  ; timer1_a0_vector   0xfffa
  ; timer1_a1_vector   0xfff8
  comparatora_vector 0xfff6
  wdt_vector         0xfff4
  timer0_a0_vector   0xfff2
  timer0_a1_vector   0xfff0
  ; usciab0rx_vector   0xffee
  ; usciab0tx_vector   0xffec
  adc10_vector        0xffea
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
    cal_bc1_8mhz_offset  7
    cal_dco_8mhz_offset  6
    cal_bc1_12mhz_offset 5
    cal_dco_12mhz_offset 4
    cal_bc1_16mhz_offset 3
    cal_dco_16mhz_offset 2
    cal_bc1_1mhz  {tag_dco_30 + cal_bc1_1mhz_offset}
    cal_dco_1mhz  {tag_dco_30 + cal_dco_1mhz_offset}
    cal_bc1_8mhz  {tag_dco_30 + cal_bc1_8mhz_offset}
    cal_dco_8mhz  {tag_dco_30 + cal_dco_8mhz_offset}
    cal_bc1_12mhz {tag_dco_30 + cal_bc1_12mhz_offset}
    cal_dco_12mhz {tag_dco_30 + cal_dco_12mhz_offset}
    cal_bc1_16mhz {tag_dco_30 + cal_bc1_16mhz_offset}
    cal_dco_16mhz {tag_dco_30 + cal_dco_16mhz_offset}
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
; port p1
  p1in   0x20 ; .b
  p1out  0x21 ; .b
  p1dir  0x22 ; .b
  p1ifg  0x23 ; .b
  p1ies  0x24 ; .b
  p1ie   0x25 ; .b
  p1sel  0x26 ; .b
  p1ren  0x27 ; .b
  p1sel2 0x41 ; .b
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
      bcsctl1_div_0  {0 << bcsctl1_diva_pos}
      bcsctl1_div_1  {1 << bcsctl1_diva_pos}
      bcsctl1_div_2  {2 << bcsctl1_diva_pos}
      bcsctl1_div_3  {3 << bcsctl1_diva_pos}
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
.end_defs