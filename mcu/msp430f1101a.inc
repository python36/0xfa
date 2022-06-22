.defs
; memory map
  mem_code     0xfc00
  mem_code_end 0xffbf
;; info
  mem_info     0x1080
  mem_info_end 0x10ff

  mem_info_segment_a     0x10c0
  mem_info_segment_a_end 0x1100
  mem_info_segment_b     0x1080
  mem_info_segment_b_end 0x10c0
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
; interrupt vectors
  reset_vector       0xfffe ; highest priority
  nmi_vector         0xfffc
  ; not used         0xfffa
  ; not used         0xfff8
  comparatora_vector 0xfff6
  wdt_vector         0xfff4
  timer0_a0_vector   0xfff2
  timer0_a1_vector   0xfff0
  ; not used         0xffee
  ; not used         0xffec
  ; not used         0xffea
  ; not used         0xffe8
  port2_vector       0xffe6
  port1_vector       0xffe4
  ; not used         0xffe2
  ; not used         0xffe0
  bslskey            0xffde
  ; not used         0xffda..0xffc0
; special function
  ie1 0 ; .b
    wdtie  1
    ofie   2
    nmie   {2 ** 4}
    accvie {2 ** 5}
  ie2 1 ; .b
  ifg1 2 ; .b
    wdtifg 1
    ofifg  2
    ; porifg 4
    ; rstifg 8
    nmiifg 16
  ifg2 3 ; .b
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
  ta0cctl2 0x166
  ta0r     0x170
  ta0ccr0  0x172
  ta0ccr1  0x174
  ta0ccr2  0x176
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
; port p2
  p2in   0x28 ; .b
  p2out  0x29 ; .b
  p2dir  0x2a ; .b
  p2ifg  0x2b ; .b
  p2ies  0x2c ; .b
  p2ie   0x2d ; .b
  p2sel  0x2e ; .b
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