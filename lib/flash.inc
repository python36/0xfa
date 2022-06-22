.macro mov_fctl1(erase=0, meras=0, eei=0, eeiex=0, wrt=0, blkwrt=0, key=0)
  mov #{(erase << fctl1_erase_pos) | (meras << fctl1_meras_pos) | (eei << fctl1_eei_pos) |
    (eeiex << fctl1_eeiex_pos) | (wrt << fctl1_wrt_pos) | (blkwrt << fctl1_blkwrt_pos) | (key << fwkey_pos)}, &{fctl1}
.end_macro

.macro mov_fctl2(fn=2, ssel=0, key=0)
  mov #{fn | (ssel << fctl2_fssel_pos) | (key << frkey_pos)}, &{fctl2}
.end_macro

.macro mov_fctl3(busy=0, keyv=0, accvifg=0, lock=1, emex=0, locka=1, fail=0, key=0)
  mov #{busy | (keyv << fctl3_keyv_pos) | (accvifg << fctl3_accvifg_pos) | (lock << fctl3_lock_pos) |
    (emex << fctl3_emex_pos) | (locka << fctl3_locka_pos) | (fail << fctl3_fail_pos) | (key << fwkey_pos)}, &{fctl3}
.end_macro

.macro mov_fctl4(mrg0=2, mrg1=0, key=0)
  mov #{(mrg0 << mrg0_pos) | (mrg1 << mrg1_pos) | (key << fwkey_pos)}, &{fctl4}
.end_macro
