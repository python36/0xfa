from pymavlink import mavutil

master = mavutil.mavlink_connection('/dev/ttyACM1', baud=57600, autoreconnect=True, source_system=255)

while 1:
  msg = master.recv_match(blocking=True, timeout=10)
  if msg.get_type() == 'RC_CHANNELS':
    print msg.chan1_raw, msg.chan2_raw, msg.chan3_raw, msg.chan4_raw, msg.chan5_raw, msg.chan6_raw, msg.chan7_raw, \
      msg.chan8_raw, msg.chan9_raw, msg.chan10_raw, msg.chan11_raw, msg.chan12_raw, msg.chan13_raw, msg.chan14_raw, msg.chan15_raw, msg.chan16_raw
    master.mav.set_mode_send(1, 1, 0)