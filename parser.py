import serial
import sys

s = serial.Serial("/dev/ttyUSB0", baudrate=115200, timeout=None)
t = 0
i = 0

# print help(serial.Serial)

while 1:
  b = ord(s.read())
  # print hex(b)
  if i == 13:
    i = 0
    print "\r",
    sys.stdout.flush()
  elif i > 0:
    if i & 1:
      t = b
    else:
      print (b << 8) + t,
    i += 1
  elif b == 0xfa:
    i = 1

s.close()
