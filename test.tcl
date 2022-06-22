package require Tk

wm title . joystick

proc moved {val} {
  puts $val
  puts -nonewline $::tty [format %c%c%c%c 250 2 9 $val]
  flush $::tty
}

scale .scale -from 0 -to 255 -command moved
pack .scale

set tty [open "/dev/ttyUSB0" r+]
fconfigure $tty -mode 9600,n,8,1 -handshake none -translation binary
# set f [open ttt r+]
# fconfigure $f -translation binary
# puts -nonewline $tty [format %c%c%c%c 250 2 10 250]
# flush $tty
# close $tty
