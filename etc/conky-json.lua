conky.config = {
   background=false,
   out_to_console=true,
   out_to_x=false,
   update_interval=1,
}

conky.text = [[\
{ "cpu_1": "${cpu cpu1}" \
, "cpu_2": "${cpu cpu2}" \
, "cpu_3": "${cpu cpu3}" \
, "cpu_4": "${cpu cpu4}" \
, "cpu_5": "${cpu cpu5}" \
, "cpu_6": "${cpu cpu6}" \
, "cpu_7": "${cpu cpu7}" \
, "cpu_8": "${cpu cpu8}" \
, "mem"  : "${memperc}" \
, "date" : "${time %Y-%m-%d}" \
, "time" : "${time %T}" \
, "battery" : "${battery_percent BAT1}" \
, "adapter" : "${acpiacadapter ADP1}" \
, "mpdstatus" : "${mpd_status}" \
, "top" : "${top name 1}" \
, "netspeed_d" : "${downspeedf eth0} ${downspeedf wlan0}" \
, "netspeed_u" : "${upspeedf eth0} ${upspeedf wlan0}" \
, "freq" : "${freq_g}" \
, "mail" : "${exec touch /tmp/xmonad_mail_check && cat /tmp/xmonad_mail_check}" \
} \
]]