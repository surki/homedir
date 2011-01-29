#!/bin/bash

cat - | {
  nt_icon="gtk-dialog-info"
	nt_time=5000
	nt_head=""
	nt_text=""
	nt_type="Message"
	while read k v
	do
	  case $k in
		  TYPE)    nt_type=$v;;
		  ICON)    nt_icon=$v;;
		  CONTENT) nt_text=$v;;
			TIMEOUT) nt_time=$v;;
		  SUBJECT) nt_head=$v;;
		esac
	done 
  if [ -n "$nt_text" -a -n "$nt_head" ]
	then
	  notify-send -i "$nt_icon" \
		            -c "$nt_type" \
								-t $nt_time -- \
								"$nt_head" "$nt_text" 
	fi
} >> /tmp/a
