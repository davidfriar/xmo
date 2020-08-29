#! /bin/sh
# curl -s 'wttr.in/?format=%c+%t' | sed  "s/+//g;
wego -f emoji | sed -n 4p | sed "s/(.*) //;
s/✨/ /g;
s/☁️/ /g;
s/🌫/ /g;
s/🌧/ /g;
s/❄️/ /g;
s/🌦/ /g;
s/🌨/ /g;
s/⛅️/ /g;
s/☀️/ /g;
s/🌩/ /g;
s/⛈/ /g;
s/[^[:print:]]\[[^a-zA-Z]*[a-zA-Z]//g"
