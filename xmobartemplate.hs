Config 
{ font = "xft:Iosevka:size=13" 
, fgColor = "{{fg}}" 
, bgColor = "{{bg}}" 
, position = BottomSize C 100 32
, commands = 
  [ Run Battery ["-t", "<left>"] 100 
  , Run MultiCpu ["-t","<total0>"] 30 
  , Run Date "%#A %_d %#B %Y  <fc={{sep}}>-</fc>  %H:%M" "date" 600 
  , Run Com "/home/bob/bin/alsavolume" [] "volume" 10 
  , Run Com "/home/bob/bin/backlight" [] "backlight" 10 
  , Run StdinReader ] 
, sepChar = "%" 
, alignSep = "}{" 
, template = " %StdinReader% }{ <fc={{sep}}>cpu</fc>  %multicpu%   <fc={{sep}}>volume</fc>  %volume%  <fc={{sep}}>backlight</fc>  %backlight%  <fc={{sep}}>battery</fc>  %battery%  <fc={{sep}}>-</fc>  %date%  " 
}
