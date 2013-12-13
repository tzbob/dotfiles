Config 
  { font = "xft:inconsolata:size=10"
  , fgColor = "#EFEFEF"
  , bgColor = "#333333"
  , position = Static { xpos = 0, ypos = 1050, width = 1920, height = 30 }
  , lowerOnStart = False
  , commands = 
    [ Run Battery ["-t", "<left>"] 100
    , Run MultiCpu ["-t","<total0>"] 30
    , Run Date "%_d %#B %Y  <fc=#9B1A1E>|</fc>  %H:%M" "date" 600
    , Run Com "/home/tzbob/bin/alsavolume" [] "volume" 10
    , Run StdinReader ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = " %StdinReader% }{ <fc=#C6361E>cpu</fc>  %multicpu%   <fc=#C6361E>vol</fc>  %volume%   <fc=#C6361E>bat</fc>  %battery%  <fc=#C6361E>|</fc>  %date%  "
  }
