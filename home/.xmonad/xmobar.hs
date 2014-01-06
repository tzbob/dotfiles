Config 
  { font = "xft:droid sans:size=8"
  , fgColor = "#EFEFEF"
  , bgColor = "#6E839C"
  , position = Static { xpos = 1, ypos = 1060, width = 1918, height = 19 }
  , lowerOnStart = False
  , commands = 
    [ Run Battery ["-t", "<left>"] 100
    , Run MultiCpu ["-t","<total0>"] 30
    , Run Date "%_d %#B %Y  <fc=#BCB1B7>|</fc>  %H:%M" "date" 600
    , Run Com "/home/tzbob/bin/alsavolume" [] "volume" 10
    , Run StdinReader ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = " %StdinReader% }{ <fc=#BCB1B7>cpu</fc>  %multicpu%   <fc=#BCB1B7>vol</fc>  %volume%   <fc=#BCB1B7>bat</fc>  %battery%  <fc=#BCB1B7>|</fc>  %date%  "
  }
