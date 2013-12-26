Config 
  { font = "xft:droid sans:size=8"
  , fgColor = "#EFEFEF"
  , bgColor = "#134D76"
  , position = Static { xpos = 1, ypos = 1060, width = 1918, height = 19 }
  , lowerOnStart = False
  , commands = 
    [ Run Battery ["-t", "<left>"] 100
    , Run MultiCpu ["-t","<total0>"] 30
    , Run Date "%_d %#B %Y  <fc=#FE526F>|</fc>  %H:%M" "date" 600
    , Run Com "/home/tzbob/bin/alsavolume" [] "volume" 10
    , Run StdinReader ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = " %StdinReader% }{ <fc=#FE526F>cpu</fc>  %multicpu%   <fc=#FE526F>vol</fc>  %volume%   <fc=#FE526F>bat</fc>  %battery%  <fc=#FE526F>|</fc>  %date%  "
  }
