Config
{ font = "xft:Roboto Condensed:size=15"
, additionalFonts = [ "xft:Ionicons:size=15" ]
, fgColor = "{{fg}}"
, bgColor = "{{bg}}"
, position = BottomSize C 100 42
, commands =
  [ Run Battery ["-t", "<left>"] 100
  , Run MultiCpu ["-t","<total0>"] 30
  , Run Date "%#A %_d %#B %Y" "date" 600
  , Run Date "%H:%M" "time" 600
  , Run Com "/home/bob/bin/alsavolume" [] "volume" 10
  , Run Com "/home/bob/bin/backlight" [] "backlight" 10
  , Run CommandReader "pymodoro -l 5 -p ▬ -b ▬ -e ▭ -bp '<fc={{sep}}><fn=1></fn></fc> ' -pp '<fc={{sep}}><fn=1></fn></fc> ' -sb '/usr/data/session.wav' -sp '/home/bob/bin/applause.wav'" "pomodoro"
  , Run StdinReader ]
, sepChar = "%"
, alignSep = "}{"
, template = "  <fc={{sep}}><fn=1></fn></fc> %StdinReader% }{  %pomodoro%   <fc={{sep}}><fn=1></fn></fc> %volume%   <fc={{sep}}><fn=1></fn></fc> %backlight%   <fc={{sep}}><fn=1></fn></fc> %multicpu%   <fc={{sep}}><fn=1></fn></fc> %battery%   <fc={{sep}}><fn=1></fn></fc> %date%   <fc={{sep}}><fn=1></fn></fc> %time%   "
}
