Config
{ font = "xft:Iosevka:size=12"
, additionalFonts = [ "xft:Ionicons:size=12" ]
, fgColor = "{{fg}}"
, bgColor = "{{bg}}"
, position = BottomSize C 100 38
, commands =
  [ Run Battery ["-t", "<left>"] 100
  , Run MultiCpu ["-t","<total0>"] 30
  , Run Date "%#A %_d %#B %Y" "date" 600
  , Run Date "%H:%M" "time" 600
  , Run Volume "default" "Master" [ "-t", "<volume>" ] 10
  , Run CommandReader "pymodoro -l 5 -p ▬ -b ▬ -e ▭ -bp '<fc={{sep}}><fn=1></fn></fc> ' -pp '<fc={{sep}}><fn=1></fn></fc> ' -sb '/home/bob/bin/trumpet.wav' -sp '/home/bob/bin/applause.wav' 25 5" "pomodoro"
  , Run StdinReader ]
, sepChar = "%"
, alignSep = "}{"
, template = "  <fc={{sep}}><fn=1></fn></fc> %StdinReader% }{ %pomodoro% <fc={{sep}}><fn=1></fn></fc> %default:Master%   <fc={{sep}}><fn=1></fn></fc> %multicpu%   <fc={{sep}}><fn=1></fn></fc> %date%   <fc={{sep}}><fn=1></fn></fc> %time%   "
}
