import XMonad
import System.IO (Handle)

import Data.List
import qualified Data.Text.Lazy as L

import qualified Text.Hastache as H
import Text.Hastache.Context

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.Submap as SM

import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named
import XMonad.Layout.ThreeColumns

import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map as M

data ColorScheme = ColorScheme
  { foreground :: String
  , background :: String
  , empty :: String
  , hidden :: String
  , highlight :: String
  , seperator :: String }

tempusfugit :: ColorScheme
tempusfugit = ColorScheme
  { foreground = "#657b83"
  , background = "#111918"
  , empty = "#657b83"
  , hidden = "#5F8C8C"
  , highlight = "#dddddd"
  , seperator = "#5F8C8C" }

aproprospiate :: ColorScheme
aproprospiate = ColorScheme
  { foreground = "#546e7a"
  , background = "#fafafa"
  , empty = "#90a4ae"
  , hidden = "#78909c"
  , highlight = "#42a5f5"
  , seperator = "#ec407a" }

scheme :: ColorScheme
scheme = aproprospiate

-- Micro states | Modal WM
-------------------------------------------------------------------------------

myLeader :: (ButtonMask, KeySym)
myLeader = (0, xK_Super_L)

subMap :: [(KeySym, X())] -> X()
subMap bindings = SM.submap $ M.fromList $ keyBindings ++ keyBindings'
    where
      keyBindings = map (\(k, v) -> ((0, k), v)) bindings
      -- allow Super+Key as well so that keybindings still register if Super is
      -- down (only usefull if the leader is Super_L or Super_R)
      keyBindings' = map (\(k, v) -> ((mod4Mask, k), v)) bindings

recursiveSubMap :: [(KeySym, X())] -> X()
recursiveSubMap bindings = sm
    where
      recursiveBindings = map (\(k, v) -> (k, v >> sm)) bindings
      sm = subMap recursiveBindings

-- Key bindings. Add, modify or remove key bindings here.
-------------------------------------------------------------------------------

myRootMap :: XConfig Layout -> ((ButtonMask, KeySym), X ())
myRootMap conf = (myLeader, rootMap)
    where
      rootMap = subMap [ (xK_j, windows W.focusDown)
                       , (xK_k, windows W.focusUp)
                       , (xK_s, shiftMap)

                       , (xK_space, spawn "rofi -show window")

                       , (xK_i, workspaceSelectMap)
                       , (xK_o, workspaceShiftMap)

                       , (xK_w, nextScreen)
                       , (xK_e, shiftNextScreen)

                       , (xK_r, resizeMap)
                       , (xK_l, layoutMap)

                       , (xK_v, volumeMap)
                       , (xK_b, brightnessMap)

                       , (xK_period, scratchpadMap)
                       , (xK_bracketleft, programMap)

                       , (xK_c, kill)
                       , (xK_t, withFocused $ windows . W.sink)
                       , (xK_q, spawn "xmonad --recompile; xmonad --restart") ]

      focusMap = recursiveSubMap [ (xK_k, windows W.focusUp)
                                 , (xK_Return, windows W.focusMaster)
                                 , (xK_j, windows W.focusDown) ]

      shiftMap = recursiveSubMap [ (xK_k, windows W.swapUp)
                                 , (xK_Return, windows W.swapMaster)
                                 , (xK_h, sendMessage (IncMasterN 1))
                                 , (xK_l, sendMessage (IncMasterN (-1)))
                                 , (xK_j, windows W.swapDown) ]

      resizeMap = recursiveSubMap [ (xK_h, sendMessage Shrink)
                                 , (xK_l, sendMessage Expand)
                                 , (xK_j, sendMessage MirrorShrink)
                                 , (xK_k, sendMessage MirrorExpand) ]

      layoutMap = recursiveSubMap [ (xK_n, sendMessage NextLayout)
                                  , (xK_r, setLayout $ XMonad.layoutHook conf) ]

      volumeMap = recursiveSubMap [ (xK_j, spawn "amixer -q set Master 5- unmute")
                                  , (xK_k, spawn "amixer -q set Master 5+ unmute")
                                  , (xK_m, spawn "amixer set Master toggle") ]

      brightnessMap = recursiveSubMap [ (xK_j, spawn "xbacklight - 15")
                                      , (xK_k, spawn "xbacklight + 15")
                                      , (xK_m, spawn "xbacklight = 100") ]

      scratchpadMap = subMap [ (xK_t, namedScratchpadAction myScratchpads "term")
                             , (xK_w, namedScratchpadAction myScratchpads "weechat") ]

      programMap = subMap [ (xK_o, spawn "rofi -show run")
                          , (xK_e, spawn "emacsclient -c")
                          , (xK_b, spawn "chromium")
                          , (xK_t, spawn "termite")
                          , (xK_l, spawn "/bin/sh -c 'xset dpms force off && slock'") ]

      indexKeySyms = [xK_a, xK_s, xK_d, xK_f, xK_g, xK_h, xK_j, xK_k, xK_l]

      makeWorkspaceMap action =
          subMap $ zipWith (\workspace key -> (key, windows $ action workspace))
                     (XMonad.workspaces conf)
                     indexKeySyms
      workspaceSelectMap = makeWorkspaceMap W.greedyView
      workspaceShiftMap = makeWorkspaceMap W.shift

      fg = highlight scheme
      bg = background scheme

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList [ myRootMap conf ]

-- Mouse bindings: default actions bound to mouse events
-------------------------------------------------------------------------------
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
  [--  ((controlMask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
  -- , ((controlMask .|. shiftMask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
  ]

-- Layouts
------------------------------------------------------------------------
myLayout = smartBorders $ avoidStruts $ tiled
           ||| mirrored
           ||| ThreeColMid 1 (3/100) (1/2)
           ||| max
  where
    tiled = named "Tall" $ ResizableTall 1 (3/100) (3/5) []
    mirrored = Mirror tiled
    max = named "Max" Full

-- Window rules:
-- > xprop | grep WM_CLASS
-------------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = manageDocks <+> composeAll
    [ isFullscreen --> doFullFloat ]
    <+> namedScratchpadManageHook myScratchpads

-- Scratchpads
-------------------------------------------------------------------------------
myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "term" spawnTerm findTerm managePad
                , NS "weechat" spawnWC findWC managePad
                ]
  where
    managePad = customFloating $ W.RationalRect l t w h
      where
        h = 0.7       -- height, 70%
        w = 0.5       -- width,  50%
        t = (1 - h)/2 -- centered left/right
        l = (1 - w)/2 -- centered left/right
    spawnTerm = "termite -t termite-scratchpad"
    findTerm = title =? "termite-scratchpad"
    spawnWC = "termite --exec='weechat-curses' --title='WEECHAT'"
    findWC = title =? "WEECHAT"

-- Status bars and logging
-------------------------------------------------------------------------------
addPad :: String -> String
addPad = wrap " " " "

colorizer :: (ColorScheme -> String) -> (String -> String)
colorizer getter = xmobarColor (getter scheme) (background scheme)

myPP :: Handle -> PP
myPP statusPipe = namedScratchpadFilterOutWorkspacePP xmobarPP
  { ppOutput = hPutStrLn statusPipe
  , ppCurrent = colorizer seperator . wrap "[" "]"
  , ppHiddenNoWindows = colorizer empty
  , ppHidden = colorizer highlight
  , ppTitle = colorizer foreground
  , ppVisible = colorizer seperator
  , ppWsSep = "  "
  , ppSep = (colorizer seperator) "  -  " }

-- Run xmonad with the settings specified. No need to modify this.
-------------------------------------------------------------------------------
main :: IO ()
main = do
  filledBar <- fillHastache ".xmonad/templates/xmobartemplate.hs"
  writeFile ".xmonad/xmobar.hs" filledBar
  bar <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"

  filledDunst <- fillHastache ".xmonad/templates/dunstrctemplate.ini"
  writeFile ".config/dunst/dunstrc" filledDunst

  filledXresources <- fillHastache ".xmonad/templates/.Xresourcestemplate"
  writeFile ".Xresources" filledXresources

  spawn "dunst"
  spawn "xrdb -merge ~/.Xresources"

  xmonad $ ewmh defaultConfig
    { terminal           = "termite"
    , focusFollowsMouse  = True
    , borderWidth        = 1
    , normalBorderColor  = foreground scheme
    , focusedBorderColor = foreground scheme
    , modMask            = mod4Mask
    , workspaces         = ["A", "S", "D", "F", "G", "H", "J", "K", "L"]

    -- bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings

    -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = fullscreenEventHook
    , logHook            = dynamicLogWithPP (myPP bar) >> updatePointer (Relative 0.5 0.5)
                           >> setWMName "LG3D"
    , startupHook        = setWMName "LG3D" }
    where
      fillHastache :: FilePath -> IO String
      fillHastache path =
        fmap L.unpack (H.hastacheFile H.defaultConfig path (mkStrContext mu))
      mu = H.MuVariable . ctx
      ctx "fg" = foreground scheme
      ctx "bg" = background scheme
      ctx "sep" = seperator scheme
