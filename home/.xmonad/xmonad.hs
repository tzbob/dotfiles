import XMonad
import Data.Monoid
import System.Exit
import System.IO

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.UpdatePointer

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named

import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map as M

myMainColor = "#BCB1B7"
myBgColor = "#6E839C"
myTextcolor = "#EFEFEF"
myLowColor = "#999999"

myNormalBorderColor = myLowColor
myFocusedBorderColor = myLowColor

-- Key bindings. Add, modify or remove key bindings here.
-------------------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- close focused window
    , ((modm .|. shiftMask, xK_c), kill)

     -- Rotate through the available layout algorithms
    , ((modm, xK_space ), sendMessage NextLayout)

    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp  )

    -- Swap the focused window and the master window
    , ((modm, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp    )

    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)

    -- Shrink a window
    , ((modm, xK_u), sendMessage MirrorShrink)

    -- Expand a window
    , ((modm, xK_i), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm .|. shiftMask, xK_h), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm .|. shiftMask, xK_l), sendMessage (IncMasterN (-1)))

    -- Volume
    , ((modm .|. controlMask , xK_j), spawn "amixer -q set Master 5- unmute")
    , ((modm .|. controlMask , xK_k), spawn "amixer -q set Master 5+ unmute")
    , ((modm .|. controlMask , xK_m), spawn "amixer set Master toggle")

    -- Cover the status bar gap
    , ((modm, xK_c), sendMessage ToggleStruts)

    -- Programs
    , ((modm, xK_p), spawn "xfce4-appfinder")
    , ((modm, xK_o), spawn ("dmenu_run -sb " ++ "\"" ++ myMainColor ++ "\""))
    , ((modm, xK_b), spawn "chromium")
    , ((modm, xK_g), spawn "gvim")
    , ((modm, xK_x), spawn "~/scala-ide/eclipse")
    , ((modm .|. shiftMask, xK_t), namedScratchpadAction myScratchpads "term")
    , ((modm .|. shiftMask, xK_n), namedScratchpadAction myScratchpads "keep")
    , ((modm .|. shiftMask, xK_q), spawn "xfce4-session-logout")
    , ((modm .|. shiftMask, xK_b), spawn "~/Dropbox/Scala/snapdim/target/start")

    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart") ]
    ++

    --
    -- mod-[asdf], Switch to workspace N
    -- mod-shift-[asdf], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_a, xK_s, xK_d, xK_f, xK_z]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


-- Mouse bindings: default actions bound to mouse events
-------------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
       >> windows W.shiftMaster)) ]

-- Layouts
------------------------------------------------------------------------
myLayout = smartBorders $ spacing 1 $ avoidStruts $ tiled ||| max
  where
    tiled = named "Tall" $ ResizableTall 1 (3/100) (3/5) []
    max = named "Max" Full

-- Window rules:
-- > xprop | grep WM_CLASS
-------------------------------------------------------------------------------
myManageHook = manageDocks <+> composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Xfce4-notifyd" --> doIgnore
    , className =? "Conky" --> doIgnore
    , className =? "Xfce4-appfinder" --> doFloat ] 
    <+> namedScratchpadManageHook myScratchpads

-- Scratchpads
-------------------------------------------------------------------------------
myScratchpads = [ NS "term" spawnTerm findTerm managePad
                , NS "keep" spawnKeep findKeep managePad ]
  where
    managePad = customFloating $ W.RationalRect l t w h
      where 
        h = 0.7       -- height, 70% 
        w = 0.5       -- width,  50%
        t = (1 - h)/2 -- centered left/right
        l = (1 - w)/2 -- centered left/right
    spawnTerm = "termite -t termite-scratchpad"
    findTerm = title =? "termite-scratchpad"
    spawnKeep = "chromium --app=https://drive.google.com/keep"
    findKeep = resource =? "drive.google.com__keep"

-- Status bars and logging
-------------------------------------------------------------------------------
addPad = wrap " " " "

myPP statusPipe = namedScratchpadFilterOutWorkspacePP xmobarPP 
  { ppOutput = hPutStrLn statusPipe
  , ppCurrent = xmobarColor myMainColor myBgColor . addPad
  , ppHiddenNoWindows = xmobarColor myLowColor "" . addPad
  , ppHidden = xmobarColor myTextcolor "" . addPad
  , ppTitle = xmobarColor myTextcolor ""
  , ppSep = xmobarColor myMainColor myBgColor "  |  " }

-- Run xmonad with the settings specified. No need to modify this.
-------------------------------------------------------------------------------
main = do
    bar <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ ewmh defaultConfig 
      { terminal           = "termite"
      , focusFollowsMouse  = True
      , borderWidth        = 0
      , modMask            = mod4Mask
      , workspaces         = ["A", "S", "D", "F", "Z"]
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor

      -- bindings
      , keys               = myKeys
      , mouseBindings      = myMouseBindings

      -- hooks, layouts
      , layoutHook         = myLayout
      , manageHook         = myManageHook
      , handleEventHook    = fullscreenEventHook
      , logHook            = dynamicLogWithPP (myPP bar)
        >> updatePointer (Relative 0.5 0.5)
      , startupHook        = setWMName "LG3D" }
