import XMonad
import Data.Monoid
import System.Exit

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.UpdatePointer

import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

import XMonad.Util.Run(spawnPipe)
import System.IO(hPutStrLn)

import qualified XMonad.StackSet as W
import qualified Data.Map as M

myMainColor = "#C6361E"
myBgColor = "#333333"
myTextcolor = "#EFEFEF"
myLowColor = "#999999"

myNormalBorderColor = myBgColor
myFocusedBorderColor = myBgColor

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
    , ((modm, xK_v), spawn "~/bin/toggleTrayer.sh" >> refresh)
    , ((modm, xK_g), spawn "gvim")
    , ((modm, xK_x), spawn "~/scala-ide/eclipse")
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
myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled = ResizableTall 1 (3/100) (3/5) []

-- Window rules:
-- > xprop | grep WM_CLASS
-------------------------------------------------------------------------------
myManageHook = manageDocks <+> composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Xfce4-notifyd" --> doIgnore
    , className =? "Conky" --> doIgnore
    , className =? "Xfce4-appfinder" --> doFloat ]

-- Event handling
-------------------------------------------------------------------------------
myEventHook = fullscreenEventHook

-- Status bars and logging
-------------------------------------------------------------------------------
addPad = wrap " " " "

myPP statusPipe = xmobarPP {
    ppOutput = hPutStrLn statusPipe
    , ppCurrent = xmobarColor myMainColor myBgColor . addPad
    , ppHiddenNoWindows = xmobarColor myLowColor "" . addPad
    , ppHidden = xmobarColor myTextcolor "" . addPad
    , ppTitle = xmobarColor myTextcolor ""
    , ppSep = xmobarColor myMainColor myBgColor "  |  "
}

myLogHook pipe = dynamicLogWithPP (myPP pipe) >> updatePointer (Relative 0.5 0.5)

-- Startup hook
-------------------------------------------------------------------------------
myStartupHook = setWMName "LG3D"

-- Configuration structure
-------------------------------------------------------------------------------
defaults statusPipe = ewmh defaultConfig {
    -- simple stuff
    terminal           = "termite",
    focusFollowsMouse  = True,
    borderWidth        = 1,
    modMask            = mod4Mask,
    workspaces         = ["A", "S", "D", "F", "Z"],
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = myLogHook statusPipe,
    startupHook        = myStartupHook
}

-- Run xmonad with the settings specified. No need to modify this.
-------------------------------------------------------------------------------
main = do
    statusPipe <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ defaults statusPipe
