{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
import XMonad
import System.IO

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

import XMonad.Layout.Tabbed(defaultTheme, shrinkText,
                            TabbedDecoration, addTabs)
import XMonad.Layout.Simplest(Simplest(..))
import XMonad.Layout.SubLayouts(subLayout, onGroup, pullGroup, GroupMsg(..))

import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows

import XMonad.Layout.NoBorders
import XMonad.Layout.Named

import XMonad.Util.Themes
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Util.Themes

import qualified XMonad.StackSet as W

import qualified Data.Map as M

import XMonad.Layout.LayoutModifier
import Graphics.X11 (Rectangle(..))
import XMonad.Util.Font (fi)

data ColorScheme = ColorScheme
  { foreground :: String
  , background :: String
  , empty :: String
  , hidden :: String
  , highlight :: String
  , seperator :: String }

scheme :: ColorScheme
scheme = ColorScheme
  { foreground = "#C0C5CE"
  , background = "#2B303B"
  , empty = "#90a4ae"
  , hidden = "#C0C5CE"
  , highlight = "#B18770"
  , seperator = "#BF6160" }

myBorderWidth :: Int
myBorderWidth = 3

iosevkaXftString = "xft:Iosevka:size=12"

-- Overscan with borderWidth
overscan :: Int -> l a -> ModifiedLayout Overscan l a
overscan p = ModifiedLayout (Overscan p)

data Overscan a = Overscan Int deriving (Show, Read)

instance LayoutModifier Overscan a where
  modifyLayout (Overscan p) ws (Rectangle x y w h) = runLayout ws rect
    where
      rect = Rectangle (x - fi p) (y - fi p) (w + 2 * fi p) (h + 2 * fi p)

-- Micro states | Modal WM
-------------------------------------------------------------------------------

setAllWindowBorders :: String -> X ()
setAllWindowBorders cs =
  do
    XState { windowset = ws } <- get
    XConf { display = d } <- ask

    c' <- io $ initColor d cs

    let windows = W.allWindows ws
        setWindowBorder' c w = io $ setWindowBorder d w c

    case c' of
      Just c -> mapM_ (setWindowBorder' c) windows
      _      -> io $ hPutStrLn stderr $ concat ["Warning: bad border color ", show cs]

hlCommandMode = setAllWindowBorders (foreground scheme)
hlRegularMode = setAllWindowBorders (highlight scheme)
hlRecursiveMode = setAllWindowBorders (seperator scheme)

myLeader :: (ButtonMask, KeySym)
myLeader = (0, xK_Super_L)

subMap :: [(KeySym, X())] -> X()
subMap bindings = SM.submap $ M.fromList allKeys
    where
      allKeys = keyBindings ++ keyBindings'
      keyBindings = map (\(k, v) -> ((0, k), v)) bindings
      -- allow Super+Key as well so that keybindings still register if Super is
      -- down (only usefull if the leader is Super_L or Super_R)
      keyBindings' = map (\(k, v) -> ((mod4Mask, k), v)) bindings

recursiveSubMap :: [(KeySym, X())] -> X()
recursiveSubMap bindings = sm
    where
      recursiveBindings = map (\(k, v) -> (k, v >> hlRecursiveMode >> sm)) bindings
      sm = subMap recursiveBindings

-- Key bindings. Add, modify or remove key bindings here.
-------------------------------------------------------------------------------

myRootMap :: XConfig Layout -> ((ButtonMask, KeySym), X ())
myRootMap conf = (myLeader, hlCommandMode >> rootMap >> hlRegularMode)
    where
      rootMap = subMap [ (xK_j, focusDown)
                       , (xK_k, focusUp)
                       , (xK_Tab, onGroup W.focusUp')

                       , (xK_s, shiftMap)

                       , (xK_space, spawn "rofi -show window")

                       , (xK_i, workspaceSelectMap)
                       , (xK_o, workspaceShiftMap)

                       , (xK_1, sendMessage ToggleStruts)

                       , (xK_w, nextScreen)
                       , (xK_e, shiftNextScreen)

                       , (xK_r, resizeMap)
                       , (xK_l, layoutMap)
                       , (xK_m, tabMap)

                       , (xK_v, volumeMap)
                       , (xK_b, brightnessMap)

                       , (xK_period, scratchpadMap)
                       , (xK_bracketleft, programMap)

                       , (xK_c, kill)
                       , (xK_t, withFocused $ windows . W.sink)
                       , (xK_q, spawn "killall lemonbar; xmonad --recompile; xmonad --restart") ]

      focusMap = recursiveSubMap [ (xK_k, windows W.focusUp)
                                 , (xK_Return, windows W.focusMaster)
                                 , (xK_j, windows W.focusDown) ]

      tabMap = recursiveSubMap [ (xK_h, sendMessage $ pullGroup L)
                               , (xK_l, sendMessage $ pullGroup R)
                               , (xK_k, sendMessage $ pullGroup U)
                               , (xK_j, sendMessage $ pullGroup D)
                               , (xK_m, withFocused (sendMessage . MergeAll))
                               , (xK_u, withFocused (sendMessage . UnMerge))
                               ]

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

      volumeMap = recursiveSubMap [ (xK_j, spawn "amixer -D pulse sset Master 5%- unmute")
                                  , (xK_k, spawn "amixer -D pulse sset Master 5%+ unmute")
                                  , (xK_l, spawn "playerctl next")
                                  , (xK_h, spawn "playerctl previous")
                                  , (xK_Return, spawn "playerctl play-pause")
                                  , (xK_m, spawn "amixer -D pulse set Master toggle") ]

      brightnessMap = recursiveSubMap [ (xK_j, spawn "xbacklight - 15")
                                      , (xK_k, spawn "xbacklight + 15")
                                      , (xK_m, spawn "xbacklight = 100") ]

      scratchpadMap = subMap [ (xK_t, namedScratchpadAction myScratchpads "term")
                             , (xK_k, namedScratchpadAction myScratchpads "keepass")
                             , (xK_i, namedScratchpadAction myScratchpads "inbox")
                             , (xK_w, namedScratchpadAction myScratchpads "weechat") ]

      programMap = subMap [ (xK_o, spawn "j4-dmenu-desktop --dmenu='rofi -dmenu -p'")
                          , (xK_p, spawn "rofi -show run")
                          , (xK_e, spawn "emacsclient -c -a emacs")
                          , (xK_n, spawn "nmcli_dmenu")
                          , (xK_k, spawn "keepass --auto-type")
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
myLayout = avoidStruts $ tiled ||| mirrored ||| max
  where
    tiled = overscan myBorderWidth $ named "Tabbed Horizontal" $ windowNavigation $ boringWindows $ subTabbed $ tall
    mirrored = overscan myBorderWidth $ named "Tabbed Vertical" $ windowNavigation $ boringWindows $ subTabbed $ Mirror $ tall
    myTabConfig = defaultTheme { activeTextColor = seperator scheme
                               , activeColor = background scheme
                               , activeBorderColor = background scheme
                               , inactiveBorderColor = background scheme
                               , fontName = iosevkaXftString
                               , decoHeight = 28
                               }
    subTabbed  x = addTabs shrinkText myTabConfig $ subLayout [] Simplest x
    max = named "Max" Full
    tall = ResizableTall 1 (3/100) (3/5) []

-- Window rules:
-- > xprop | grep WM_CLASS
-------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = manageDocks <+> composeAll
    [ isFullscreen --> doFullFloat
    , resource =? "xmobar" --> doSideFloat SC]
    <+> namedScratchpadManageHook myScratchpads

-- Scratchpads
-------------------------------------------------------------------------------

myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "term" spawnTerm findTerm managePad
                , NS "weechat" spawnWC findWC managePad
                , NS "keepass" spawnKP findKP managePad
                , NS "inbox" spawnIB findIB managePad
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
    spawnKP = "keepass"
    findKP = className =? "KeePass2"
    spawnIB = "chromium --app='https://inbox.google.com'"
    findIB = resource =? "inbox.google.com"

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
    , borderWidth        = fi myBorderWidth
    , normalBorderColor  = highlight scheme
    , focusedBorderColor = highlight scheme
    , modMask            = mod4Mask
    , workspaces         = ["A", "S", "D", "F", "G", "H", "J", "K", "L"]

    -- bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings

    -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = fullscreenEventHook
    , logHook            = dynamicLogWithPP (myPP bar) >> updatePointer (0.5, 0.5) (0, 0)
    , startupHook        = setWMName "LG3D" }
    where
      fillHastache :: FilePath -> IO String
      fillHastache path =
        fmap L.unpack (H.hastacheFile H.defaultConfig path (mkStrContext mu))
      mu = H.MuVariable . ctx
      ctx "fg" = foreground scheme
      ctx "bg" = background scheme
      ctx "sep" = seperator scheme
