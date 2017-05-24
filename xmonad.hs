{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Text.Lazy as L

import Graphics.X11 (Rectangle(..))
-- TODO: Try and make guide-key
import Graphics.X11.Xlib.Misc (keysymToString)

import System.IO

import Text.Hastache.Context
import qualified Text.Hastache as H

import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest(Simplest(..))
import XMonad.Layout.SubLayouts(subLayout, onGroup, pullGroup, GroupMsg(..))
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Util.Cursor
import XMonad.Util.Font (fi)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Themes

import qualified XMonad.Actions.Submap as SM
import qualified XMonad.StackSet as W

-- UI Goodies
-------------------------------------------------------------------------------

data ColorScheme = ColorScheme
  { foreground :: String
  , background :: String
  , empty :: String
  , highlight :: String
  , seperator :: String }

scheme :: ColorScheme
scheme = ColorScheme
  { foreground = "#C0C5CE"
  , background = "#2B303B"
  , empty = "#A3BE8C" --"#B48EAC"
  , highlight = "#B18770"
  , seperator = "#BF6160" }

myBorderWidth :: Int
myBorderWidth = 3

iosevkaXftString = "xft:Iosevka:size=13:autohint=true"

-- GHC doesn't accept these special unicode chars in the parser so workaround
makeIcon :: Int -> String
makeIcon chrCode = (colorizer seperator) ("<fn=1>" ++ [chr chrCode] ++ "</fn>")

myXPConfig = defaultXPConfig { font = iosevkaXftString
                             , promptBorderWidth = 0
                             , alwaysHighlight = True
                             , bgColor = background scheme
                             , fgColor = foreground scheme
                             , bgHLight = background scheme
                             , fgHLight = seperator scheme
                             , position = Top
                             , height = 30
                             , searchPredicate = isInfixOf
                             }

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

hlRegularMode = setAllWindowBorders (foreground scheme)
hlCommandMode = setAllWindowBorders (highlight scheme)
hlRecursiveMode = setAllWindowBorders (seperator scheme)

myLeader :: (ButtonMask, KeySym)
myLeader = (0, xK_Super_L)

subMap :: [(KeySym, X())] -> X()
subMap bindings = SM.submap $ M.fromList allKeys
    where
      -- allow Super+Key as well so that keybindings still register if Super is
      -- down (only useful if the leader is Super_L or Super_R)
      allKeys = keyBindings 0 ++ keyBindings mod4Mask
      -- Try to clean up after every action that we do
      keyBindings mod = map (\(k, v) -> ((mod, k), removeEmptyWorkspaceAfter v)) bindings

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

                       , (xK_space, windowPromptGoto myXPConfig)

                       , (xK_i, workspaceSelectMap)

                       , (xK_1, sendMessage ToggleStruts)

                       , (xK_z, toggleWS)

                       -- absolute select of monitors
                       , (xK_w, onNextNeighbour W.view)
                       , (xK_e, onNextNeighbour W.shift)
                       , (xK_q, onNextNeighbour W.greedyView)

                       , (xK_r, resizeMap)
                       , (xK_l, layoutMap)
                       , (xK_m, tabMap)

                       , (xK_v, volumeMap)
                       , (xK_b, brightnessMap)

                       , (xK_period, scratchpadMap)
                       , (xK_bracketleft, programMap)

                       , (xK_c, kill)
                       , (xK_t, withFocused $ windows . W.sink)
                       , (xK_BackSpace, spawn "xmonad --recompile; xmonad --restart") ]

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

      programMap = subMap [ (xK_o, spawn "j4-dmenu-desktop --dmenu='rofi -dmenu -i'")
                          , (xK_g, spawn "touch ~/.pomodoro_session")
                          , (xK_p, spawn "rofi -show run")
                          , (xK_e, spawn "emacsclient -c -a emacs")
                          , (xK_n, spawn "nmcli_dmenu")
                          , (xK_k, spawn "keepass --auto-type-selected")
                          , (xK_b, spawn "chromium")
                          , (xK_t, spawn "termite")
                          , (xK_l, spawn "/bin/sh -c 'xset dpms force off && slock'") ]

      workspaceSelectMap = subMap $ [ (xK_c, removeEmptyWorkspace)
                                    , (xK_Return, addWorkspacePrompt myXPConfig)
                                    , (xK_space, selectWorkspace myXPConfig)
                                    , (xK_m, withWorkspace myXPConfig (windows . W.shift))
                                    , (xK_r, renameWorkspace myXPConfig) ]

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

-- Overscan with borderWidth
overscan :: Int -> l a -> ModifiedLayout Overscan l a
overscan p = ModifiedLayout (Overscan p)

data Overscan a = Overscan Int deriving (Show, Read)

instance LayoutModifier Overscan a where
  modifyLayout (Overscan p) ws (Rectangle x y w h) = runLayout ws rect
    where
      rect = Rectangle (x - fi p) (y - fi p) (w + 2 * fi p) (h + 2 * fi p)

myLayout = desktopLayoutModifiers $ tiled ||| mirrored ||| max
  where
    tiled = makeSubTabbed "vertical" tall
    mirrored = makeSubTabbed "horizontal" (Mirror tall)
    makeSubTabbed name layout = overscan myBorderWidth $ named name $ windowNavigation $ boringWindows $ subTabbed $ layout
    myTabConfig = defaultTheme { activeTextColor = seperator scheme
                               , activeColor = background scheme
                               , activeBorderColor = background scheme
                               , inactiveBorderColor = background scheme
                               , fontName = iosevkaXftString
                               , decoHeight = 28
                               }
    subTabbed  x = addTabs shrinkText myTabConfig $ subLayout [] Simplest x
    max = overscan myBorderWidth $ named "max" Full
    tall = ResizableTall 1 (3/100) (3/5) []

-- Window rules:
-- > xprop | grep WM_CLASS
-------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll [ isFullscreen --> doFullFloat ]
    <+> namedScratchpadManageHook myScratchpads
    <+> manageHook desktopConfig

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

colorizer :: (ColorScheme -> String) -> (String -> String)
colorizer getter = xmobarColor (getter scheme) (background scheme)

myPP :: Handle -> PP
myPP statusPipe = namedScratchpadFilterOutWorkspacePP xmobarPP
  { ppOutput = hPutStrLn statusPipe
  , ppCurrent = colorizer empty
  , ppHidden = colorizer highlight
  , ppTitle = (colorizer foreground) . ((++) $ (makeIcon 61769) ++ " ")
  , ppVisible = colorizer seperator
  , ppLayout = (colorizer foreground) . ((++) $ (makeIcon 62448) ++ " ")
  , ppWsSep = " "

  , ppSep = "   " }

-- Run xmonad & set up desktop 'environment'
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

  xmonad desktopConfig
    { terminal           = "termite"
    , focusFollowsMouse  = True
    , borderWidth        = fi myBorderWidth
    , normalBorderColor  = foreground scheme
    , focusedBorderColor = foreground scheme
    , modMask            = mod4Mask
    , workspaces         = ["inbox", "dev", "paper"]

    -- bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings

    -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = fullscreenEventHook <+> handleEventHook desktopConfig
    , logHook            = dynamicLogWithPP (myPP bar) >> updatePointer (0.5, 0.5) (0, 0)
                           <+> logHook desktopConfig
    , startupHook = startupHook desktopConfig <+> setWMName "LG3D"
    }
    where
      -- Fill in Mustache templates
      fillHastache :: FilePath -> IO String
      fillHastache path =
        fmap L.unpack (H.hastacheFile H.defaultConfig path (mkStrContext mu))
      mu = H.MuVariable . ctx
      ctx "fg" = foreground scheme
      ctx "bg" = background scheme
      ctx "sep" = seperator scheme
