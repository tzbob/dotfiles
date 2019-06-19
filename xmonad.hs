{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Text.Lazy as L

import System.IO
import System.Directory
import XMonad.Util.Run

import Text.Hastache.Context
import qualified Text.Hastache as H

import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.DynamicProjects
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.ResizableTile
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
  { foreground = "#999"
  , background = "#FFF"
  , empty = "#e12f2f"
  , highlight = "#e12f2f"
  , seperator = "#333" }

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
      rootMap = subMap [ (xK_j, windows W.focusDown)
                       , (xK_k, windows W.focusUp)

                       , (xK_s, shiftMap)

                       , (xK_BackSpace, spawn "rofi -show window")

                       , (xK_n, ravenMap)

                       , (xK_i, workspaceSelectMap)

                       , (xK_1, sendMessage ToggleStruts)

                       , (xK_z, toggleWS)

                       -- absolute select of monitors
                       , (xK_w, onNextNeighbour def W.view)
                       , (xK_e, onNextNeighbour def W.shift)
                       , (xK_q, onNextNeighbour def W.greedyView)

                       , (xK_r, resizeMap)
                       , (xK_l, layoutMap)

                       , (xK_v, volumeMap)
                       , (xK_b, brightnessMap)

                       , (xK_period, scratchpadMap)
                       , (xK_bracketleft, programMap)

                       , (xK_c, kill)
                       , (xK_t, withFocused $ windows . W.sink)
                       , (xK_Delete, spawn "xmonad --recompile; xmonad --restart") ]

      ravenMap = subMap [ (xK_h, ravenSend "ToggleNotificationsView")
                        , (xK_l, ravenSend "ClearNotifications")
                        , (xK_space, ravenToggleNotifications) ]

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

      volumeMap = recursiveSubMap [ (xK_j, spawn "amixer sset Master 5%- unmute")
                                  , (xK_k, spawn "amixer sset Master 5%+ unmute")
                                  , (xK_l, spawn "playerctl next")
                                  , (xK_h, spawn "playerctl previous")
                                  , (xK_Return, spawn "playerctl play-pause")
                                  , (xK_m, spawn "amixer set Master toggle") ]

      brightnessMap = recursiveSubMap [ (xK_j, spawn "xbacklight - 15")
                                      , (xK_k, spawn "xbacklight + 15")
                                      , (xK_m, spawn "xbacklight = 100") ]

      scratchpadMap = subMap [ (xK_k, namedScratchpadAction myScratchpads "keepassxc")
                             ]

      programMap = subMap [ (xK_o, spawn "j4-dmenu-desktop --dmenu='rofi -dmenu -i'")
                          , (xK_g, spawn "rofi -dmenu > ~/.pomodoro_session")
                          , (xK_p, spawn "rofi -show run")
                          , (xK_e, spawn "emacsclient -c -a emacs")
                          , (xK_k, spawn "keepass --auto-type-selected")
                          , (xK_n, spawn "rofi -dmenu | xargs -I {} xdg-open 'mailto:tzbobr@gmail.com?subject=Note: {}&body={}'")
                          , (xK_b, spawn "chromium")
                          , (xK_t, spawn "gnome-terminal")
                          , (xK_l, spawn "/bin/sh -c 'xset dpms force off && slock'") ]

      workspaceSelectMap = subMap $ [ (xK_c, removeEmptyWorkspace)
                                    , (xK_Return, addWorkspacePrompt myXPConfig)
                                    , (xK_space, selectWorkspace myXPConfig)
                                    , (xK_BackSpace, selectWorkspace myXPConfig)
                                    , (xK_m, withWorkspace myXPConfig (windows . W.shift))
                                    , (xK_r, renameWorkspace myXPConfig) ]
      ravenSend method = spawn $ "dbus-send --type=method_call --dest=org.budgie_desktop.Raven /org/budgie_desktop/Raven org.budgie_desktop.Raven." ++ method
      ravenToggleNotifications = do
        rawReply <- liftIO $ runProcessWithInput "dbus-send" [ "--type=method_call"
                                            , "--dest=org.budgie_desktop.Raven"
                                            , "--print-reply"
                                            , "/org/budgie_desktop/Raven"
                                            , "org.budgie_desktop.Raven.GetDoNotDisturbState" ]
                                            ""
        if isInfixOf "true" rawReply then
          ravenSend "SetDoNotDisturb boolean:false"
        else ravenSend "SetDoNotDisturb boolean:true"

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
    tiled = makeSub "vertical" tall
    mirrored = makeSub "horizontal" (Mirror tall)
    makeSub name layout = overscan myBorderWidth $ named name $ windowNavigation $ layout
    max = overscan myBorderWidth $ named "max" Full
    tall = ResizableTall 1 (3/100) (3/5) []

-- Window rules:
-- > xprop | grep WM_CLASS
-------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll [ -- isFullscreen --> doFullFloat
                           className =? "budgie-helper" --> doIgnore
                          ]
    <+> namedScratchpadManageHook myScratchpads
    <+> manageHook desktopConfig

-- Scratchpads
-------------------------------------------------------------------------------

myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "keepassxc" spawnKPXC findKPXC managePad
                ]
  where
    managePad = customFloating $ W.RationalRect l t w h
      where
        h = 0.7       -- height, 70%
        w = 0.5       -- width,  50%
        t = (1 - h)/2 -- centered left/right
        l = (1 - w)/2 -- centered left/right
    spawnKP = "keepass"
    findKP = className =? "KeePass2"
    spawnKPXC = "keepassxc"
    findKPXC = className =? "keepassxc"

-- Projects
-------------------------------------------------------------------------------

myProjects :: [Project]
myProjects =
  [ Project { projectName      = "inbox"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "chromium"
            }
  , Project { projectName      = "emacs"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "emacsclient -c -a emacs"
            }
  , Project { projectName      = "media"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "spotify"
            }
  , Project { projectName      = "chat"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "signal-desktop"
                                           spawn "chromium --app=https://messenger.com"
                                           spawn "/opt/IGdm/igdm"
            }
  ]

-- Status bars and logging
-------------------------------------------------------------------------------

colorizer :: (ColorScheme -> String) -> (String -> String)
colorizer getter = xmobarColor (getter scheme) (background scheme)

myPP :: Handle -> PP
myPP statusPipe = namedScratchpadFilterOutWorkspacePP xmobarPP
  { ppOutput = hPutStrLn statusPipe
  , ppCurrent = colorizer highlight
  , ppHidden = colorizer foreground
  , ppTitle = (colorizer foreground) . ((++) $ (makeIcon 62318) ++ " ")
  , ppVisible = colorizer seperator
  , ppLayout = (colorizer foreground) . ((++) $ (makeIcon 62448) ++ " ")
  , ppWsSep = " "
  , ppSep = "   " }

-- Run xmonad & set up desktop 'environment'
-------------------------------------------------------------------------------
main :: IO ()
main = do
  liftIO $ putStrLn "starting XMONAD"
  h <- getHomeDirectory

  filledBar <- fillHastache $ h ++ "/.xmonad/templates/xmobartemplate.hs"
  writeFile (h ++ "/.xmonad/xmobar.hs") filledBar

  filledDunst <- fillHastache $ h ++ "/.xmonad/templates/dunstrctemplate.ini"
  writeFile (h ++ "/.config/dunst/dunstrc") filledDunst

  filledXresources <- fillHastache $ h ++ "/.xmonad/templates/Xresourcestemplate"
  writeFile (h ++ "/.Xresources") filledXresources

  bar <- spawnPipe "~/.local/bin/xmobar ~/.xmonad/xmobar.hs"

  spawn $ "xrdb -merge " ++ h ++ "/.Xresources"
  spawn $ "nitrogen --restore"

  xmonad $ dynamicProjects myProjects desktopConfig
    { terminal           = "gnome-terminal"
    , focusFollowsMouse  = True
    , borderWidth        = fi myBorderWidth
    , normalBorderColor  = foreground scheme
    , focusedBorderColor = foreground scheme
    , modMask            = mod4Mask
    , workspaces         = ["inbox", "media", "chat"]

    -- bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings

    -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = fullscreenEventHook <+> handleEventHook desktopConfig
    , logHook            = logHook desktopConfig >> dynamicLogWithPP (myPP bar) >> updatePointer (0.5, 0.5) (0, 0)
    --, logHook            =  logHook desktopConfig >> updatePointer (0.5, 0.5) (0, 0)
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
