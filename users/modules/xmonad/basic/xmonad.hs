import XMonad
import System.IO
import System.Exit

import XMonad.Util.Run                ( spawnPipe )
import XMonad.Util.EZConfig           ( additionalKeys )
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce

import XMonad.Wallpaper

import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive      ( fadeInactiveLogHook )
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicProjects

import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing          ( smartSpacing )
import XMonad.Layout.NoBorders        ( smartBorders
                                        , noBorders
                                      )
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SubLayouts
import XMonad.Layout.Named
import XMonad.Layout.Simplest
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)


import XMonad.Config.Gnome
import System.Directory(getHomeDirectory)
import qualified Data.Map                      as M
import qualified XMonad.StackSet               as W
import qualified Graphics.X11.ExtraTypes.XF86  as XF86

import XMonad.Hooks.DynamicLog ( dynamicLogWithPP
                               , PP (..)
                               , xmobarPP
                               , xmobar
                               , dynamicLog )
import XMonad.Util.Run ( spawnPipe
                       , hPutStrLn
                       , runInTerm )
import GHC.IO.Handle (Handle)

myModKey = mod4Mask
myLauncher =
        "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun"
-- myScreensaver = "dm-tool switch-to-greeter"
myScreensaver = "xset dpms force off"
myStartupHook = do
        startupHook gnomeConfig
        spawnOnce "xrandr --setprovideroutputsource NVIDIA-G0 modesetting; autorandr --change --force || (xrandr --output eDP-1-1 --auto || xrandr --output eDP-1 --auto)"
        spawnOnce "notify-osd"
        spawnOnce "systemctl --user start emacs"
        spawn "ibus-daemon"
        spawn "setxkbmap -model thinkpad -layout us -option ctrl:nocaps -option altwin:prtsc_rwin"
        -- spawn "picom"

-- myLogHook = do
--         logHook gnomeConfig
myLogHook :: Handle -> X()
myLogHook = dynamicLogWithPP . myXMobarPP
myXMobarPP :: Handle -> PP
myXMobarPP h = xmobarPP {ppOutput          = hPutStrLn h
                        , ppOrder           = \ (wss:_) -> [wss]
                        , ppWsSep           = " " }
--spawnXMobar = spawnPipe $ unwords [ "xmobar", "~/.xmonad/xmobarrc" ]

myFadeHook = composeAll [opaque]

wsCmd     = "\61728"
wsWeb     = "\62845"
wsCode    = "\61729"
wsDoc     = "\62744"
wsNote    = "\61614"
wsMail    = "\61664"
wsFloat   = "\62029"
wsMedia   = "\63612"
wsRemote  = "\61705"

myWorkspaces = [wsCmd, wsWeb, wsCode, wsDoc, wsNote, wsMail, wsFloat, wsMedia, wsRemote]
projects :: [Project]
projects = [ Project { projectName = wsCmd
                     , projectDirectory = "~/"
                     , projectStartHook = Nothing
                     }
             , Project { projectName = wsWeb
                     , projectDirectory = "~/"
                     , projectStartHook = Just $ do
                         spawnOn wsWeb "google-chrome-stable"
                     }
             , Project { projectName = wsCode
                     , projectDirectory = "~/ws"
                     , projectStartHook = Just $ do
                         spawnOn wsCode "alacritty -e tmux new-session -A -s code"
                     }
             , Project { projectName = wsDoc
                     , projectDirectory = "~/"
                     , projectStartHook = Nothing
                     }
             , Project { projectName = wsNote
                     , projectDirectory = "~/note"
                     , projectStartHook = Just $ do
                         spawnOn wsNote "alacritty -e tmux new-session -A -s todo"
                     }
             , Project { projectName = wsMail
                     , projectDirectory = "~/"
                     , projectStartHook = Just $ do
                         spawnOn wsMail "thunderbird"
                         spawnOn wsMail "alacritty -e tmux new-session -A -s irssi"
                     }
             , Project { projectName = wsFloat
                     , projectDirectory = "~/"
                     , projectStartHook = Nothing
                     }
             , Project { projectName = wsMedia
                     , projectDirectory = "~/"
                     , projectStartHook = Just $ do
                         spawn "spotify"
                     }
             , Project { projectName = wsRemote
                     , projectDirectory = "~/"
                     , projectStartHook = Nothing
                     }
           ]

myManageHook = composeAll
        [ isDialog --> doFloat
        , className =? "okular"           --> doShift wsDoc
        , className =? "Google-chrome"    --> doShift wsWeb
        , className =? "Spotify"          --> doShift wsMedia
        , className =? "vlc"              --> doShift wsMedia
        , className =? "Thunderbird"      --> doShift wsMail
        , isFullscreen                    --> doFullFloat
        , className =? "Gnome-calculator" --> doCenterFloat
        , className =? "Pavucontrol"      --> doCenterFloat
        , isDialog                        --> doCenterFloat
       ]

-----------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------- SHORTCUTS  --------------------------------------------------------------------------------------------

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
        [ ((myModKey .|. controlMask, xK_Escape), spawn "systemctl suspend")
        , ((0, XF86.xF86XK_Display), spawn "autorandr --change")
        , ((0, XF86.xF86XK_MonBrightnessUp)     , spawn "brightnessctl -d intel_backlight s +5% || xbacklight + 5%")
        , ((0, XF86.xF86XK_MonBrightnessDown)   , spawn "brightnessctl -d intel_backlight s 5%- || xbacklight - 5%")
        , ((0, XF86.xF86XK_AudioRaiseVolume)
          , spawn "~/.xmonad/bin/raise-volume.sh")
        , ((0, XF86.xF86XK_AudioLowerVolume)
          , spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ((0, XF86.xF86XK_AudioMute)
          , spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

        --take a screenshot of entire display
        , ( (myModKey, xK_backslash)
          , spawn "~/.xmonad/bin/maimcopy"
          )
        --take a screenshot of focused window
        , ( (myModKey .|. shiftMask, xK_backslash)
          , spawn "~/.xmonad/bin/maimsave"
          )

        -- Layout toggles
        , ((myModKey, xK_f)                      , sendMessage $ Toggle NBFULL)
        , ((myModKey .|. shiftMask, xK_f)        , sendMessage $ Toggle FULL)
        , ((myModKey, xK_z)                      , sendMessage $ Toggle MIRROR)
        , ((myModKey, xK_y)                      , sendMessage $ Toggle REFLECTX)
        , ((myModKey, xK_x)                      , sendMessage $ Toggle REFLECTY)
        , ((myModKey, xK_p)                      , spawn myLauncher)
        , ((myModKey .|. shiftMask, xK_q)        , io (exitWith ExitSuccess))
        , ((myModKey, xK_q)                      , restart "xmonad" True)

        -- Input method toggles
        , ((myModKey, xK_slash) , spawn "if [ $(ibus engine) == xkb:us::eng ]; then ibus engine Bamboo; else ibus engine xkb:us::eng ; fi")
        ]



-----------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------- LAYOUT HOOK --------------------------------------------------------------------------------------------
basicTallLayout = Tall nmaster delta ratio where
  nmaster = 1
  delta   = 3/100
  ratio   = 1/2

tallLayout = named "tall"
  $ mkToggle (single MIRROR)
  $ mkToggle (single REFLECTX)
  $ mkToggle (single REFLECTY)
  $ basicTallLayout

tabbedLayout = named "tab" $ noBorders simpleTabbed

fullscreenLayout = named "fullscreen" $ noBorders Full

toggleFullScreenLayout = mkToggle (NBFULL ?? EOT)
  . avoidStruts
  . mkToggle (single FULL)

myLayoutHook =
  smartBorders
  $ toggleFullScreenLayout
  $ float $ normal where
    float  = onWorkspace wsFloat simplestFloat
    normal = tallLayout ||| tabbedLayout

-----------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------- MAIN------- --------------------------------------------------------------------------------------------
main = do
  n <- countScreens
  homeDir <- getHomeDirectory
  setRandomWallpaper [ show homeDir ++ "/Pictures/wallpapers"]
  xmproc <- spawnPipe $ "xmobar " ++ show homeDir ++  "/.xmonad/xmobarrc"
  xmonad
    -- $ dynamicProjects projects
    $ ewmh $ gnomeConfig
    { modMask            = myModKey -- Use Super instead of Alt
    , manageHook         = manageDocks <+> myManageHook <+> manageHook gnomeConfig
    , handleEventHook    = handleEventHook defaultConfig  <+> fullscreenEventHook <+> docksEventHook
    , terminal           = "alacritty"
    , borderWidth        = 3
    , focusedBorderColor = "#00FFFF"
    , normalBorderColor  = "#AAAAAA"
    , startupHook = gnomeRegister >> myStartupHook
    , layoutHook         =  myLayoutHook
    , logHook = dynamicLogWithPP $ myXMobarPP xmproc
    -- , workspaces         = myWorkspaceIds
    , workspaces = myWorkspaces
    }
    `additionalKeys` myAdditionalKeys
