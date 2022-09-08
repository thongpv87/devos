{-# LANGUAGE RecordWildCards  #-}
module Main where

import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import Graphics.X11.Xrandr

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen)
import Graphics.X11.Xinerama (getScreenInfo, compiledWithXinerama, xineramaQueryScreens)
import XMonad.Layout.Gaps(Direction2D(..), gaps, setGaps, GapMessage(..))
import XMonad.Layout
import XMonad.StackSet (Screen(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WallpaperSetter
import XMonad.Layout.Magnifier as LM
import XMonad.Layout.PerScreen
import XMonad.Layout.Tabbed as Tabbed
import XMonad.Layout.Master as Master
import XMonad.Layout.SortedLayout
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen
import XMonad.Util.Loggers
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.SpawnOnce
import Config
import KeyBindings
import Layouts
import Common

data Terminal
    = Alacritty
    | GnomeTerminal

instance Show Terminal where
    show Alacritty = "alacritty"
    show GnomeTerminal = "gnome-terminal"


-- mkXConfig :: XMonadConfig -> XConfig l
mkXConfig XMonadConfig{..} = def
    { terminal = show GnomeTerminal
    , modMask = mod4Mask
    , clickJustFocuses = True
    , focusFollowsMouse = False
    , borderWidth = 2
    , normalBorderColor = "#3b4252"
    , focusedBorderColor = "#E57254" -- "#E95065"
    , keys = myKeyBindings
    , workspaces = myWorkspaceNames
    , manageHook = myManageHook
    , layoutHook = tallOrFull
    , startupHook = myStartupHook
    }

statusbarPP :: PP
statusbarPP = def
    { ppSep             = magenta " | "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = yellow . xmobarBorder "Top" "#8be9fd" 2
    , ppVisible         = yellow
    , ppHidden          = white . wrap "" ""
    , ppHiddenNoWindows = lowWhite . wrap "" ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, gray, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#fbfbf8" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    gray     = xmobarColor "#666666" ""
    lowWhite = xmobarColor "#bbbbbb" ""


myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "org.gnome.Nautilus" --> doCenterFloat
    , className =? "music-hub"        --> doCenterFloat
    , appName   =? "Music"            --> doCenterFloat
    , className =? "Thunderbird"      --> shiftToWs Mail
    , className =? "Gnome-calculator" --> doCenterFloat
    , className =? "Pavucontrol"      --> doCenterFloat
    , className =? "Gimp"             --> doFloat
    , className =? "Xmessage"         --> doCenterFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    , className =? "trayer"           --> doIgnore
    , isDialog                        --> doCenterFloat
    , isFullscreen --> doFullFloat
    ] where shiftToWs = doShift . wsName

myStartupHook = do
    spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true  --expand true --width 5 --transparent true --alpha 0 --tint 0x22242b --height 24 --padding 5 --iconspacing 3"
    spawnOnce "systemctl --user start emacs"
    spawn "ibus-daemon"
    spawn "xsetroot -cursor_name left_ptr"
    spawn "nm-applet"
    spawn "feh --bg-fill ~/.wallpapers/default"

setWallpaper = wallpaperSetter defWallpaperConf
               { wallpapers = WallpaperList ((\ws->(ws,WallpaperDir "default")) <$> myWorkspaceNames)
               , wallpaperBaseDir = "~/.wallpapers/"
               }

main :: IO ()
main = do
    -- dp <- openDisplay ""
    -- screens <- getScreenInfo dp
    -- xqs <- xineramaQueryScreens dp
    myConfig <- mkDefaultXMonadConfig
    xmonad
        . ewmh
        . withSB (statusBarProp "statusbar" (pure statusbarPP))
        $ mkXConfig myConfig
