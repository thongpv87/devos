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
import XMonad.Layout.Magnifier as LM
import XMonad.Layout.PerScreen
import XMonad.Layout.Tabbed as Tabbed
import XMonad.Layout.Master as Master
import XMonad.Layout.SortedLayout
import XMonad.Layout.ThreeColumns
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
    { terminal = show Alacritty
    , modMask = mod4Mask
    , clickJustFocuses = True
    , focusFollowsMouse = False
    , borderWidth = 2
    , normalBorderColor = "#3b4252"
    , focusedBorderColor = "#E57254" -- "#E95065"
    , keys = myKeyBindings
    , workspaces = myWorkspaceNames
    , layoutHook = tallOrFull
    }

getMyConfig = undefined

main :: IO ()
main = do
    -- dp <- openDisplay ""
    -- screens <- getScreenInfo dp
    -- xqs <- xineramaQueryScreens dp
    myConfig <- mkDefaultXMonadConfig
    xmonad . ewmh . xmobarProp $ mkXConfig myConfig
