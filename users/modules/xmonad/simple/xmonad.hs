{-# LANGUAGE RecordWildCards  #-}
module Main where

import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import Graphics.X11.Xrandr

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen)
import Graphics.X11.Xinerama (getScreenInfo, compiledWithXinerama, xineramaQueryScreens)
import XMonad.StackSet (Screen(screenDetail))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import Config
import KeyBindings

data Terminal
    = Alacritty
    | GnomeTerminal

instance Show Terminal where
    show Alacritty = "alacritty"
    show GnomeTerminal = "gnome-terminal"

myWorkspaces =
    [ ("cmd", "\61728")
    , ("web", "\62845")
    , ("code", "\61729")
    , ("doc", "\62744")
    , ("note", "\61614")
    , ("com", "\61664")
    , ("float", "\62029")
    , ("media", "\63612")
    , ("remote", "\61705")
    ]

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
    , workspaces = snd <$> myWorkspaces
    }

getMyConfig = undefined

main :: IO ()
main = do
    -- dp <- openDisplay ""
    -- screens <- getScreenInfo dp
    -- xqs <- xineramaQueryScreens dp
    myConfig <- mkDefaultXMonadConfig
    xmonad . ewmh . xmobarProp $ mkXConfig myConfig
