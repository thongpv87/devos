--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import XMonad
import XMonad.Layout.Fullscreen
    ( fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull )
import Data.Monoid ()
import System.Exit ()
import XMonad.Util.SpawnOnce ( spawnOnce )
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext, xF86XK_Display)
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Config.Gnome( gnomeConfig )
import Control.Monad ( join, when )
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
    ( avoidStruts, docks, manageDocks, Direction2D(D, L, R, U) )
import XMonad.Hooks.ManageHelpers ( doFullFloat, doCenterFloat, isFullscreen, isDialog )
import XMonad.Layout.Spacing ( spacingRaw, Border(Border), toggleWindowSpacingEnabled, toggleScreenSpacingEnabled )
import XMonad.Layout.Gaps
    ( Direction2D(D, L, R, U),
      gaps,
      setGaps,
      GapMessage(DecGap, ToggleGaps, IncGap) )

import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle as LMT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Actions.SpawnOn
import XMonad.Layout.Magnifier as LM

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import Control.Arrow (first)
import XMonad.Prompt.Man

import XMonad.Hooks.DynamicLog
import XMonad.Util.Run ( spawnPipe
                       , runInTerm
                       , hPutStrLn
                       , runProcessWithInput)
import GHC.IO.Handle (Handle)
import Graphics.X11.Xinerama as X11
import Graphics.X11.Xlib as X11

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (maybeToList, fromMaybe)
import Data.Char (toLower, toUpper, isSpace)
import Data.List (elemIndex)
import System.Directory(getHomeDirectory)

import XMonad.Wallpaper(setRandomWallpaper)
import XMonad.Util.EZConfig(additionalKeys)
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

-- Width of the window border in pixels.
--

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
altMask         = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

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
-- Border colors for unfocused and focused windows, respectively.
--

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]


------------------------------------------------------------------------
-- Prompt. Add, modify or remove custom prompt here.
--
dtXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
dtXPKeymap = M.fromList $
     map (first $ (,) controlMask)      -- control + <key>
     [ (xK_z, killBefore)               -- kill line backwards
     , (xK_k, killAfter)                -- kill line forwards
     , (xK_a, startOfLine)              -- move to the beginning of the line
     , (xK_e, endOfLine)                -- move to the end of the line
     , (xK_m, deleteString Next)        -- delete a character foward
     , (xK_b, moveCursor Prev)          -- move cursor forward
     , (xK_f, moveCursor Next)          -- move cursor backward
     , (xK_BackSpace, killWord Prev)    -- kill the previous word
     , (xK_y, pasteString)              -- paste a string
     , (xK_g, quit)                     -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)          -- meta key + <key>
     [ (xK_BackSpace, killWord Prev)    -- kill the prev word
     , (xK_f, moveWord Next)            -- move a word forward
     , (xK_b, moveWord Prev)            -- move a word backward
     , (xK_d, killWord Next)            -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

dtXPConfig :: XPConfig
dtXPConfig = def
      { font                = "xft:FiraCode Nerd Font:style=Regular:size=12:antialias=true:hinting=true"
      , bgColor             = "#282c34"
      , fgColor             = "#bbc2cf"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = dtXPKeymap
      , position            = Bottom
      -- , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      -- , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
      -- , defaultPrompter     = unwords . map reverse . words  -- reverse the prompt
      -- , defaultPrompter     = drop 5 .id (++ "XXXX: ")  -- drop first 5 chars of prompt and add XXXX:
      , alwaysHighlight     = True
      , maxComplRows        = Just 10      -- set to 'Just 5' for 5 rows
      }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
dtXPConfig' :: XPConfig
dtXPConfig' = dtXPConfig
      { height = 20
      , position = Bottom
      , autoComplete = Nothing
      }


calcPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
    where
        trim  = f . f
            where f = reverse . dropWhile isSpace


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch rofi and dashboard
    , ((modm,               xK_p     ), spawn "~/.xmonad/bin/launcher.sh")
    -- , ((modm,               xK_o     ), calcPrompt dtXPConfig' "qalc")
    , ((modm,               xK_i     ), manPrompt dtXPConfig)

    -- Audio keys
    , ((0,                    xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0,                    xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0,                    xF86XK_AudioNext), spawn "playerctl next")
    , ((0,                    xF86XK_AudioRaiseVolume), spawn "~/.xmonad/bin/raise-volume.sh")
    , ((0,                    xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0,                    xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")

    -- Brightness keys
    , ((0,                    xF86XK_MonBrightnessUp), spawn "brightnessctl -d intel_backlight s +5% || xbacklight + 5%")
    , ((0,                    xF86XK_MonBrightnessDown), spawn "brightnessctl -d intel_backlight s 5%- || xbacklight - 5%")
    , ((0,                    xF86XK_Display), spawn "autorandr --change")
    -- Screenshot
    , ((modm,                 xK_backslash), spawn "~/.xmonad/bin/maimcopy")
    , ((modm .|. shiftMask,   xK_backslash), spawn "~/.xmonad/bin/maimsave")
    , ((modm .|. controlMask,
        xK_backslash), spawn "~/.xmonad/bin/maimscreen")
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- GAPS!!!
    , ((modm .|. controlMask, xK_g), sendMessage ToggleGaps)               -- toggle all gaps
    , ((modm .|. shiftMask  , xK_g), toggleWindowSpacingEnabled)               -- toggle window spacing
    , ((modm                , xK_g), sendMessage ToggleGaps >> toggleWindowSpacingEnabled) -- toggle all spacing

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    -- , ((modm .|. shiftMask, xK_q     ), spawn "~/.xmonad/bin/powermenu.sh")

    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))


    , ((modm,               xK_v), sendMessage LM.Toggle)
    -- , ((modm, xK_f)                      , sendMessage $ LMT.Toggle FULL)
    -- , ((modm .|. shiftMask, xK_f)        , sendMessage $ LMT.Toggle NBFULL)
    , ((modm, xK_z)                      , sendMessage $ LMT.Toggle MIRROR)
    , ((modm, xK_y)                      , sendMessage $ LMT.Toggle REFLECTX)
    , ((modm, xK_x)                      , sendMessage $ LMT.Toggle REFLECTY)
    , ((modm, xK_slash) , spawn "if [ $(ibus engine) == xkb:us::eng ]; then ibus engine Bamboo; else ibus engine xkb:us::eng ; fi")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myAdditionalKeys resCfg =
  [((myModMask .|. shiftMask, xK_q),
       spawn $ "~/.xmonad/bin/powermenu.sh " ++ show width)]
  where
    width = resWidth resCfg
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
basicTallLayout = Tall nmaster delta ratio where
  nmaster = 1
  delta   = 3/100
  ratio   = 1/2

tallLayout = named "tall"
  $ LM.magnifiercz' 1.5
  $ mkToggle (single MIRROR)
  $ mkToggle (single REFLECTX)
  $ mkToggle (single REFLECTY)
  $ basicTallLayout

fullscreenLayout = named "full" $ noBorders Full

toggleFullScreenLayout = mkToggle (NBFULL ?? EOT)
  . avoidStruts
  . mkToggle (single FULL)

addGaps layout = gaps [(L,20), (R,20), (U,40), (D,20)]
          $ spacingRaw True (Border 0 0 0 0) True (Border 5 5 5 5) True
          $ layout

myLayout =  addGaps
  $ smartBorders
  $ toggleFullScreenLayout
  $ float $ normal where
    float  = onWorkspace wsFloat simplestFloat
    normal = tallLayout ||| fullscreenLayout

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "okular"           --> shiftToWs wsDoc
    , className =? "Google-chrome"    --> shiftToWs wsWeb
    , className =? "org.gnome.Nautilus" --> doCenterFloat
    , className =? "vlc"              --> shiftToWs wsMedia
    , className =? "music-hub"        --> shiftToWs wsMedia <+> doCenterFloat
    , appName   =? "Music"            --> shiftToWs wsMedia <+> doCenterFloat
    , className =? "Thunderbird"      --> shiftToWs wsMail
    , className =? "Gnome-calculator" --> doCenterFloat
    , className =? "Pavucontrol"      --> doCenterFloat
    , className =? "MPlayer"          --> shiftToWs wsMedia
    , className =? "Gimp"             --> doFloat
    , className =? "Xmessage"         --> doCenterFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    , isDialog                        --> doCenterFloat
    , isFullscreen --> doFullFloat
    ] where shiftToWs = doShift . clickableWs

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

clickableWs ws = "<action=xdotool key super+" ++ show index ++ ">" ++ xmobarEscape ws ++ "</action>"
  where
    index = fromMaybe 1 (elemIndex ws myWorkspaces) + 1


myWorkspacePP :: Handle -> PP
myWorkspacePP h= xmobarPP {
  ppOutput = hPutStrLn h
  , ppCurrent = xmobarColor "#E5C453" "" . wrap "[" "]"
  , ppVisible = xmobarColor "#E5C453" "" . wrap "(" ")"
  , ppHidden = xmobarColor "#E57254" ""
  , ppHiddenNoWindows = xmobarColor "#E57254" ""
  , ppOrder = \(ws:_) -> [ws]
  , ppUrgent = xmobarColor "#E95065" ""
  , ppSep =  "  "
  , ppWsSep =  "  "
}

layoutSymbol layout
  | "spacing tall" == l = "\62226"
  | "spacing full" == l = "\62227"
  | otherwise = "\62210"
  where l = map toLower layout

myStatusPP h = xmobarPP {
  ppOutput = hPutStrLn h
  , ppCurrent = xmobarColor "#e86413" "" . wrap "[" "]"
  , ppOrder = \(ws:l:t:_) -> [l]
  , ppLayout = layoutSymbol
}
myClockPP h = xmobarPP {
  ppOutput = hPutStrLn h
  , ppOrder = \(ws:l:t:_) -> []
}


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X()
myStartupHook = do
  spawnOnce "xrandr --setprovideroutputsource NVIDIA-G0 modesetting; autorandr --change --force || (xrandr --output eDP-1-1 --auto || xrandr --output eDP-1 --auto)"
  spawnOnce "systemctl --user start emacs"
  spawnOnce "zsh -c 'tmux new-session -d -s default'"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "exec ~/.xmonad/bin/lock.sh"
  spawn "ibus-daemon"
  spawn "picom"
  sendMessage LM.ToggleOff

------------------------------------------------------------------------
-- Spawn xmobar pannel, need to select appropriate config
-- depend on screen resolution
getScreenWidth :: IO Int
getScreenWidth = do
  dpy <- openDisplay ""
  rects <- getScreenInfo dpy
  return $ fromIntegral $ rect_width $ head rects

data ResolutionConfig = ResolutionConfig {
  resWidth :: Int
  , resHeight :: Int
  , myBorderWidth :: Dimension
  , xmobarDir :: String
  }

resolutionConfigOf width
  | width == 3840 = ResolutionConfig { resWidth = 3840
                          , resHeight = 2160
                          , myBorderWidth = 3
                          , xmobarDir = ".xmonad/xmobar/3840"
                          }
  | otherwise =  ResolutionConfig { resWidth = 1920
                      , resHeight = 1080
                      , myBorderWidth = 2
                      , xmobarDir = ".xmonad/xmobar/1920"
                      }

configXmobarPPs :: String-> IO [PP]
configXmobarPPs xmobarCfgDir = do
  let baseCmd = "xmobar " ++ xmobarCfgDir
  hws <- spawnPipe $ baseCmd ++ "/xmobar-workspace.rc"
  hst <- spawnPipe $ baseCmd ++ "/xmobar-status.rc"
  hcl <- spawnPipe $ baseCmd ++ "/xmobar-clock.rc"
  return [myWorkspacePP hws, myStatusPP hst,  myClockPP hcl]

xmobarLogHook :: [PP] -> X ()
xmobarLogHook pps = do
  mapM_ (\h -> dynamicLogWithPP h) pps
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  setRandomWallpaper ["$HOME/Pictures/wallpapers"]
  width <- getScreenWidth
  let resCfg = resolutionConfigOf width
  homeDir <- getHomeDirectory
  pps <- configXmobarPPs $ homeDir ++ "/" ++ xmobarDir resCfg
  xmonad $ fullscreenSupport $ docks $ ewmh $ defaults resCfg pps


-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--

defaultHook :: X ()
defaultHook = return ()
-- defaults :: ResolutionConfig [PP] -> XConfig
defaults config pps = def {
      -- simple stuff
        terminal             = myTerminal
        , focusFollowsMouse  = False
        , clickJustFocuses   = True
        , borderWidth        = myBorderWidth config
        , modMask            = myModMask
        -- workspaces         = myWorkspaces,
        , workspaces = map clickableWs myWorkspaces
        , normalBorderColor  = "#3b4252"
        , focusedBorderColor = "#E57254" -- "#E95065"

      -- key bindings
        , keys               = myKeys
        -- , mouseBindings      = myMouseBindings

      -- hooks, layouts
        , manageHook = myManageHook
        , layoutHook =  myLayout
        , handleEventHook    = myEventHook
        , logHook            = xmobarLogHook pps
        -- logHook            = defaultHook,
        , startupHook        = myStartupHook >> addEWMHFullscreen
    }
  `additionalKeys` myAdditionalKeys config

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
