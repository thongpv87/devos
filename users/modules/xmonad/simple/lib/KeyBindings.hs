module KeyBindings where

import Config
import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Layout.MultiToggle as LMT
import XMonad.Layout.Magnifier as LM
import XMonad.Layout.MultiToggle.Instances
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.Volume(lowerVolume, raiseVolume, toggleMute)
import XMonad.Layout.Gaps
    ( Direction2D(D, L, R, U),
      gaps,
      setGaps,
      GapMessage(DecGap, ToggleGaps, IncGap) )
import XMonad.Layout.Spacing ( spacingRaw, Border(Border), toggleWindowSpacingEnabled, toggleScreenSpacingEnabled )
import XMonad.Layout.Reflect
import XMonad.Hooks.ManageDocks
import XMonad.Layout.ToggleLayouts as LTL
import Control.Monad(void)

myKeyBindings conf@(XConfig{XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch rofi and dashboard
    , ((modm,               xK_p     ), spawn "launcher")
    -- , ((modm,               xK_o     ), calcPrompt dtXPConfig' "qalc")

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), spawn "powermenu")


    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)

    , ((0,                    xF86XK_Display), spawn "autorandr --change")
    -- Screenshot
    , ((modm,                 xK_backslash), spawn "maimcopy")
    , ((modm .|. shiftMask,   xK_backslash), spawn "maimclip")
    , ((modm .|. controlMask, xK_backslash), spawn "maimscreen")

    -- others customized shortcuts
    , ((modm .|. shiftMask, xK_u) , spawn "toggle-glava toggle")
    , ((modm, xK_u )                     , spawn "toggle-glava restart")
    , ((modm, xK_v)                      , sendMessage LM.Toggle)
    , ((modm, xK_f)                      , sendMessage $ LMT.Toggle FULL)
    , ((modm .|. shiftMask, xK_f)        , sendMessage $ LMT.Toggle NBFULL)
    , ((modm, xK_z)                      , sendMessage $ LMT.Toggle MIRROR)
    , ((modm, xK_y)                      , sendMessage $ LMT.Toggle REFLECTX)
    , ((modm, xK_x)                      , sendMessage $ LMT.Toggle REFLECTY)
    , ((modm, xK_slash)                  , spawn "switch-input-method")

    -- Audio keys
    , ((0, xF86XK_AudioPlay)       , spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPrev)       , spawn "playerctl previous")
    , ((0, xF86XK_AudioNext)       , spawn "playerctl next")
    , ((0, xF86XK_AudioRaiseVolume), void (raiseVolume 3))
    , ((0, xF86XK_AudioLowerVolume), void (lowerVolume 3))
    , ((0, xF86XK_AudioMute)       , void toggleMute)
    -- , ((0,                    xF86XK_AudioRaiseVolume), spawn "~/.xmonad/bin/raise-volume.sh")
    -- , ((0,                    xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    -- , ((0,                    xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")

    -- Brightness keys
    , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl -d intel_backlight s +5% || xbacklight + 5%")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl -d intel_backlight s 5%- || xbacklight - 5%")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- GAPS!!!
    , ((modm .|. shiftMask   , xK_g), sendMessage ToggleGaps >> toggleWindowSpacingEnabled) -- toggle all spacing
    , ((modm                 , xK_g), sendMessage ToggleLayout) -- toggle all spacing

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
