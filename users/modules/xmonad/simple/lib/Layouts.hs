module Layouts where

import XMonad.Layout
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen
    ( fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull )
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing ( spacingRaw, Border(..), toggleWindowSpacingEnabled, toggleScreenSpacingEnabled )
import XMonad.Layout.SimplestFloat
import XMonad.Layout.LimitWindows
import XMonad.Layout.PerWorkspace
import XMonad.Layout.PerScreen
import XMonad.Layout.Reflect
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MultiToggle as LMT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.Gaps
import XMonad.Layout.Magnifier as LM
import XMonad.Hooks.ManageDocks
import Common

basicTallLayout = Tall nmaster delta ratio where
  nmaster = 1
  delta   = 3/100
  ratio   = 1/2

tallLayout = named "Tall"
  $ LM.magnifierOff -- $ LM.magnifiercz' 1.5
  $ mkToggle (single MIRROR)
  $ mkToggle (single REFLECTX)
  $ mkToggle (single REFLECTY)
  $ limitWindows 4 basicTallLayout

verticalLayout =
    named "Vert"
    $ LM.magnifiercz' 1.5
    $ reflectVert
    $ limitWindows 5
    $ Mirror $ ThreeCol 1 (3/100) 0.4

toggleGapsLayout layout = toggleLayouts layout (addGaps layout)

toggleFullScreenLayout = mkToggle (NBFULL ?? EOT)
  . avoidStruts
  . mkToggle (single FULL)

addGaps layout = gaps [(L,20), (R,20), (U,20), (D,20)]
          $ spacingRaw True (Border 0 0 0 0) False (Border 10 10 10 10) True layout

tallOrFull =
    smartBorders
    $ toggleFullScreenLayout
    $ onWorkspace (wsName Float) simplestFloat
    $ layouts
    where
        layouts =
            ifWider 1920 (ifWider 3800 (toggleGapsLayout tallLayout) verticalLayout) (toggleGapsLayout tallLayout)
