{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Layouts where

import Common (MyWorkspace (..), wsName)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout (Mirror (..), Tall (..))
import XMonad.Layout.Gaps (Direction2D (..), gaps)
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.Magnifier as LM (magnifierOff, magnifiercz')
import XMonad.Layout.MultiToggle as LMT (EOT (..), mkToggle, single, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (..))
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (REFLECTX (..), REFLECTY (..), reflectVert)
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol (..))
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.ManageHook
import XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

basicTallLayout :: Tall a
basicTallLayout = Tall nmaster delta ratio
  where
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

tallLayout =
  named "Tall" $
    LM.magnifierOff $ -- LM.magnifiercz' 1.5
      mkToggle (single MIRROR) $
        mkToggle (single REFLECTX) $
          mkToggle (single REFLECTY) $
            limitWindows 4 basicTallLayout

verticalLayout =
  named "Vert" $
    LM.magnifiercz' 1.5 $
      reflectVert $
        limitWindows 5 $
          Mirror $ XMonad.Layout.ThreeColumns.ThreeCol 1 (3 / 100) 0.4

toggleGapsLayout layout = toggleLayouts layout (addGaps layout)

toggleFullScreenLayout =
  mkToggle (NBFULL ?? EOT)
    . avoidStruts
    . mkToggle (single FULL)

addGaps layout =
  gaps [(L, 20), (R, 20), (U, 24), (D, 20)] $
    spacingRaw True (Border 0 0 0 0) False (Border 5 10 10 10) True layout

tallOrFull =
  smartBorders
    . toggleFullScreenLayout
    . onWorkspace (wsName Float) simplestFloat
    $ layouts
  where
    layouts =
      ifWider 1920 (ifWider 3800 (toggleGapsLayout tallLayout) verticalLayout) (toggleGapsLayout tallLayout)

centerFloatMedium = customFloating $ RationalRect (1 / 4) (1 / 4) (2 / 4) (2 / 4)

centerFloatBig = customFloating $ RationalRect (1 / 8) (1 / 8) (6 / 8) (6 / 8)

nsOpenDoc pdf = NS pdf (concat ["evince --class \"", pdf, "\" -f \"", pdf, "\""]) (className =? pdf)

namedScratchpads :: [NamedScratchpad]
namedScratchpads =
  [ NS "file-manager" "alacritty --class ns-file-manager -e ranger" (className =? "ns-file-manager") centerFloatMedium,
    NS "terminal" "alacritty --class ns-terminal -e tmux new-session -A -s scratch" (className =? "ns-terminal") centerFloatBig,
    NS "emacs" spawnEmacs (title =? "emacs") centerFloatBig,
    nsOpenDoc "~/Documents/vi-vim-tutorial.pdf" centerFloatBig,
    nsOpenDoc "~/Documents/Vim cheatsheet.pdf" centerFloatBig
  ]
  where
    spawnEmacs = "emacsclient -a -n -c --frame-parameters='(quote (name . \"ns-emacs\"))'"

namedScratchpadKeymaps c =
  mkKeymap
    c
    [ ("M-s f", namedScratchpadAction namedScratchpads "file-manager"),
      ("M-s t", namedScratchpadAction namedScratchpads "terminal"),
      ("M-s e", namedScratchpadAction namedScratchpads "emacs"),
      ("M-s 1", namedScratchpadAction namedScratchpads "~/Documents/Vim cheatsheet.pdf"),
      ("M-s 2", namedScratchpadAction namedScratchpads "~/Documents/vi-vim-tutorial.pdf")
    ]
