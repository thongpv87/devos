module Main where

import Control.Concurrent ()
import Control.Concurrent.STM ()
import Data.List ()
import Data.String ()
import Monitors ()
import Themes
  ( Palette (..),
    baseConfig,
    darkOnOrange,
    defaultHeight,
    (<~>),
    (>~<),
  )
import Xmobar
  ( Align (..),
    Border (..),
    Command (..),
    Config (..),
    Date (..),
    Monitors (..),
    Runnable (..),
    XMonadLog (..),
    XPosition (..),
    configFromArgs,
    xmobar,
  )

-- myScriptPath script = "~/.config/xmobar/bin/" <> script
myScriptPath script = "/home/thongpv87/ws/devos/users/modules/xmonad/simple/xmobar/bin/" <> script

bluetooth = Com (myScriptPath "bt-status") [] "bluetooth"

runScriptOnClick script button = concat ["<action=`", myScriptPath script, "`>", button, "</action>"]

cpuTemp p =
  MultiCoreTemp
    ( p
        <~> [ "-t",
              "<avgbar> <core0>°C",
              "-W",
              "0",
              "-f",
              "\xf2cb\xf2ca\xf2ca\xf2c9\xf2c9\xf2c8\xf2c8\xf2c7\xf2c7\xf2c7",
              "-L",
              "40",
              "-H",
              "60"
            ]
    )

battery p rate =
  BatteryN
    ["BAT0"]
    [ "-t",
      "<acstatus>",
      "-S",
      "Off",
      "-d",
      "0",
      "-m",
      "2",
      "-L",
      "10",
      "-H",
      "90",
      "-p",
      "3",
      "-W",
      "0",
      "-f",
      "\xf244\xf243\xf243\xf243\xf242\xf242\xf242\xf241\xf241\xf240",
      "--low",
      pHigh p,
      "--normal",
      pNormal p,
      "--high",
      pLow p,
      "--",
      "-P",
      "-a",
      "notify-send -u critical 'Battery running out!!!!!!'",
      "-A",
      "10",
      "-i",
      "\xf011",
      "-O",
      "<leftbar> \xfa34" ++ "<left> <timeleft>",
      "-o",
      "<leftbar>" ++ "<left> <timeleft>",
      "-H",
      "14",
      "-L",
      "10",
      "-h",
      pHigh p,
      "-l",
      pLow p,
      "-p",
      "#2ae300"
    ]
    rate
    "battery"

wireless p n =
  Wireless
    n
    ( p
        >~< [ "-t",
              "\xf1eb <ssid>",
              "-W",
              "5",
              "-M",
              "15",
              "-m",
              "3",
              "-L",
              "20",
              "-H",
              "80"
            ]
    )

-- volume p = Alsa "default" "Master"
--          [ "-t", "<volumebar><volume>", "-m", "2", "-W", "0"
--          , "-f", "\xfa7e\xfa7f\xfa7f\xfa7f\xfa7f\xfa7d\xfa7d\xfa7d\xfa7d\xfa7d"
--          ]

volume p =
  Alsa
    "default"
    "Master"
    [ "-t",
      "<status> <volume>",
      "-m",
      "2",
      "--",
      "-C",
      pForeground p,
      "-c",
      pForeground p,
      "-O",
      "",
      "-o",
      "\xfc5d",
      "-h",
      "\xfa7d",
      "-m",
      "\xfa7f",
      "-l",
      "\xfa7e"
    ]

brightness =
  Brightness
    [ "-t",
      "<bar> <percent>%",
      "-W",
      "0",
      "-f",
      "\xe3d5\xf5d9\xf5dd\xf5dd\xf5de\xf5de\xf5de\xf5de\xf5df\xf5df",
      "--",
      "-D",
      "intel_backlight"
    ]

sep = "<fc=#bd93f9>|</fc>"

config p =
  (baseConfig p)
    { position = TopW L 92,
      textOffset = defaultHeight - 6,
      textOffsets = [defaultHeight - 6],
      border = BottomB,
      commands =
        [ Run (wireless p "wlp82s0" 600),
          Run (brightness 600),
          Run (cpuTemp p 50),
          Run (volume p),
          Run (bluetooth 600),
          Run XMonadLog,
          Run (Date "%a %d %R" "datetime" 30),
          Run (battery p 600)
        ],
      template =
        unwords
          [ " ^XMonadLog^",
            "{^^}",
            sep,
            runScriptOnClick "wf-onclick" "^wlp82s0wi^",
            sep,
            runScriptOnClick "bt-onclick" "^bluetooth^",
            sep,
            runScriptOnClick "vol-onclick" "^alsa:default:Master^",
            sep,
            "^bright^",
            sep,
            "^multicoretemp^",
            sep,
            "^battery^",
            sep,
            "^datetime^",
            " <fc=#bd93f9>|</fc>"
          ]
    }

main =
  pure darkOnOrange >>= configFromArgs . config >>= xmobar
