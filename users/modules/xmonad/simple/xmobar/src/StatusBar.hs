module Main where
import Xmobar
import Themes
import Monitors
import Xmobar
import Themes
import Control.Concurrent
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import qualified Data.Char as Char
import qualified Text.Printf as Printf
import Data.String
import Data.List


-- myScriptPath script = "~/.config/xmobar/bin/" <> script
myScriptPath script = "/home/thongpv87/ws/devos/users/modules/xmonad/simple/xmobar/bin/" <> script

bluetooth rate = Com (myScriptPath "bt-status") [] "bluetooth" rate

runScriptOnClick script button = concat ["<action=`", myScriptPath script, "`>", button,"</action>"]

cpuTemp p rate = MultiCoreTemp (p <~> ["-t", "<avgbar> <core0>°C" , "-W", "0"
                                 , "-f", "\xf2cb\xf2ca\xf2ca\xf2c9\xf2c9\xf2c8\xf2c8\xf2c7\xf2c7\xf2c7"
                                 , "-L", "40", "-H", "60" ]) rate

battery p rate =
  BatteryN ["BAT0"]
           ["-t", "<acstatus>"
           , "-S", "Off", "-d", "0", "-m", "2"
           , "-L", "10", "-H", "90", "-p", "3"
           , "-W", "0", "-f"
           , "\xf244\xf243\xf243\xf243\xf242\xf242\xf242\xf241\xf241\xf240"
           , "--low", pHigh p, "--normal", pNormal p, "--high", pLow p
           , "--"
           , "-P"
           , "-a", "notify-send -u critical 'Battery running out!!!!!!'"
           , "-A", "10"
           , "-i", fni "\xf011"
           , "-O", fni "<leftbar>\xf0e7" ++ "<left> <timeleft>"
           , "-o", fni "<leftbar>" ++ "<left> <timeleft>"
           , "-H", "14", "-L", "10"
           , "-h", pHigh p, "-l", pLow p] rate "battery"

wireless p n rate = Wireless n (p >~< ["-t", "\xf1eb <ssid>"
                                 , "-W", "5", "-M", "15" , "-m", "3"
                                 , "-L", "20", "-H", "80"]) rate

-- volume p = Alsa "default" "Master"
--          [ "-t", "<volumebar><volume>", "-m", "2", "-W", "0"
--          , "-f", "\xfa7e\xfa7f\xfa7f\xfa7f\xfa7f\xfa7d\xfa7d\xfa7d\xfa7d\xfa7d"
--          ]

volume p = Alsa "default" "Master"
         [ "-t", "<status> <volume>", "-m", "2"
         , "--", "-C", pForeground p, "-c", pForeground p
         , "-O", "", "-o", "\xfc5d", "-h", "\xfa7d"
         , "-m", "\xfa7f", "-l", "\xfa7e"
         ]

brightness rate = Brightness
    [ "-t", "<bar> <percent>%", "-W", "0"
    , "-f", "\xe3d5\xf5d9\xf5dd\xf5dd\xf5de\xf5de\xf5de\xf5de\xf5df\xf5df"
    , "--", "-D", "intel_backlight"
    ] rate

config p = (baseConfig p) {
  position = TopSize C 100 defaultHeight
  , textOffset = defaultHeight - 8
  , textOffsets = [defaultHeight - 9]
  , border = BottomB
  , commands = [ Run (wireless p "wlp82s0" 600)
               , Run (brightness 100)
               , Run (cpuTemp p 50)
               , Run (volume p)
               , Run (bluetooth 100)
               , Run XMonadLog
               , Run (Date "%a %d %R" "datetime" 30)
               , Run (battery p 600)
               ]
  , template = unwords
               [ " |XMonadLog|"
               , "{||}"
               , runScriptOnClick "wf-onclick" "|wlp82s0wi| "
               , runScriptOnClick "bt-onclick" "|bluetooth| "
               , runScriptOnClick "vol-onclick" "|alsa:default:Master| "
               , "|bright| "
               , "|multicoretemp| |battery| |datetime|"
               ]
}

main =
  pure darkOnOrange >>= configFromArgs . config >>= xmobar
