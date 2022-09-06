module StatusBar where
import Xmobar
import XMobar.Themes
import XMobar.Monitors

-- <fn=1> 💡 </fn>
config p = (baseConfig p) {
  position = TopSize C 100 defaultHeight
  , textOffset = defaultHeight - 8
  , textOffsets = [defaultHeight - 9]
  , border = BottomB
  , commands = [ Run (topProc p)
               , Run (multiCPU p)
               , Run (cpuFreq p)
               , Run memory
               , Run (diskU p)
               , Run (diskIO p)
               , Run (coreTemp p)
               , Run brightness
               , Run (kbd p)
               , Run XMonadLog
               , Run (batt p)
               ]
  , template = " |bright| |memory| |top| <fn=1>|kbd|</fn>\
               \{|XMonadLog|}\
               \ |diskio| |disku| |cpufreq| |multicpu|\
               \  |multicoretemp| |batt0| "
}

topBar =
  palette >>= configFromArgs . config >>= xmobar
