import XMonad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import Data.Monoid (mconcat, (<>))

import SharedConfig

myModMask = windowsKey

main = do
  xmproc <- spawnPipe "xmobar"

  moveSignalToCurrentWindowHook <- watchForSignalNotifications

  let baseConfig = sharedConfig xmproc

      startupPrograms = [signalApp, emacsApp, chromeApp]

      mediaKeys = [ ((0, 0x1008FF11), spawn "amixer -q sset Master 2%-"),
                    ((0, 0x1008FF13), spawn "amixer -q sset Master 2%+"),
                    ((0, 0x1008FF12), spawn "amixer set Master toggle")
                  ]


  xmonad $ (
    baseConfig { workspaces = ["1:code", "2:term", "3:web", "4", "5", "6:entertainment", "7:music", "8", "9"]
               , manageHook = manageHook baseConfig
               , startupHook = startupProgramsHook startupPrograms <> startupHook baseConfig
               , handleEventHook = moveSignalToCurrentWindowHook
               , modMask = myModMask
               } `additionalKeys` ((sharedKeyMap myModMask) ++ mediaKeys)
    )
