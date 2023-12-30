import Data.Monoid (mconcat, (<>))
import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

import SharedConfig

myModMask = windowsKey

main = do
  xmproc <- spawnPipe "xmobar"

  moveSignalToCurrentWindowHook <- watchForSignalNotifications

  let baseConfig = sharedConfig xmproc

      -- Define special workspaces here
      codeWorkspace          = "1:code"
      termWorkspace          = "2:term"
      webWorkspace           = "3:web"
      healthWorkspace        = "5:health"
      entertainmentWorkspace = "6:entertainment"
      musicWorkspace         = "7:music"

      startupPrograms = [(signalApp, healthWorkspace)]

      mediaKeys = [ ((0, 0x1008FF11), spawn "amixer -q sset Master 2%-"),
                    ((0, 0x1008FF13), spawn "amixer -q sset Master 2%+"),
                    ((0, 0x1008FF12), spawn "amixer set Master toggle")
                  ]


  xmonad $ (
    baseConfig { workspaces = [codeWorkspace, termWorkspace, webWorkspace, "4", healthWorkspace, entertainmentWorkspace, musicWorkspace, "8", "9"]
               , manageHook = manageHook baseConfig
               , startupHook = startupProgramsHook startupPrograms <> startupHook baseConfig
               , handleEventHook = moveSignalToCurrentWindowHook
               , modMask = myModMask
               } `additionalKeys` ((sharedKeyMap myModMask) ++ mediaKeys)
    )
