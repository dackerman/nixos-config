import XMonad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import Data.Monoid (mconcat, (<>))
import Data.List (tail)

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

  let startupPrograms = [(signalApp, healthWorkspace)]

  xmonad $ (
    baseConfig { workspaces = [codeWorkspace, termWorkspace, webWorkspace, "4", healthWorkspace, entertainmentWorkspace, musicWorkspace, "8", "9"]
               , manageHook = manageHook baseConfig
               , startupHook = startupProgramsHook startupPrograms <> startupHook baseConfig
               , handleEventHook = moveSignalToCurrentWindowHook
               , modMask = myModMask
               } `additionalKeys` (sharedKeyMap myModMask)
    )
