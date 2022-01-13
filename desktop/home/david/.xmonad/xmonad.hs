import XMonad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)

import SharedConfig

myModMask = windowsKey

main = do
  xmproc <- spawnPipe "xmobar"

  moveSignalToCurrentWindowHook <- watchForSignalNotifications

  let baseConfig = sharedConfig xmproc

  xmonad $ (
    baseConfig { workspaces = ["1:code", "2:term", "3:web", "4", "5", "6:entertainment", "7:music", "8", "9"]
               , manageHook = manageHook baseConfig
               , handleEventHook = moveSignalToCurrentWindowHook
               , modMask = myModMask
               } `additionalKeys` (sharedKeyMap myModMask)
    )
