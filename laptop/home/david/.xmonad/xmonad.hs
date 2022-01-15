import XMonad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import Data.Monoid (mconcat, (<>))

import SharedConfig

myModMask = altKey

main = do
  xmproc <- spawnPipe "xmobar"

  moveSignalToCurrentWindowHook <- watchForSignalNotifications

  let baseConfig = sharedConfig xmproc

  let startupPrograms =
        [ "signal-desktop"
        , "emacs"
        , "google-chrome-stable"
        ]

  xmonad $ (
    baseConfig { workspaces = ["1:code", "2:term", "3:web", "4", "5", "6:entertainment", "7:music", "8", "9"]
               , manageHook = manageHook baseConfig
               , startupHook = mconcat (spawn <$> startupPrograms) <> startupHook baseConfig
               , handleEventHook = moveSignalToCurrentWindowHook
               , modMask = myModMask
               } `additionalKeys` (sharedKeyMap myModMask)
    )
