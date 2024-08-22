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

      workspaces =
        [ "1:code"
        , "2:term"
        , "3:web"
        , "4"
        , "5:health"
        , "6:entertainment"
        , "7:music"
        , "8"
        , "9"
        ]

      workspace n
        | n > 0 && n <= length workspaces = workspaces !! (n - 1)
        | otherwise = show n

  let startupPrograms = [(signalApp, workspace 5)]

  xmonad $ (
    baseConfig
      { workspaces = workspaces
      , manageHook = manageHook baseConfig
      , startupHook = startupProgramsHook startupPrograms <> startupHook baseConfig
      , handleEventHook = moveSignalToCurrentWindowHook
      , modMask = myModMask
      } `additionalKeys` (sharedKeyMap myModMask)
    )
