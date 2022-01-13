import XMonad (xmonad, stringProperty, className, (=?), (<&&>), (-->), (<+>), (.|.), doFloat, composeAll, workspaces,
               keys, xK_b, xK_e, xK_c, mod4Mask, mod3Mask, mod2Mask, mod1Mask, defaultConfig, startupHook, manageHook,
               controlMask, logHook, normalBorderColor,focusedBorderColor, modMask, terminal, layoutHook, spawn)
import XMonad.Operations (sendMessage)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobar, xmobarPP, xmobarColor, shorten, ppOutput, ppTitle)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docks, ToggleStruts(ToggleStruts))
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeCol, ThreeColMid))
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout ((|||))
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import System.IO (hPutStrLn)
import Data.Default (def)
import Data.Map (fromList)

import SharedConfig

myModMask = windowsKey

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ (sharedConfig xmproc)
    { workspaces = ["1:code", "2:term", "3:web", "4", "5", "6:entertainment", "7:music", "8", "9"]
    , modMask = myModMask
    } `additionalKeys` (sharedKeyMap myModMask)
