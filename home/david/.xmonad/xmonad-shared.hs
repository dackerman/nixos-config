import XMonad (xmonad, stringProperty, className, (=?), (<&&>), (-->), (<+>), doFloat, composeAll,
               keys, xK_b, mod4Mask, mod3Mask, mod2Mask, mod1Mask, defaultConfig, startupHook, manageHook,
               logHook, normalBorderColor,focusedBorderColor, modMask, terminal, layoutHook)
import XMonad.Operations (sendMessage)
import XMonad.Hooks.SetWMName (setWMName)              
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobar, xmobarPP, xmobarColor, shorten, ppOutput, ppTitle)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docks, ToggleStruts(ToggleStruts))
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import System.IO (hPutStrLn)
import Data.Default (def)
import Data.Map (fromList)

windowRole = stringProperty "WM_WINDOW_ROLE"

------------------------------------------------------------------------
-- Window rules

-- | Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
--  xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
floatingWindows = [ className =? "MPlayer"
                  , className =? "Gimp"
                  , className =? "Galculator"
                  , windowRole =? "pop-up"
                  , windowRole =? "gnome-calculator"
                  , className =? "Signal"
                  ]

makeFloating w = w --> doFloat

floatingWindowsHook = composeAll $ (map makeFloating floatingWindows)

altKey = mod1Mask
rightAlt = mod3Mask
windowsKey = mod4Mask

sharedKeyMap =
  [ ((customModMask, xK_b), sendMessage ToggleStruts)
  ]

sharedConfig xmobarProcess = docks $ def
    { manageHook = floatingWindowsHook
    , layoutHook = avoidStruts $ layoutHook def
    , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmobarProcess
                    , ppTitle = xmobarColor "green" "" . shorten 200
                    }
    , startupHook = setWMName "LG3D"
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#cccccc"
    , modMask = customModMask
    , terminal = "terminator"
    } `additionalKeys` customKeyMap
