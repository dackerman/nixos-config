import XMonad (xmonad, stringProperty, className, (=?), (<&&>), (-->), (<+>), (.|.), doFloat, composeAll,
               keys, xK_b, xK_e, xK_c, mod4Mask, mod3Mask, mod2Mask, mod1Mask, defaultConfig, startupHook, manageHook,
               controlMask, logHook, normalBorderColor,focusedBorderColor, modMask, terminal, layoutHook, spawn)
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
gtkAppId = stringProperty "_GTK_APPLICATION_ID"

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
                  , gtkAppId =? "org.gnome.Nautilus"
                  ]

makeFloating w = w --> doFloat

floatingWindowsHook = composeAll $ (map makeFloating floatingWindows)

altKey = mod1Mask
ctrlKey = controlMask
rightAlt = mod3Mask
windowsKey = mod4Mask

sharedKeyMap =
  [ ((customModMask, xK_b), sendMessage ToggleStruts)
  , ((ctrlKey .|. altKey, xK_e), spawn "emacs")
  , ((ctrlKey .|. altKey, xK_c), spawn "google-chrome-stable")
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
