import XMonad (xmonad, stringProperty, className, (=?), (<&&>), (-->), (<+>), doFloat, composeAll,
               mod4Mask, mod3Mask, mod2Mask, mod1Mask, defaultConfig, manageHook, layoutHook, logHook, normalBorderColor,
               focusedBorderColor, modMask, terminal)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, xmobarColor, shorten, ppOutput, ppTitle)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import System.IO (hPutStrLn)

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
                  , className =? "Chromium" <&&> windowRole =? "pop-up"
                  ]

makeFloating w = w --> doFloat

floatingWindowsHook = composeAll $ (map makeFloating floatingWindows)

altKey = mod1Mask
rightAlt = mod3Mask
windowsKey = mod4Mask

main = do
  xmproc <- spawnPipe "/run/current-system/sw/bin/xmobar /home/david/.xmobarrc"
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> floatingWindowsHook
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                }
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#cccccc"
    , modMask = windowsKey
    , terminal = "terminator"
    }
