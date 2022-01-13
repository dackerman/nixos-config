module SharedConfig
  ( floatingWindows
  , makeFloating
  , floatingWindowsHook
  , altKey
  , ctrlKey
  , rightAlt
  , windowsKey
  , sharedKeyMap
  , sharedConfig
  , watchForSignalNotifications
  ) where
import XMonad (xmonad, stringProperty, className, (=?), (<&&>), (-->), (<+>), (.|.), doFloat, composeAll, workspaces,
               keys, xK_b, xK_e, xK_c, mod4Mask, mod3Mask, mod2Mask, mod1Mask, defaultConfig, startupHook, manageHook,
               controlMask, logHook, normalBorderColor,focusedBorderColor, modMask, terminal, layoutHook, spawn, io,
               Query(), WindowSet, title, appName, X, handleEventHook, reveal, Window, whenJust, runQuery, windowset)
import XMonad.Operations (sendMessage, windows)
import XMonad.StackSet (allWindows, shiftWin, currentTag, StackSet, Stack(Stack), member, modify, findTag, tagMember,
                        delete', view)
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

import Data.Text (isInfixOf, pack)
import System.Process (createProcess, proc, std_out, StdStream(CreatePipe))
import Control.Concurrent (forkIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import GHC.IO.Handle (hGetLine, hWaitForInput)
import Control.Monad (forever, filterM)
-- import System.IO
import Data.Maybe (listToMaybe)
import Data.Monoid (Endo, All(All))
--import Graphics.X11.Xlib (All)
import Graphics.X11.Xlib.Extras (getWindowAttributes, WindowAttributes, Event)
import Control.Monad.State (gets)

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

sharedKeyMap customModMask =
  [ ((customModMask, xK_b), sendMessage ToggleStruts)
  , ((ctrlKey .|. altKey, xK_e), spawn "emacs")
  , ((ctrlKey .|. altKey, xK_c), spawn "google-chrome-stable")
  ]

sharedLayouts = layoutHook def ||| ThreeColMid 1 (3/100) (1/2) ||| Grid ||| spiral (1/2)

sharedConfig xmobarProcess = docks $ def
    { manageHook = floatingWindowsHook
    , layoutHook = avoidStruts sharedLayouts
    , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmobarProcess
                    , ppTitle = xmobarColor "green" "" . shorten 200
                    }
    , startupHook = setWMName "LG3D"
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#cccccc"
    , terminal = "terminator"
    }

watchForSignalNotifications :: IO (Event -> X All)
watchForSignalNotifications = do
  (_, Just dbusStdout, _, _) <- createProcess (proc "dbus-monitor" ["interface='org.freedesktop.Notifications'"]) {std_out = CreatePipe}

  ref <- newIORef False

  threadId <- forkIO $ forever $ (
    do
      _ <- hWaitForInput dbusStdout (-1) -- wait indefinitely for output from dbus-monitor
      line <- hGetLine dbusStdout

      if isInfixOf (pack "Signal") (pack line)
        then (do
                 writeIORef ref True
                 return ()
             )
        else return ()
    )

  return (watchForSignalEventHook ref)

findSignal :: [Window] -> X (Maybe Window)
findSignal windowIds = do
  signalWindows <- filterM (\w -> runQuery (className =? "Signal") w) windowIds
  return (listToMaybe signalWindows)

watchForSignalEventHook :: IORef Bool -> Event -> X All
watchForSignalEventHook ref event = do
  shouldMoveSignal <- io $ readIORef ref
  if shouldMoveSignal
    then (do
             maybeSignalWindow <- gets (allWindows . windowset) >>= findSignal
             whenJust maybeSignalWindow (
               \signalWindow -> do
                 windows (\stackSet -> shiftWinWithoutFocus (currentTag stackSet) signalWindow stackSet)
               )
             io (writeIORef ref False)
         )
    else return ()

  return (All True)

-- See shiftWin function from XMonad.StackSet. This is just a version that doesn't also focus
-- the window after moving it. Instead, it moves the given window to the current screen as-is.
-- https://hackage.haskell.org/package/xmonad-0.17.0/docs/XMonad-StackSet.html#g:10
shiftWinWithoutFocus :: (Ord a, Eq s, Eq i) => i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWinWithoutFocus n w s =
  case findTag w s of
    Just from | n `tagMember` s && n /= from -> go from s
    _                                        -> s
  where go from = onWorkspace n (insertWithoutFocus w) . onWorkspace from (delete' w)

-- See insertUp from XMonad.StackSet. This just inserts without also adding focus.
insertWithoutFocus :: Eq a => a -> StackSet i l a s sd -> StackSet i l a s sd
insertWithoutFocus a s = if member a s then s else insert
  where insert = modify (Just $ Stack a [] []) (\(Stack t l r) -> Just $ Stack t l (a:r)) s

-- Helper function that wasn't exposed from XMonad.StackSet. It's unchanged
onWorkspace :: (Eq i, Eq s) => i -> (StackSet i l a s sd -> StackSet i l a s sd)
            -> (StackSet i l a s sd -> StackSet i l a s sd)
onWorkspace n f s = view (currentTag s) . f . view n $ s
