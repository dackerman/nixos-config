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
  , startupProgramsHook
  , spawnIfNotRunning
  , signalApp
  , emacsApp
  , chromeApp
  , terminalApp
  , weatherApp
  ) where
import XMonad hiding (modify)
import XMonad.Operations (sendMessage, windows)
import XMonad.StackSet (allWindows, currentTag, StackSet, Stack(Stack), member, findTag, tagMember, modify,
                        delete', view, insertUp)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobar, xmobarPP, xmobarColor, shorten, ppOutput, ppTitle)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docks, ToggleStruts(ToggleStruts))
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
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
import Control.Monad (forever, filterM, void)
-- import System.IO
import Data.Maybe (listToMaybe)
import Data.Monoid (Endo, All(All))
--import Graphics.X11.Xlib (All)
import Graphics.X11.Xlib.Extras (getWindowAttributes, WindowAttributes, Event)
import Control.Monad.State (gets, MonadIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)

windowRole = stringProperty "WM_WINDOW_ROLE"
gtkAppId = stringProperty "_GTK_APPLICATION_ID"

debugLog msg = do
  currentTime <- getCurrentTime
  let logline = (formatShow iso8601Format currentTime) ++ ": " ++ msg ++ "\n"
  appendFile "/home/david/.xmonad/log.txt" logline

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
                  , className =? "gnome-calculator"
                  , windowRole =? "gnome-calculator"
                  , className =? "Signal"
                  , gtkAppId =? "org.gnome.Nautilus"
                  , className =? "Xmessage"
                  , appQuery weatherApp
                  ]

makeFloating w = w --> doFloat

floatingWindowsHook = composeAll $ (map makeFloating floatingWindows)

altKey = mod1Mask
ctrlKey = controlMask
rightAlt = mod3Mask
windowsKey = mod4Mask

data Application = Application
  { appQuery :: Query Bool
  , spawnProgram :: String
  }

signalApp = Application (className =? "Signal") "signal-desktop"
emacsApp = Application (className =? "Emacs") "emacsclient -c"
terminalApp = Application (className =? "kitty") "kitty"
chromeApp = Application (className =? "Google-chrome") "chrome"
weatherApp = Application (className =? "Org.gnome.Weather") "gnome-weather"
calculatorApp = Application (className =? "gnome-calculator") "gnome-calculator"

sharedKeyMap customModMask =
  [ ((customModMask, xK_b), sendMessage ToggleStruts)
  , ((customModMask .|. shiftMask, xK_v), spawn "killall -9 '.vlc-wrapped'")
  , ((customModMask .|. shiftMask, xK_s), spawn "/home/david/bin/shot")
  , ((ctrlKey .|. altKey, xK_e), spawnApp emacsApp)
  , ((ctrlKey .|. altKey, xK_c), spawnApp calculatorApp)
  , ((ctrlKey .|. altKey, xK_s), focusOnCurrentWorkspace signalApp)
  , ((ctrlKey .|. altKey, xK_w), focusOnCurrentWorkspace weatherApp)
  ]

spawnApp :: Application -> X ()
spawnApp (Application _ program) = spawn program

focusOnCurrentWorkspace (Application query spawnProgram) = do
  maybeWindow <- findWindow query
  maybe (spawn spawnProgram) (selectWindow insertAndFocus) maybeWindow

sharedLayouts = layoutHook def ||| ThreeColMid 1 (3/100) (1/2) ||| Grid ||| spiral (1/2)

sharedConfig xmobarProcess = docks $ def
    { manageHook = manageSpawn <> floatingWindowsHook
    , layoutHook = avoidStruts sharedLayouts
    , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmobarProcess
                    , ppTitle = xmobarColor "green" "" . shorten 200
                    }
    , startupHook = setWMName "LG3D"
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#cccccc"
    , terminal = "kitty"
    }

startupProgramsHook :: [(Application, String)] -> X ()
startupProgramsHook programs =
  mconcat (spawnIfNotRunning <$> programs)

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

  return $ updatedRefEventHook ref (moveWindowToCurrentWorkspace (className =? "Signal"))


spawnIfNotRunning :: (Application, String) -> X ()
spawnIfNotRunning ((Application query spawnProgram), workspace) = do
  maybeWindow <- findWindow query

  if maybeWindow == Nothing
    then spawnOn workspace spawnProgram
    else return ()

moveWindowToCurrentWorkspace :: Query Bool -> X ()
moveWindowToCurrentWorkspace query = do
  maybeWindow <- findWindow query
  whenJust maybeWindow (selectWindow insertWithoutFocus)

findWindow :: Query Bool -> X (Maybe Window)
findWindow query = do
  windowIds <- gets (allWindows . windowset)
  signalWindows <- filterM (\w -> runQuery query w) windowIds
  return (listToMaybe signalWindows)

findWindowByClassName :: String -> X (Maybe Window)
findWindowByClassName theClassName = findWindow (className =? theClassName)

updatedRefEventHook :: IORef Bool -> X a -> (Event -> X All)
updatedRefEventHook ref action event = do
  shouldTrigger <- io $ readIORef ref
  if shouldTrigger
    then action >> io (writeIORef ref False)
    else return ()
  return (All True)

selectWindow :: (Window -> WindowSet -> WindowSet) -> Window -> X ()
selectWindow inserter window = windows doSelect
  where doSelect stackSet = shiftWin inserter (currentTag stackSet) window stackSet

-- See shiftWin function from XMonad.StackSet. This is just a version that doesn't also focus
-- the window after moving it. Instead, it moves the given window to the current screen as-is.
-- https://hackage.haskell.org/package/xmonad-0.17.0/docs/XMonad-StackSet.html#g:10
shiftWin :: (Ord a, Eq s, Eq i) => (a -> StackSet i l a s sd -> StackSet i l a s sd) -> i -> a -> StackSet i l a s sd -> StackSet i l a s sd
shiftWin inserter n w s =
  case findTag w s of
    Just from | n `tagMember` s && n /= from -> go from s
    _                                        -> s
  where go from = onWorkspace n (inserter w) . onWorkspace from (delete' w)


-- See insertUp from XMonad.StackSet. This just inserts without also adding focus.
insertWithoutFocus :: Eq a => a -> StackSet i l a s sd -> StackSet i l a s sd
insertWithoutFocus a s = if member a s then s else insert
  where insert = modify (Just $ Stack a [] []) (\(Stack t l r) -> Just $ Stack t l (a:r)) s

insertAndFocus :: Eq a => a -> StackSet i l a s sd -> StackSet i l a s sd
insertAndFocus = insertUp

-- Helper function that wasn't exposed from XMonad.StackSet. It's unchanged
onWorkspace :: (Eq i, Eq s) => i -> (StackSet i l a s sd -> StackSet i l a s sd)
            -> (StackSet i l a s sd -> StackSet i l a s sd)
onWorkspace n f s = view (currentTag s) . f . view n $ s
