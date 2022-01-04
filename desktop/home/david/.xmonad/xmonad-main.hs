main = xmonad =<< xmobar myConfig

myConfig = sharedConfig
    { modMask = windowsKey
    }
