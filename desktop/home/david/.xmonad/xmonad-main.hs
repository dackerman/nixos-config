main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ (sharedConfig xmproc)

customModMask = windowsKey

customKeyMap = sharedKeyMap
  
