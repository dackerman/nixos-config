main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ (sharedConfig xmproc)

customModMask = altKey

customKeyMap = sharedKeyMap
