import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doFloat
    , isFullscreen --> doFullFloat
    ])



myModMask       = mod4Mask

main = xmonad $ gnomeConfig
        { manageHook = myManageHook,
          modMask    = myModMask
        }
        `additionalKeysP`
                  [ ("M-p",  spawn "gmrun")
                  ]



