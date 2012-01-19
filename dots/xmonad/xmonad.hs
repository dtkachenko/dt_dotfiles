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


main = xmonad $ gnomeConfig
        { manageHook = myManageHook
        }
        `additionalKeysP`
                  [ ("M-p",  spawn "gmrun")
                  ]



