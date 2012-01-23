import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import System.IO
import XMonad.Util.Run

-- myManageHook = composeAll (
--    [ manageHook gnomeConfig
--    , className =? "Unity-2d-panel" --> doIgnore
--    , className =? "Unity-2d-launcher" --> doFloat
--    , isFullscreen --> doFullFloat
--    ])

myManageHook = composeAll (
    [ manageHook defaultConfig
    , className =? "stalonetray"    --> doIgnore
    , isFullscreen --> doFullFloat
    ])


myModMask       = mod4Mask

main = do
    xmproc <- spawnPipe "xmobar"
    -- xmproc <- spawnPipe "gnome-settings-manager"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook
         --{ manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = myModMask
        } `additionalKeysP`
                  [ ("M-p",  spawn "gmrun")
                  ]



