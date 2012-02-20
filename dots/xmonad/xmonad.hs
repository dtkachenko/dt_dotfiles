import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import System.IO
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import XMonad.Hooks.SetWMName

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
    , className =? "Skype" --> doF (W.shift "3:im")
    , className =? "Firefox-bin" --> doF (W.shift "2:web")
    , className =? "Firefox" --> doF (W.shift "2:web")
    ])

myworkspaces = ["1:main", "2:web", "3:im", "4:term", "5:media", "6", "7", "8", "9" ]

myModMask       = mod4Mask

main = do
    xmproc <- spawnPipe "xmobar"
    -- xmproc <- spawnPipe "gnome-settings-manager"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook
         --{ manageHook = manageDocks <+> manageHook defaultConfig
        -- , normalBorderColor = "#D3D7CF"
        -- , focusedBorderColor = "#729FCF"
        -- , focusedBorderColor = "#fecb65"
        , terminal = "gnome-terminal"
        , startupHook = setWMName "LG3D"
        , borderWidth = 3
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , workspaces = myworkspaces
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = myModMask
        } `additionalKeysP`
                  [ ("M-p",  spawn "gmrun")
                  , ("M-C-k", spawn "/usr/bin/skype-single-instance")
                  ]



