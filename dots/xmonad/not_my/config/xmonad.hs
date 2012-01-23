--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
 
import XMonad
import System.Exit
 
import qualified XMonad.StackSet as W 
import qualified Data.Map        as M 

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CopyWindow
import XMonad.Actions.FloatKeys
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Util.Themes
import XMonad.Actions.CycleWS
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.SetWMName
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Man
import XMonad.Prompt.AppLauncher as AL


startupHook = setWMName "LG3D"

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvtc"
myncmpc      = "urxvtc -e ncmpc"

-- Width of the window border in pixels.
--
myBorderWidth   = 2
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]


-- myManageHook = composeAll
--                [ className =? "Iceweasel"    
--                , className =? "Firefox"        --> doF (W.shift "1:web" )
--                , title     =? "Save a Bookmark" --> doFloat
--                , className =? "Emacs"          --> doF (W.shift "2:emacs" )
--                , className =? "MPlayer"        --> doF (W.shift "3:media" )
--                , className =? "Rhythmbox"      --> doF (W.shift "3:media" )
--                , className =? "Stardict"       --> doF (W.shift "9:dict" )
--                , className =? "Gimp"           --> doFloat
--                , resource  =? "desktop_window" --> doIgnore
--                , className =? "Kio_uiserver"   --> doFloat
--                , resource  =? "kdesktop"       --> doIgnore]

myManageHook = composeAll
               [ className =? "Iceweasel"      --> doF (W.shift "1" )
               , className =? "Firefox"        --> doF (W.shift "1" )
               , title     =? "Save a Bookmark" --> doFloat
               -- , className =? "Stardict"       --> doF (W.shift "9:d" )
               , className =? "Gimp"           --> doFloat
               , className =? "Qt-subapplication"         --> doIgnore
               , resource  =? "desktop_window" --> doIgnore
               , resource  =? "Conky" --> doIgnore
               , className =? "stalonetray"    --> doIgnore
               , className =? "XClock"    --> doIgnore
               , className =? "Kio_uiserver"   --> doFloat
               , resource  =? "kdesktop"       --> doIgnore]



-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#252525"
myFocusedBorderColor = "#de9900"



myTabConfig = defaultTheme { decoHeight = 12
                           }


myLdefault = avoidStruts (noBorders (tabbed shrinkText myTabConfig) |||tiled ||| Mirror tiled ||| noBorders Full)
    where
      -- default tiling algorithm partitions the screen into two panes
      tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
      nmaster = 1
 
     -- Default proportion of screen occupied by master pane
      ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
      delta   = 3/100

myLvideo = avoidStruts (noBorders Full ||| tiled )
    where
      tiled   = Tall nmaster delta ratio
      nmaster = 1
      ratio   = 1/2
      delta   = 3/100

myLweb = avoidStruts (noBorders (tabbed shrinkText myTabConfig) ||| tiled )
    where
      tiled   = Tall nmaster delta ratio
      nmaster = 1
      ratio   = 1/2
      delta   = 3/100

myLemacs = avoidStruts (tiled ||| (noBorders (tabbed shrinkText myTabConfig)))
    where
      tiled   = Tall nmaster delta ratio
      nmaster = 1
      ratio   = 1/2
      delta   = 3/100
           
myLayout = onWorkspace "1:web" myLweb $  
           onWorkspace "2:emacs" myLemacs $
           onWorkspace "3:video" myLvideo $
           myLdefault                      

 
-- Default offset of drawable screen boundaries from each physical
-- screen. Anything non-zero here will leave a gap of that many pixels
-- on the given edge, on the that screen. A useful gap at top of screen
-- for a menu bar (e.g. 15)
--
-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(18,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {   font        = "-*-terminus-medium-r-normal-*-14-*-*-*-*-*-*"
                               , position    = Top }


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
--     , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu -fn '-*-terminus-bold-r-normal-*-14-*-*-*-*-*-*' -nb '#4d4d4d' -nf '#e2e2e2' -sb '#ffffff' -sf '#ff0000'` && eval \"exec $exe\"")
 

--     , ((modMask .|. shiftMask, xK_o), spawn "mpc next > /dev/null")
--     , ((modMask,               xK_u), spawn "mpc toggle > /dev/null")
    , ((modMask,               xK_p     ), shellPrompt myXPConfig)
    , ((modMask  .|. controlMask, xK_x), runOrRaisePrompt myXPConfig)
    , ((modMask , xK_F1), manPrompt myXPConfig)
    -- , ((modMask , xK_d), AL.launchApp myXPConfig "qstardict.py" )
    , ((modMask , xK_d), spawn "qdbus org.qstardict.dbus /qstardict org.freedesktop.DBus.Properties.Set org.qstardict.dbus mainWindowVisible $(( ! $(qdbus org.qstardict.dbus /qstardict org.freedesktop.DBus.Properties.Get org.qstardict.dbus mainWindowVisible)))" )
    , ((modMask ,              xK_r     ), spawn "run-raise-rtorrent.sh")
    , ((modMask ,              xK_n     ), spawn (myTerminal ++ " -e ncmpcpp"))
    , ((modMask  .|. shiftMask, xK_x),  spawn "xkill ")
    , ((modMask ,               xK_o),  toggleWS)
 
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill1)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.swapMaster)
 
    -- Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_m     ), spawn "im")
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap
--     , ((modMask              , xK_b     ),
--           modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i
--                              in if n == x then (0,0,0,0) else x))
 
    , ((modMask , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- mod-control-shift-[1..9] @@ Copy client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
     
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--






------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

    
 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
-- myLogHook = dynamicLogXmobar
myLogHook = dynamicLogWithPP  $ sjanssenPP {ppCurrent  = xmobarColor "yellow" "black" ,
--                                             ppLayout = \s -> "",
                                            ppTitle = \s -> ""}

 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad defaults
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--


defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        focusFollowsMouse  = myFocusFollowsMouse,
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
--         defaultGaps        = myDefaultGaps,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook
    }

