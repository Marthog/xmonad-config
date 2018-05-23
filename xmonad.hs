--
-- XMonad
--
-- Designed to work well with a neo2 keyboard layout and with two monitor setup.
--
-- Most of the keys are remapped for easy typing. Some programs have hotkelys.
 
import XMonad
import Data.Monoid
import System.Exit(exitSuccess)
import System.IO(Handle)
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Hooks.ManageDocks(avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers(isDialog, isFullscreen, doFullFloat)
import XMonad.Layout.NoBorders(lessBorders,Ambiguity(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(unsafeSpawn, safeSpawn)

import XMonad.Layout((|||), Full, Tall)
import XMonad.Layout.ThreeColumns(ThreeCol(..))

import XMonad.Actions.UpdatePointer(updatePointer)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 


--setBgMon1 = "xsetbg -at 0,0 .xmonad/carina_2560x1440.png"
--setBgMon2 = "xsetbg -at 0,0 .xmonad/carina_2560x1440.png -at 1920,0 .xmonad/carina_2560x1440.png"
setBgMon1 = ""
setBgMon2 = ""

xrandrMon1 = "xrandr --output DisplayPort-0 --auto --primary --output DVI-1 --off"
xrandrMon2 = "xrandr --output DisplayPort-0 --auto --primary --output DVI-1 --auto --left-of DisplayPort-0" 

data StartupInfo = StartupInfo {
}


monitorDual, monitorSingle :: MonadIO m => m ()
monitorDual = do
    unsafeSpawn $ concat [xrandrMon2] -- , " && ", setBgMon2]

monitorSingle = do
    unsafeSpawn $ concat [xrandrMon1] -- , " && ", setBgMon1]


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal      = "lxterminal"
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
-- Width of the window border in pixels.
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask       = mod4Mask      -- key for all xmonad command
extensionMask   = shiftMask      -- key for controlling xmonad (default=shift)
quickstartMask  = controlMask     -- key for quickstarting programs
extraKeyMask    = mod1Mask       -- key for choosing keyboard settings


extMask         = extensionMask

-- Increase the volume in steps of 5%.
volumeStep      = show 5

volumeMod inc   = amixerSet [
            "Master"
            , volumeStep++"%"++if inc then "+" else "-"
        ]

-- Use amixer to set a volume.
amixerSet x = safeSpawn "amixer" ("set":x)


toggleXMobarKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = show `map` [1..9]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "black"
myFocusedBorderColor = "red"


-- Spawn dmenu to select a program.
dmenuSpawn = unsafeSpawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\""

-- Run dmenu on a list of selected programs.
programListRun = unsafeSpawn "exe=`cat .xmonad/program_list | sort | dmenu` && eval \"exec $exe\""

-- Spawn a new terminal.
terminalSpawn = unsafeSpawn . XMonad.terminal


-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@XConfig {modMask = modm} = M.fromList $
 
    -- launch a terminal
    [ ((modm .|. extMask, xK_Return), terminalSpawn conf)

    -- launch dmenu
    , ((modm,               xK_d     ), programListRun)

    -- launch gmrun
    -- removed unsafeSpawn gmrun
    , ((modm .|. extMask, xK_d     ), dmenuSpawn)
 
    -- close focused window
    , ((modm .|. extMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. extMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_j     ), refresh)
 
    -- Move focus to the next window
    --, ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_r     ), windows W.focusDown)
    , ((modm,               xK_e     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_n     ), windows W.focusUp  )
    , ((modm,               xK_a     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm .|. extMask, xK_m), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. extMask, xK_e     ), windows W.swapDown  )
    , ((modm .|. extMask, xK_r     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. extMask, xK_a     ), windows W.swapUp    )
    , ((modm .|. extMask, xK_n     ), windows W.swapUp    )
 
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
--    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
--    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Quit xmonad
    , ((modm .|. extMask  , xK_q     ), spawn "shutdown now")

    -- Quit xmonad
    , ((modm .|. extMask .|. quickstartMask  , xK_q     ), spawn "shutdown now")
 
 
    -- Restart xmonad
    , ((modm              , xK_q     ), unsafeSpawn "xmonad --recompile && xmonad --restart")


    , ((modm             , xK_Up ), volumeMod True)
    , ((modm             , xK_Down ), volumeMod False)
    , ((modm             , xK_Tab ), amixerSet ["Master", "toggle"])


    , ((modm .|. extraKeyMask  , xK_0     ), changeKeyboardMap "us")
    , ((modm .|. extraKeyMask  , xK_8     ), changeKeyboardMap "de")
--    , ((modm .|. extraKeyMask  , xK_9     ), changeKeyboardMap "dvorak")
    , ((modm .|. extraKeyMask  , xK_9     ), enableNeo)

    , ((modm .|. extraKeyMask  , xK_2     ), monitorDual)
    , ((modm .|. extraKeyMask  , xK_1     ), monitorSingle)
    ]


    -- special programs
    ++

    [ ((modm .|. quickstartMask  , k), unsafeSpawn p) | (k, p) <- programList]

    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    --[((m .|. modm, k), windows $ f i)
    --    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{v,x}, Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{v,x}, Move client to screen 1 or 2
    --
     [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_v, xK_x] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]] 

-- List of hotkeys and programs. The keys are connected with 
programList = 
    [ (xK_v,    "vlc")
    , (xK_f,    "firefox")
    , (xK_c,    "chromium")
    , (xK_g,    "geany")
    , (xK_space, "pcmanfm-qt")
    , (xK_h,    "thunderbird")
    , (xK_t,    "tor-browser-en")
    , (xK_Return, myTerminal)
    , (xK_s,    "~/steam")
    ]

-- Enable the neo layout. Disable numlock first.
-- Also use xset -r 51 although I forgot, why.
enableNeo :: MonadIO m => m ()
enableNeo = unsafeSpawn "numlockx off; setxkbmap de neo; xset -r 51"

-- Change the keyboard map.
changeKeyboardMap x = safeSpawn "setxkbmap" [x]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), move mouseMoveWindow)
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), move return)
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), move mouseResizeWindow)
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
    where move f w = focus w >> f w >> windows W.shiftMaster
 
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
myLayout info = lessBorders Screen . avoidStruts $ layout
    where
        -- layout = layoutHook defaultConfig
        layout = tiled ||| Full ||| three
        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall 1 delta ratio

       -- Default proportion of screen occupied by master pane
        ratio   = 55/100

       -- Percent of screen to increment by when resizing panes
        delta   = 5/100       

        three   = ThreeCol 1 delta (1/3)


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
myManageHook info = composeAll [ 
    bindToScreen "Thunderbird" 6
    , bindToScreen "Vlc" 2
    , bindToScreen "Steam" 7
    , className =? "Xmessage" --> doFloat
    , isDialog --> doFloat
    , isFullscreen --> doFullFloat
    ]
    where
        bindToScreen win screen = className =? win --> doF (W.shift (show screen))


------------------------------------------------------------------------
-- Event handling
 
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = handleEventHook def <> fullscreenEventHook 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
textColor color = xmobarColor color ""

myPP = xmobarPP
               { 
               ppTitle  = textColor "lightblue" . shorten 100
               , ppSep    = replicate 5 ' '
               , ppWsSep  = " | "
               , ppCurrent = textColor "orange"
               , ppUrgent = textColor "red"
--               , ppHiddenNoWindows = textColor "#707070"
               , ppOrder = \(x:xs) -> reverse xs++[wrap "[ " " ]" x]
               , ppExtras = ppExtras xmobarPP
               } 

myLogHook info = updatePointer (0.5, 0.5) (0.5, 0.5) <* dynamicLog 

 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
myStartupHook = monitorSingle

 -- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
main = do
    -- start single monitor

    xmobar <- statusBar "xmobar ~/.xmonad/xmobarrc" myPP toggleStrutsKey (defaults StartupInfo)
    xmonad xmobar
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults info = ewmh def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout info,
        manageHook         = manageDocks <> myManageHook info <> manageHook def
        , handleEventHook    = myEventHook,
        logHook            = myLogHook info,
        startupHook        = myStartupHook
    }
