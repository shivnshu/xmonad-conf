-- Imports.
import qualified Data.Map    as M             -- Unordered Map implementation in haskell
import Data.Ratio                             -- Standard functions on rational numbers

import XMonad
import XMonad.Actions.CopyWindow              -- Bindings to duplicate a window on multiple workspaces
import XMonad.Hooks.DynamicLog                -- System status information
import XMonad.Hooks.ManageDocks               -- Manage dock type programs such as gnome-panel, kicker, dzen
import XMonad.Hooks.ManageHelpers             -- Helper functions to be used in manageHook
import XMonad.Actions.CycleWS                 -- Move windows between workspaces, cycle between screens

import XMonad.Layout.Combo                    -- Combines multiple layouts
import XMonad.Layout.Grid                     -- Put all windows in a square grid
import XMonad.Layout.Gaps                     -- Manually create gaps along edges of screen
import XMonad.Layout.LayoutModifier           -- Helps in modifying the behavior of other layouts
import XMonad.Layout.Named                    -- Assign name to a layout
import XMonad.Layout.NoBorders                -- Useful for full screen
import XMonad.Layout.PerWorkspace             -- Use layouts and apply layout modifiers selectively
import XMonad.Layout.Reflect                  -- Reflect a layout horizontally or vertically
import XMonad.Layout.TwoPane                  -- Layout splits the screen horizontally into 2, left is master
import XMonad.Layout.WindowNavigation         -- easy navigation of a workspace
import XMonad.Layout.Circle                   -- An elliptical, overlapping layout
import XMonad.Layout.MosaicAlt                -- Gives each window a specified amount of screen space relative to the others
import XMonad.Layout.Spiral                   -- Spiral tiling layout
import XMonad.Layout.SimpleFloat              -- Basic floating layout

import DBus.Client                            -- Abstraction over the lower-level messaging system
import System.Taffybar.XMonadLog ( dbusLogWithPP, taffybarDefaultPP, taffybarColor, taffybarEscape )
import qualified Debug.Trace as D             -- Debugging
import qualified XMonad.StackSet as W         -- Window manager abstraction

import XMonad.Hooks.EwmhDesktops (ewmh)       -- Allows the user to interact with xmonad by clicking
import System.Taffybar.Hooks.PagerHints (pagerHints) -- Complements the XMonad.Hooks.EwmhDesktops with two additional hints

import System.IO
import System.Exit

--  Variables definition
myTerminal = "urxvt"
myModMask  = mod4Mask
altMask    = mod1Mask

myWorkSpaces = ["1:Web", "2:Terminal", "3:Emacs", "4:Files", "5:VMs", "6:Chat", "7:Music", "8:Misc", "9:Misc"]

-- Key mapping {{{
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_d        ), spawn "dmenu_run -fn 'terminus-16'")
    , ((modMask .|. shiftMask,      xK_Return   ), spawn $ XMonad.terminal conf)
--    , ((modMask,                    xK_F2       ), spawn "gmrun")
    , ((modMask .|. shiftMask,      xK_c        ), kill)
    , ((modMask .|. shiftMask, xK_l ), spawn "slock")
    -- Programs
    , ((0,                          xK_Print    ), spawn "scrot -e 'mv $f ~/screenshots/'")
    , ((modMask,                    xK_m        ), spawn "nautilus --no-desktop --browser")
    -- Media Keys
    , ((0,                          0x1008ff12  ), spawn "pulseaudio-ctl mute")        -- XF86AudioMute
    , ((0,                          0x1008ff11  ), spawn "pulseaudio-ctl down 10")   -- XF86AudioLowerVolume
    , ((0,                          0x1008ff13  ), spawn "pulseaudio-ctl up 10")   -- XF86AudioRaiseVolume
    , ((0,                          0x1008FF02  ), spawn "xbacklight -inc 10")
    , ((0,                          0x1008FF03  ), spawn "xbacklight -dec 10")
    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf)          -- reset layout on current desktop to default
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts)                    -- Fullscreen current window
    , ((modMask,                    xK_n        ), refresh)
    , ((modMask,                    xK_Tab      ), windows W.focusDown)                         -- move focus to next window
    , ((modMask .|. shiftMask,      xK_Tab      ), windows W.focusUp  )
    , ((modMask .|. shiftMask,      xK_Down     ), windows W.swapDown)                          -- swap the focused window with the next window
    , ((modMask .|. shiftMask,      xK_Up       ), windows W.swapUp)                            -- swap the focused window with the previous window
    , ((modMask,                    xK_Return   ), windows W.swapMaster)
    , ((modMask,                    xK_t        ), withFocused $ windows . W.sink)              -- Push window back into tiling
    , ((modMask,                    xK_h        ), sendMessage Shrink)                          -- %! Shrink a master area
    , ((modMask,                    xK_l        ), sendMessage Expand)                          -- %! Expand a master area
    , ((modMask,                    xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                    xK_period   ), sendMessage (IncMasterN (-1)))


    -- workspaces
    , ((modMask .|. controlMask,   xK_Right     ), nextWS)
    , ((modMask .|. shiftMask,     xK_Right     ), shiftToNext)
    , ((modMask .|. controlMask,   xK_Left      ), prevWS)
    , ((modMask .|. shiftMask,     xK_Left      ), shiftToPrev)
    
    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask,                    xK_q        ), spawn "stack exec xmonad -- --recompile && stack exec xmonad -- --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
--    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
--        | (key, sc) <- zip [xK_w, xK_e] [1, 0]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

--}}}

-- The main function.
main = do
  client <- connectSession
  let pp = myTaffyBarPP
  xmonad $ ewmh $ pagerHints $ myConfig { logHook = dbusLogWithPP client pp }

-- Custom PP.
myTaffyBarPP = taffybarDefaultPP {
    ppCurrent = taffybarColor "#f8f8f8" "DodgerBlue4"   . wrap " " " "
  , ppVisible = taffybarColor "#f8f8f8" "LightSkyBlue4" . wrap " " " "
  , ppUrgent  = taffybarColor "#f8f8f8" "red4"          . wrap " " " "
  , ppLayout  = taffybarColor "DarkOrange" "" . wrap " [" "] "
  , ppTitle   = taffybarColor "#61ce3c" "" . shorten 50
  }

-- Keybinding to toggle the gap for the bar.
--toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


-- Layouts
basicLayout = Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
tallLayout       = named "tall"     $ avoidStruts $ basicLayout
wideLayout       = named "wide"     $ avoidStruts $ Mirror basicLayout
singleLayout     = named "single"   $ avoidStruts $ noBorders Full
circleLayout     = named "circle"   $ Circle
twoPaneLayout    = named "two pane" $ TwoPane (2/100) (1/2)
mosaicLayout     = named "mosaic"   $ MosaicAlt M.empty
gridLayout       = named "grid"     $ Grid
spiralLayout     = named "spiral"   $ spiral (1 % 1)

myLayoutHook = tallLayout ||| wideLayout   ||| singleLayout ||| circleLayout ||| twoPaneLayout
                                         ||| mosaicLayout ||| gridLayout   ||| spiralLayout

myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ [resource     =?  r           --> doIgnore                | r <-  myIgnores ]
    , [className    =?  c           --> doFloat                 | c <-  myFloats  ]
    , [className    =?  c           --> doShift "1:Web"         | c <-  myWebs    ]
    , [className    =?  c           --> doShift "2:Terminal"    | c <-  myDev     ]
    , [className    =?  c           --> doShift "3:Emacs"       | c <-  myEmacs   ]
    , [className    =?  c           --> doShift "4:Files"       | c <-  myFiles   ]
    , [className    =?  c           --> doShift "5:VMs"         | c <-  myVMs     ]
    , [className    =?  c           --> doShift "6:Chat"        | c <-  myChat   ]
    , [className    =?  c           --> doShift "7:Music"       | c <-  myMusic   ]
    , [isFullscreen                 --> myDoFullFloat                             ]
    ])
    
    where
      myWebs  = ["Firefox","Google-chrome"]
      myDev   = ["URxvt"]
      myFiles = ["Nautilus"]
      myEmacs = ["Emacs"]
      myChat  = ["Slack"]
      myMusic = ["Rhythmbox"]
      myVMs   = ["virtualbox", "virt-manager", "qemu"]
      myIgnores = ["desktop","trayer"]
      myFloats  = []

myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

-- Main config.
myConfig = defaultConfig {
    modMask            = myModMask
  , terminal           = myTerminal
--  , focusFollowsMouse  = False
  , workspaces         = myWorkSpaces
  , manageHook         = manageDocks <+> myManageHook <+> manageHook defaultConfig
  , layoutHook         = avoidStruts $ smartBorders myLayoutHook 
  {-, layoutHook         = avoidStruts $ smartBorders $ gaps [(U,30)] $ myLayoutHook -}
  , handleEventHook    = docksEventHook <+> handleEventHook defaultConfig
  , normalBorderColor  = "#2a2b2f"
  , focusedBorderColor = "DarkOrange"
  , borderWidth        = 2
  , startupHook        = spawn "stack exec taffybar"
  , keys               = myKeys
  }
