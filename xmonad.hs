import Control.Monad              (when)
import Data.Monoid                (mappend, All (All))
import XMonad
import XMonad.Actions.CycleWS     (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Config.Gnome        (gnomeConfig)
import XMonad.Hooks.ManageHelpers (doCenterFloat, (/=?), isInProperty, isFullscreen, (-?>), doFullFloat, composeOne)
import XMonad.Hooks.SetWMName     (setWMName)
import XMonad.Layout.MagicFocus   (followOnlyIf, disableFollowOnWS)
import XMonad.Layout.NoBorders    (smartBorders)
import XMonad.Util.Run            (safeSpawn)
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Key bindings.
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Basic CycleWS setup (not using left/right because that clashes
    -- with browser history navigation):
    [ ((modMask,               xK_bracketright),  nextWS     )
    , ((modMask,               xK_bracketleft ),  prevWS     )
    , ((modMask .|. shiftMask, xK_bracketright),  shiftToNext)
    , ((modMask .|. shiftMask, xK_bracketleft ),  shiftToPrev)
    ]

isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

------------------------------------------------------------------------
-- Window rules:
 
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
myManageHook = composeAll
    [ title     =? "Ediff"                --> doFloat
    , className =? "File-roller"          --> doFloat
    , className =? "Gcalctool"            --> doFloat
    , className =? "Gimp"                 --> doFloat
    , className =? "Gmrun"                --> doCenterFloat
    , className =? "Gnome-typing-monitor" --> doFloat
    , className =? "MPlayer"              --> doFloat
    , (className =? "Nautilus" <&&>
       title /=? "x-nautilus-desktop")    --> doFloat -- need to exclude desktop or selection doesn't work
    -- that quick-search box used by gmarks in firefox:
    , (title    =? "Quick Search"
       <&&> className =? "Firefox")       --> doFloat
    , className =? "Screenruler"          --> doFloat
    , className =? "Skype"                --> doCenterFloat
    , className =? "Tsclient"             --> doFloat
    , className =? "VirtualBox"           --> doFloat
    , className =? "Thunderbird"          --> doF (W.shift "1") -- open thunderbird on first work-space
    , className =? "Unity-2d-panel"       --> doIgnore
    , isSplash                            --> doIgnore
    , resource  =? "desktop_window"       --> doIgnore
    , (title =? "Top Expanded Edge Panel"
       <&&> resource  =? "gnome-panel")   --> doIgnore
    , (title =? "Calendar"
       <&&> resource  =? "gnome-panel")   --> doIgnore
    , resource  =? "kdesktop"             --> doIgnore
    -- Forces typing-break to always open on the left screen (which is #1, not 0 because of my monitor orientation):
--    , className =? "Gnome-typing-monitor" --> doF (\w -> (flip W.shift) w $ fromJust $ W.lookupWorkspace 1 w)
    ]
    <+>
    composeOne [ isFullscreen -?> doFullFloat ] -- Fix flash fullscreen; see
                                                -- http://code.google.com/p/xmonad/issues/detail?id=228

-- Specify a workspace(s) to use focusFollowsMouse on (such as for use with gimp):
-- We will disable follow-mouse on all but the last:
followEventHook = followOnlyIf $ disableFollowOnWS allButLastWS
    where allButLastWS = init allWS
          allWS        = workspaces gnomeConfig

-- Full-screen support (eg, for Chrome); there's apparently a patch
-- for XMonad.Hooks.EwmhDesktops, but it hasn't been incorporated yet
-- so this does the job.  From
-- http://code.google.com/p/xmonad/issues/detail?id=339

-- Helper functions to fullscreen the window:
fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ W.float w r
    where r = W.RationalRect 0 0 1 1
tileWin w = windows $ W.sink w

fullscreenEventHook :: Event -> X All
fullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state  <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win

  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add    = 1
      toggle = 2
      -- The ATOM property type for changeProperty
      ptype  = 4
      action = head dat

  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
         fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace []
         tileWin win

  -- It shouldn't be necessary for xmonad to do anything more with this event
  return $ All False
fullscreenEventHook _ = return $ All True

main = spawn "xcompmgr" >> myConfig
    where myConfig = xmonad $ gnomeConfig {
         terminal          = "urxvt"
       , layoutHook        = smartBorders $ layoutHook gnomeConfig
       , handleEventHook   = handleEventHook gnomeConfig `mappend` followEventHook `mappend` fullscreenEventHook
       , manageHook        = myManageHook <+> manageHook gnomeConfig
       , startupHook       = startupHook gnomeConfig >> setWMName "LG3D"
       , focusFollowsMouse = False
       , normalBorderColor = "#dddddd" -- not sold on the default "grey" colour
       , keys              = \c -> myKeys c `M.union` keys gnomeConfig c
       }
