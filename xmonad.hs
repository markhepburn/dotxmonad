import XMonad
import XMonad.Actions.CycleWS         (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.GroupNavigation (nextMatch, historyHook, Direction(History))
import XMonad.Config.Desktop          (desktopConfig)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops      (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageHelpers     (doCenterFloat, (/=?), isInProperty, isFullscreen, (-?>), doFullFloat, composeOne)
import XMonad.Hooks.SetWMName         (setWMName)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook       (focusUrgent, withUrgencyHook, NoUrgencyHook(..))
import XMonad.Layout.Fullscreen       (fullscreenEventHook, fullscreenManageHook, fullscreenFull, fullscreenFloat)
import XMonad.Layout.MagicFocus       (followOnlyIf, disableFollowOnWS)
import XMonad.Prompt                  (XPConfig(..), XPPosition(Top))
import XMonad.Prompt.FuzzyMatch       (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Shell            (shellPrompt)
import XMonad.Util.Run                (spawnPipe)
import XMonad.Util.EZConfig           (additionalKeysP)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Control.Monad
import Data.List
import Data.Maybe
import System.IO (hPutStrLn)

------------------------------------------------------------------------
-- Prompt setup:
myPrompt = def {
             position    = Top
           , font        = "xft:Consolas-14"
           , height      = 40
           -- Fuzzy match:
           , searchPredicate = fuzzyMatch
           , sorter        = fuzzySort
           -- Zenburn!:
           , bgColor     = "#3F3F3F"
           , fgColor     = "#EFEFEF"
           , fgHLight    = "#000D18"
           , bgHLight    = "#8FAF9F"
           , borderColor = "#719E7F"
           }

------------------------------------------------------------------------
-- Key bindings.
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList
    -- Basic CycleWS setup (not using left/right because that clashes
    -- with browser history navigation):
    [ ((modMask,               xK_bracketright),  nextWS     )
    , ((modMask,               xK_bracketleft ),  prevWS     )
    , ((modMask .|. shiftMask, xK_bracketright),  shiftToNext)
    , ((modMask .|. shiftMask, xK_bracketleft ),  shiftToPrev)
    -- History -- pop back to most-recently-used focus:
    , ((modMask .|. shiftMask, xK_m),    nextMatch History (return True))
    -- shell prompt:
    , ((modMask .|. shiftMask, xK_p),    shellPrompt myPrompt)
    -- Screenshot (flameshot)
    , ((modMask .|. shiftMask, xK_s),    spawn "flameshot gui")
    -- File explorer:
    , ((modMask,               xK_Home), spawn "nautilus"    )
    -- Focus urgent:
    , ((modMask,               xK_u),    focusUrgent)
    -- screensaver / lock:
    , ((modMask .|. controlMask, xK_l),  spawn "xset s activate")
    , ((0, 0x1008FF01), spawn "amixer -q set Master toggle")
    ]

myAdditionalKeys = [
  -- Volume control:
    ("<XF86AudioRaiseVolume>", spawn "amixer -q Master set 2+" )
  , ("<XF86AudioLowerVolume>", spawn "amixer -q Master set 2-")
  , ("<XF86AudioMute>",        spawn "amixer -q Master set toggle"  )
  -- Brightness control; install https://github.com/Ventto/lux/
  , ("<XF86MonBrightnessUp>",  spawn "lux -a 5%")
  , ("<XF86MonBrightnessDown>", spawn "lux -s 5%")
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
    , className =? "Gloobus-preview"      --> doFloat
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
    , resource  =? "stalonetray"          --> doIgnore
    , className =? "Unity-2d-panel"       --> doIgnore
    , isSplash                            --> doIgnore
    , resource  =? "desktop_window"       --> doIgnore
    , resource  =? "xfce4-notifyd"        --> doIgnore
    , (title =? "Top Expanded Edge Panel"
       <&&> resource  =? "gnome-panel")   --> doIgnore
    , (title =? "Calendar"
       <&&> resource  =? "gnome-panel")   --> doIgnore
    , resource  =? "kdesktop"             --> doIgnore
    , fmap ("Android Emulator" `isInfixOf`) title --> doFloat
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
          allWS        = workspaces def

myTitleColor     = "#eeeeee"  -- color of window title
myTitleLength    = 70         -- truncate window title to this length
myCurrentWSColor = "#e6744c"  -- color of active workspace
myVisibleWSColor = "#c185a7"  -- color of inactive workspace
myUrgentWSColor  = "#cc0000"  -- color of workspace with 'urgent' window
myCurrentWSLeft  = "["        -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft  = "("        -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft  = "{"         -- wrap urgent workspace with these
myUrgentWSRight = "}"

myXmobarPP :: PP
myXmobarPP = def
  { ppLayout = const ""
  , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
  , ppCurrent = xmobarColor myCurrentWSColor ""
                . wrap myCurrentWSLeft myCurrentWSRight
  , ppVisible = xmobarColor myVisibleWSColor ""
                . wrap myVisibleWSLeft myVisibleWSRight
  , ppUrgent = xmobarColor myUrgentWSColor ""
               . wrap myUrgentWSLeft myUrgentWSRight
  }

-- https://www.reddit.com/r/xmonad/comments/hlektm/installing_xmonad_with_ghcup_and_cabal/
-- Borrowing from https://xmonad.org/TUTORIAL.html#make-xmonad-and-xmobar-talk-to-each-other
main = do
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmhFullscreen
    $ ewmh
    $ withEasySB (statusBarProp "xmobar -D 180 ~/.xmonad/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
    $ desktopConfig {
    terminal           = "alacritty -e tmux new-session -A -s main"
    , layoutHook         = (fullscreenFloat . fullscreenFull) $ layoutHook desktopConfig
    , handleEventHook    = handleEventHook desktopConfig <+> followEventHook <+> fullscreenEventHook
    , manageHook         = myManageHook <+> fullscreenManageHook <+> manageHook desktopConfig
    , startupHook        = startupHook desktopConfig >> setWMName "LG3D" >> spawn "~/.xmonad/startup-hook"
    , focusFollowsMouse  = False
    , borderWidth        = 0 -- No borders; fade inactive windows instead (see fadeInactiveLogHook)
    , keys               = \c -> myKeys c `M.union` keys desktopConfig c
    } `additionalKeysP` myAdditionalKeys
