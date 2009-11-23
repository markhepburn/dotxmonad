import Data.Maybe
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Util.Run (safeSpawn)
import XMonad.Hooks.DynamicLog (dynamicLogString, PP(..), defaultPP)
import XMonad.Hooks.ManageHelpers (doCenterFloat, (/=?))
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


-- dynamicLog theme (suppress everything but window title)
myPP = defaultPP
    { ppLayout          = const ""
    , ppCurrent         = const ""
    , ppVisible         = const ""
    , ppHidden          = const ""
    , ppHiddenNoWindows = const ""
    , ppUrgent          = const ""
    , ppTitle           = id
    , ppWsSep           = ""
    , ppSep             = "" }

------------------------------------------------------------------------
-- Key bindings.
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Not using this at the moment until I think of a better key
    -- combination, now that I have dual monitors working again!:

    -- -- this currently has problems; it doesn't always work on firefox.
    -- -- I'm not sure why,but I presume it is to do with the text that
    -- -- is being passed to firefox, via the shell (eg, it could contain
    -- -- '&', '#', etc.  An alternative is safeSpawn, but as far as I
    -- -- can tell that won't accept the string containing the icon as
    -- -- well, and appending the arguments to the main argument also
    -- -- doesn't work.
    -- [ ((modMask .|. shiftMask, xK_w           ),
    --    dynamicLogString myPP >>= \s -> safeSpawn "notify-send" [s])]
                                                   
    -- ++

    -- Basic CycleWS setup (not using left/right because that clashes
    -- with browser history navigation):
    [ ((modMask,               xK_bracketright),  nextWS     )
    , ((modMask,               xK_bracketleft ),  prevWS     )
    , ((modMask .|. shiftMask, xK_bracketright),  shiftToNext)
    , ((modMask .|. shiftMask, xK_bracketleft ),  shiftToPrev)
    ]
 
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
    , className =? "Skype.real"           --> doCenterFloat
    , className =? "Tsclient"             --> doFloat
    , className =? "VirtualBox"           --> doFloat
    , className =? "Thunderbird-bin"      --> doF (W.shift "1") -- open thunderbird on first work-space
    , resource  =? "desktop_window"       --> doIgnore
    , (title =? "Top Expanded Edge Panel"
       <&&> resource  =? "gnome-panel")   --> doIgnore
    , (title =? "Calendar"
       <&&> resource  =? "gnome-panel")   --> doIgnore
    , resource  =? "kdesktop"             --> doIgnore
    -- Forces typing-break to always open on the left screen (which is #1, not 0 because of my monitor orientation):
--    , className =? "Gnome-typing-monitor" --> doF (\w -> (flip W.shift) w $ fromJust $ W.lookupWorkspace 1 w)
    ]
 
main = spawn "xcompmgr" >> myConfig
    where myConfig = xmonad $ gnomeConfig {
         terminal          = "urxvt"
       , logHook           = logHook gnomeConfig >> setWMName "LG3D"
       , layoutHook        = smartBorders $ layoutHook gnomeConfig
       , manageHook        = myManageHook
       , focusFollowsMouse = False
       , normalBorderColor = "#dddddd" -- not sold on the default "grey" colour
       , keys              = \c -> myKeys c `M.union` keys gnomeConfig c
       }
