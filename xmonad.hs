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
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- Note (hep016): e/w flipped because of the way I have my screens
    -- set up, to preserve the left-right orientation.
    -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

   -- ++

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
    [ className =? "Gmrun"                --> doCenterFloat
    , className =? "MPlayer"              --> doFloat
    , className =? "Gcalctool"            --> doFloat
    , className =? "Gimp"                 --> doFloat
    , className =? "Gnome-typing-monitor" --> doFloat
    , className =? "Mumbles"              --> doFloat
    , (className =? "Nautilus" <&&>
       title /=? "x-nautilus-desktop")    --> doFloat -- need to exclude desktop or selection doesn't work
    , className =? "Skype"                --> doFloat
    , className =? "Tsclient"             --> doFloat
    , className =? "VirtualBox"           --> doFloat
    , className =? "Evolution"            --> doF (W.shift "1") -- open evolution on first work-space
    , className =? "Thunderbird-bin"      --> doF (W.shift "1") -- open thunderbird on first work-space
    , resource  =? "desktop_window"       --> doIgnore
    , (title =? "Top Expanded Edge Panel"
       <&&> resource  =? "gnome-panel")   --> doIgnore
    , (title =? "Calendar"
       <&&> resource  =? "gnome-panel")   --> doIgnore
    , resource  =? "kdesktop"             --> doIgnore
    , title     =? "Ediff"                --> doFloat
    -- that quick-search box used by gmarks in firefox:
    , (title    =? "Quick Search"
       <&&> className =? "Shiretoko")     --> doFloat
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
