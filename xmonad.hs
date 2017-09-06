import XMonad
import XMonad.Actions.CycleWS         (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.GroupNavigation (nextMatch, historyHook, Direction(History))
import XMonad.Config.Gnome            (gnomeConfig)
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers     (doCenterFloat, (/=?), isInProperty, isFullscreen, (-?>), doFullFloat, composeOne)
import XMonad.Hooks.SetWMName         (setWMName)
import XMonad.Layout.Fullscreen       (fullscreenEventHook, fullscreenManageHook, fullscreenFull, fullscreenFloat)
import XMonad.Layout.MagicFocus       (followOnlyIf, disableFollowOnWS)
import XMonad.Prompt                  (defaultXPConfig, XPConfig(..), XPPosition(Top))
import XMonad.Prompt.Shell            (shellPrompt)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Control.Monad
import Data.Maybe

------------------------------------------------------------------------
-- Prompt setup:
myPrompt = defaultXPConfig {
             position    = Top
           , font        = "xft:Consolas-14"
           , height      = 24
           -- Zenburn!:
           , bgColor     = "#3F3F3F"
           , fgColor     = "#EFEFEF"
           , fgHLight    = "#000D18"
           , bgHLight    = "#8FAF9F"
           , borderColor = "#719E7F"
           }

------------------------------------------------------------------------
-- Key bindings.
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
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

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
        sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
        when (fromIntegral x `notElem` sup) $
          changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

-- Specify a workspace(s) to use focusFollowsMouse on (such as for use with gimp):
-- We will disable follow-mouse on all but the last:
followEventHook = followOnlyIf $ disableFollowOnWS allButLastWS
    where allButLastWS = init allWS
          allWS        = workspaces gnomeConfig

main = spawn "xcompmgr" >> myConfig
    where myConfig = xmonad $ gnomeConfig {
         terminal           = "roxterm"
       , layoutHook         = (fullscreenFloat . fullscreenFull) $ layoutHook gnomeConfig
       , logHook            = historyHook <+> fadeInactiveLogHook 0.85
       , handleEventHook    = handleEventHook gnomeConfig <+> followEventHook <+> fullscreenEventHook
       , manageHook         = myManageHook <+> fullscreenManageHook <+> manageHook gnomeConfig
       , startupHook        = startupHook gnomeConfig >> setWMName "LG3D" >> addEWMHFullscreen
       , focusFollowsMouse  = False
       , borderWidth        = 0 -- No borders; fade inactive windows instead (see fadeInactiveLogHook)
       , keys               = \c -> myKeys c `M.union` keys gnomeConfig c
       }
