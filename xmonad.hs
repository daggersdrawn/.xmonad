--
-- rizumu's xmonad-config
-- git://github.com/rizumu/rizumu-xmonad.git
--
-- Adapted from:
-- git://github.com/davidbeckingsale/xmonad-config.git
-- git://github.com/pbrisbin/xmonad-config.git

import XMonad

-- <http://pbrisbin.com/xmonad/docs/Utils.html>
import Utils
import Dzen (DzenConf(..), TextAlign(..), defaultDzenXft,
                spawnDzen, spawnToDzen)

import ScratchPadKeys             (scratchPadList, manageScratchPads, scratchPadKeys)
import System.IO                  (hPutStrLn)
import XMonad.Hooks.DynamicLog    (dynamicLogWithPP, PP(..))
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.UrgencyHook   (withUrgencyHookC)
import XMonad.Util.EZConfig       (additionalKeysP)

import XMonad.Hooks.ManageDocks   (avoidStruts, manageDocks, ToggleStruts(..) )

import XMonad.Layout.IM
import XMonad.Layout.NoBorders    (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Map ((!))


main = do
    d <- spawnDzen rizumuDzenXft { screen = Just 1 }
    spawnToDzen "conky -c ~/.conky/dzen" conkyBar
    xmonad $ withUrgencyHookC rizumuUrgencyHook rizumuUrgencyConfig $ defaultConfig
        { terminal           = "urxvtcd"
        , normalBorderColor  = rizumuTheme ! "myInactiveBorderColor"
        , focusedBorderColor = rizumuTheme ! "myActiveBorderColor"
        , borderWidth        = read $ rizumuTheme ! "myBorderWidth"
        , layoutHook         = smartBorders $ avoidStruts $ myLayoutHook
        , manageHook         = rizumuManageHook <+> myManageHook
        , logHook            = dynamicLogWithPP $ rizumuPP { ppOutput = hPutStrLn d }
        , modMask            = mod4Mask
        , mouseBindings      = myMouseBindings
        , workspaces         = rizumuWorkspaces
        , focusFollowsMouse  = True
        , startupHook        = rizumuStartupHook
        } `additionalKeysP`  myKeys

    where
        conkyBar :: DzenConf
        conkyBar = rizumuDzenXft
            { screen = Just 0
            , alignment = Just LeftAlign
            }

-- Layouts
myLayoutHook = avoidStruts $ onWorkspace " 4 im " imLayout $ standardLayouts
               where standardLayouts = tiled ||| Mirror tiled ||| Full
                     imLayout = withIM (2/10) (Role "buddy_list") (standardLayouts)
                     tiled = ResizableTall nmaster delta ratio []
                     nmaster = 1
                     delta = 0.03
                     ratio = 0.5

--{{{ Hook for managing windows
myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions ] <+> manageScratchPads scratchPadList

    where myActions = [ ("Xmessage"       , doCenterFloat     )
                      , ("Gmrun"          , doCenterFloat     )
                      , ("Pidgin"         , doShift " im "    )
                      , ("Skype"          , doShift " im "    )
                      , ("Chrome"         , doShift " mail "  )
                      , ("Firefox"        , doShift " www "   )
                      , ("Emacs"          , doShift " emacs " )
                      , ("irssi"          , doShift " irc "   )
                      , ("ncmpcpp"        , doShift " ongaku ")
                      , ("Htop"           , doShift " stats " )
                      , ("Wicd-client.py" , doFloat           )
                      ]
--}}}

--{{{ Keybindings
myKeys :: [(String, X())]
myKeys = [ ("m1-<Space>"               , yeganesh) -- TODO
         , ("M-p"                      , yeganesh)
         , ("M4-w"                     , spawn "firefox")
         --, ("M4-c"                   , spawn "chromium --app='https://calendar.google.com'")
         , ("M4-f", spawn "urxvt -e mc")
         , ("M4-n"                     , spawnInScreen "ncmpcpp")
         , ("M4-m"                     , spawn "chromium --app='https://mail.google.com'")
         --, ("M4-i"                     , spawnInScreen "irssi" )
         , ("M4-r"                     , spawnInScreen "rtorrent")
         , ("<xK_Print>"               , spawn "scrot")
         , ("<xF86XK_AudioMute>"       , spawn "amixer -q set PCM toggle")
         , ("<xF86XK_AudioRaiseVolume>", spawn "amixer -q set PCM 2+")
         , ("<xF86XK_AudioLowerVolume>", spawn "amixer -q set PCM 2-")
         , ("<xF86XK_AudioPlay>"       , spawn "exaile -t")
         , ("<xF86XK_AudioStop>"       , spawn "exaile -s")
         , ("<xF86XK_AudioNext>"       , spawn "exaile -n")
         , ("<xF86XK_AudioPrev>"       , spawn "exaile -p")
         , ("M4-y"                     , sendMessage ToggleStruts)
         , ("M4-u"                     , sendMessage MirrorShrink)
         , ("M4-i"                     , sendMessage MirrorExpand)
         , ("M-q"                      , cleanStart) -- restart xmonad
         ] ++ scratchPadKeys scratchPadList
--}}}

--{{{ Mousebindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
--}}}
