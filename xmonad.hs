-- rizumu's xmonad-config
-- git://github.com/rizumu/rizumu-xmonad.git
--
-- Adapted from:
-- git://github.com/davidbeckingsale/xmonad-config.git
-- git://github.com/pbrisbin/xmonad-config.git
-- https://github.com/capdevc/nnoell-xmonad
--
-------------------------------------------------------------------------------

import XMonad

import Data.Ratio
import System.IO                        (hPutStrLn)

-- <http://pbrisbin.com/xmonad/docs/Utils.html>
import Utils
import Dzen --(DzenConf(..), TextAlign(..), DzenWidth(..), defaultDzenXft,
            --    spawnDzen, spawnToDzen)
import ScratchPadKeys                   (scratchPadList, manageScratchPads, scratchPadKeys)

import XMonad.Hooks.DynamicLog          (dynamicLogWithPP, PP(..))
import XMonad.Hooks.ManageDocks         (avoidStruts, manageDocks, ToggleStruts(..) )
import XMonad.Hooks.ManageHelpers       (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.UrgencyHook         (withUrgencyHookC)
import XMonad.Util.EZConfig             (additionalKeysP)
import XMonad.Util.Run                  (spawnPipe)

import XMonad.Layout.Circle
import XMonad.Layout.Dishes
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders          (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Map ((!))


main :: IO ()
main = do
    taskbarPrimaryLeft  <- spawnDzen dzenPrimaryLeft
    taskbarPrimaryRight <- spawnToDzen "conky -c ~/.xmonad/data/conky/taskbar_primary"  dzenPrimaryRight
    taskbarDivider      <- spawnToDzen " " dzenDivider { yPosition = Just $ 18 }
    taskbarSecondary    <- spawnToDzen "conky -c ~/.xmonad/data/conky/taskbar_secondary" dzenSecondary
    xmonad $ withUrgencyHookC myUrgencyHook myUrgencyConfig $ defaultConfig
        { terminal           = "urxvtcd"
        , normalBorderColor  = myTheme ! "myInactiveBorderColor"
        , focusedBorderColor = myTheme ! "myActiveBorderColor"
        , borderWidth        = read $ myTheme ! "myBorderWidth"
        , layoutHook         = smartBorders $ avoidStruts $ myLayoutHook
        , manageHook         = myManageHook  <+> extraManageHook
        , logHook            = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn taskbarPrimaryLeft }
        , modMask            = mod4Mask
        , mouseBindings      = myMouseBindings
        , workspaces         = myWorkspaces
        , focusFollowsMouse  = True
        , startupHook        = myStartupHook
        } `additionalKeysP`  myKeys

    where
        dzenPrimaryLeft :: DzenConf
        dzenPrimaryLeft = myDzenXft  { width = Just $ Percent 35 }

        dzenPrimaryRight :: DzenConf
        dzenPrimaryRight = myDzenXft { alignment  = Just RightAlign
                                         , xPosition  = Just $ Percent 35
                                         , width      = Just $ Percent 55
                                         }

        dzenDivider :: DzenConf
        dzenDivider = myDzenXft      { alignment  = Just Centered
                                         , width      = Just $ Percent 100
                                         , height     = Just $ 1
                                         , bgColor    = Just $ "#93d44f"
                                         }

        dzenSecondary :: DzenConf
        dzenSecondary = myDzenXft    { alignment  = Just Centered
                                         , yPosition  = Just $ 19
                                         , width      = Just $ Percent 100
                                         , bgColor    = Just $ "#333333"
                                         }


-- Layouts
myLayoutHook = avoidStruts $
               onWorkspace " 𐌎 "      shLayouts     $
               onWorkspace " λ "     shLayouts   $
               onWorkspace " Ϣ "     webLayouts   $
               onWorkspace " ⎇ "    webLayouts     $
               onWorkspace " ⇄ "      threecolLayout $
               onWorkspace " Φ "     webLayouts     $
               onWorkspace " Ψ "      webLayouts     $
               onWorkspace " ζ "       floatLayout    $
               standardLayouts
               where tiled           = ResizableTall nmaster delta ratio []
                     gridLayout      = avoidStruts $ Grid
                     spiralLayout    = spiral ( 1 % 1)
                     threecolLayout  = ThreeCol 1 (3/100) (1/2)
                     floatLayout     = simpleFloat
                     fullLayout      = Full
                     nmaster         = 1
                     delta           = 0.03
                     ratio           = 0.5
                     shLayouts       =     tiled
                                       ||| Mirror tiled
                                       ||| gridLayout
                                       ||| threecolLayout
                                       ||| fullLayout
                     webLayouts      =     spiralLayout
                                       ||| fullLayout
                                       ||| tiled
                                       ||| Mirror tiled
                     standardLayouts =     tiled
                                       ||| Mirror tiled
                                       ||| gridLayout
                                       ||| threecolLayout
                                       ||| Circle
                                       ||| Dishes 2 (1/6)
                                       ||| spiralLayout
                                       ||| fullLayout
                                       ||| floatLayout

--{{{ Hook for managing windows
myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions ] <+> manageScratchPads scratchPadList
    where myActions = [ ("Xmessage"            , doCenterFloat     )
                      , ("Gmrun"               , doCenterFloat     )
                      , ("gitg"                , doCenterFloat     )
                      , ("Firefox Preferences" , doFloat           )
                      , ("Saved Passwords"     , doFloat           )
                      , ("Emacs"               , doShift " λ " )
                      , ("Firefox"             , doShift " Ϣ "   )
                      , ("Chromium"            , doShift " Ϣ "   )
                      , ("Uzbl"                , doShift " Ϣ "   )
                      , ("Uzbl-core"           , doShift " Ϣ "   )
                      , ("Hipchat"             , doShift " ⎇ "    )
                      , ("Pidgin"              , doShift " ⇄ "    )
                      , ("Skype"               , doShift " ⇄ "    )
                      ]
-- | Default plus docks, dialogs and smarter full screening.
extraManageHook :: ManageHook
extraManageHook = composeAll $ concat
    [ [ manageDocks                                      ]
    , [ manageHook defaultConfig                         ]
    , [ isDialog     --> doCenterFloat                   ]
    , [ isFullscreen --> doF W.focusDown <+> doFullFloat ]
    ]
--}}}

--{{{ Keybindings http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html
myKeys :: [(String, X())]
myKeys = [ ("M-g"                       , spawn "firefox")
         , ("M-S-s"                     , spawn "xscreensaver-command --lock")
         , ("M-r"                       , spawn "rox")
         , ("M-<Backspace>"             , spawn "mpc toggle")
         , ("M-<xK_Print>"              , spawn "sleep 0.2; scrot -s")
         , ("<xK_Print>"                , spawn "scrot")
         , ("<xF86XK_AudioMute>"        , spawn "amixer -q set PCM toggle")
         , ("<xF86XK_AudioRaiseVolume>" , spawn "amixer -q set PCM 2+")
         , ("<xF86XK_AudioLowerVolume>" , spawn "amixer -q set PCM 2-")
         , ("<xF86XK_AudioPlay>"        , spawn "exaile -t")
         , ("<xF86XK_AudioStop>"        , spawn "exaile -s")
         , ("<xF86XK_AudioNext>"        , spawn "exaile -n")
         , ("<xF86XK_AudioPrev>"        , spawn "exaile -p")
         , ("M-S-n"                     , spawn "touch ~/.pomodoro_session")
         , ("M-C-n"                     , spawn "[ -f ~/.pomodoro_session ] && rm ~/.pomodoro_session")
         , ("M-y"                       , sendMessage ToggleStruts)
         , ("M-u"                       , sendMessage MirrorShrink)
         , ("M-i"                       , sendMessage MirrorExpand)
         , ("M-q"                       , cleanStart) -- restart xmonad
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
