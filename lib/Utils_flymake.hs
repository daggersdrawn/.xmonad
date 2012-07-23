--
-- rizumu's xmonad-config
-- git://github.com/rizumu/rizumu-xmonad.git
--
-- Adapted from:
-- git://github.com/davidbeckingsale/xmonad-config.git
-- git://github.com/pbrisbin/xmonad-config.git
--
-- haddocks: <http://pbrisbin.com/static/docs/haskell/xmonad-config/Utils.html>
--
-------------------------------------------------------------------------------

module Utils
    (
    -- * Config entries
      rizumuWorkspaces
    , rizumuDzenXft
    , rizumuManageHook
    , rizumuLayout
    , rizumuPP
    , rizumuTheme
    , rizumuStartupHook

    -- * Urgency
    , SpawnSomething(..)
    , rizumuUrgencyHook
    , rizumuUrgencyConfig

    -- * Utilities
    , matchAny
    , name
    , role
    , hideNSP
    , yeganesh
    , runInTerminal
    , spawnInScreen
    , cleanStart
    ) where

import XMonad

import Data.List (isInfixOf, isPrefixOf)

import Dzen (DzenConf(..), defaultDzenXft, DzenWidth(..))

import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog      (dzenPP, dynamicLogWithPP, PP(..), dzenColor, wrap, shorten, pad)
import XMonad.Hooks.ManageDocks     (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers   (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.UrgencyHook     (UrgencyHook(..), UrgencyConfig(..), urgencyConfig, SuppressWhen(OnScreen))
import XMonad.Hooks.SetWMName       (setWMName)
import XMonad.Layout.LayoutHints    (layoutHints)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

rizumuStartupHook :: X ()
rizumuStartupHook = do
          setWMName "LG3D"
          spawnHere "exec conky -c ~/.xmonad/data/conky/main"
          spawnOn   "www"   "[[ -z \"$(pgrep firefox)\" ]] && exec firefox"
          spawnOn   "im"    "[[ -z \"$(pgrep pidgin)\" ]] && pidgin"
          spawnOn   "im"    "[[ -z \"$(pgrep skype)\" ]] && skype"
--{{{ Path variables
icons = "~/.icons/"
urgencytone  = "ogg123 -q ~/.xmonad/data/tones/urgency.ogg"
--}}}

--{{{ Helper Functions
stripIM s = if ("IM " `isPrefixOf` s) then drop (length "IM ") s else s
wrapIcon icon = "^p(5)^i(" ++ icons ++ icon ++ ")^p(5)"
--}}}

--{{{ Theme
-- Dzen defaults
rizumuTheme = M.fromList [ ("myActiveBorderColor",   myActiveBorderColor)
                         , ("myInactiveBorderColor", myInactiveBorderColor)
                         , ("myBorderWidth",         show myBorderWidth)
                         ]
myDzenHeight = 18

--Font
myFont = "xft:inconsolata:size=8:antialias=true"

--- Main Colours
myFgColor = "#DCDCCC"
myBgColor = "#2F2F2F"
myHighlightedFgColor = myFgColor
myHighlightedBgColor = "#93d44f"

--- Borders
myActiveBorderColor = myCurrentWsBgColor
myInactiveBorderColor = "#555753"
myBorderWidth = 2

--- Ws Stuff
myCurrentWsFgColor = "#FFFFFF"
myCurrentWsBgColor = myHighlightedBgColor
myVisibleWsFgColor = myBgColor
myVisibleWsBgColor = "#c8e7a8"
myHiddenWsFgColor = "#FFD7A7"
myHiddenEmptyWsFgColor = "#8F8F8F"
myUrgentWsBgColor = "#ff6565"
myTitleFgColor = myFgColor

--- Urgency
myUrgencyHintFgColor = "#000000"
myUrgencyHintBgColor = "#ff6565"

--- }}}

--- Workspaces
rizumuWorkspaces :: [WorkspaceId]
rizumuWorkspaces = [
  " sh ",
  " emacs ",
  " www ",
  " w3 ",
  " mail ",
  " cal ",
  " org ",
  " sc ",
  " im ",
  " . "]

-- dzen custom options
rizumuDzenXft = defaultDzenXft
    { font    = Just myFont
    , height  = Just myDzenHeight
    , fgColor = Just myFgColor
    , bgColor = Just myBgColor
    }

-- | Default plus docks, dialogs and smarter full screening.
rizumuManageHook :: ManageHook
rizumuManageHook = composeAll $ concat
    [ [ manageDocks                                      ]
    , [ manageHook defaultConfig                         ]
    , [ isDialog     --> doCenterFloat                   ]
    , [ isFullscreen --> doF W.focusDown <+> doFullFloat ]
    ]

-- | Match a string against any one of a window's class, title, name or
--   role.
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

-- | Match against @WM_NAME@.
name :: Query String
name = stringProperty "WM_NAME"

-- | Match against @WM_ROLE@.
role :: Query String
role = stringProperty "WM_ROLE"

-- Default plus hinting and avoidStruts.
rizumuLayout = avoidStruts . layoutHints $ layoutHook defaultConfig

-- | @dzenPP@, zenburnish title/layout colors,
--   hiding of the NSP workspace and a nice @ppLayout@
--
-- > logHook = dynamicLogWithPP $ rizumuPP { ppOutput = hPutStrLn d }
--

rizumuPP :: PP
rizumuPP = dzenPP
    { ppHidden = hideNSP
    , ppUrgent = ppUrgent dzenPP . hideNSP

    -- , ppSep = (wrapFg myHighlightedBgColor "|")
    -- , ppWsSep = ""
    -- , ppUrgent = wrapBg myUrgentWsBgColor
    -- , ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor
    -- , ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor
    , ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor

    , ppTitle  = dzenColor myTitleFgColor "" . pad
    , ppLayout = dzenColor myFgColor"" . pad . \s ->
        case s of
            "ResizableTall"          -> wrapIcon "dzen_bitmaps/tall.xbm"
            "Mirror ResizableTall"   -> wrapIcon "dzen_bitmaps/mtall.xbm"
            "Full"                   -> wrapIcon "dzen_bitmaps/full.xbm"
            _                        -> pad s
    }
    where
        wrapFgBg fgColor bgColor content = wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
        wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
        wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content

-- | Hide the "NSP" workspace.
hideNSP :: WorkspaceId -> String
hideNSP ws = if ws /= "NSP" then pad ws else ""

-- | Spawn any command on urgent; discards the workspace information.
data SpawnSomething = SpawnSomething String deriving (Read, Show)

instance UrgencyHook SpawnSomething where
    urgencyHook (SpawnSomething s) _ = spawn s

-- | Ding! on urgent via ossplay and homemade sound.
rizumuUrgencyHook :: SpawnSomething
rizumuUrgencyHook = SpawnSomething urgencytone

-- | Default but still show urgent on visible non-focused workspace.
--
-- > xmonad $ withUrgencyHookC rizumuUrgencyHook rizumuUrgencyConfig $ defaultConfig
--
rizumuUrgencyConfig :: UrgencyConfig
rizumuUrgencyConfig = urgencyConfig { suppressWhen = OnScreen }

-- | Spawns yeganesh <http://dmwit.com/yeganesh/>, set the environment
--   variable @$DMENU_OPTIONS@ to customize dmenu appearance, this is a
--   good @M-p@ replacement.
yeganesh :: MonadIO m => m ()
yeganesh = spawn "exe=`yeganesh -x -- $DMENU_OPTIONS` && eval \"exec $exe\""

-- | Execute a command in the user-configured terminal.
--
-- > runInTerminal [ "screen", "-S", "my-session", "-R", "-D", "my-session" ]
--
runInTerminal :: [String] -> X ()
runInTerminal args = asks config >>= \(XConfig { terminal = t }) -> spawn $ unwords (t:args)

-- | Spawn in accordance with <http://pbrisbin.com/posts/screen_tricks>.
spawnInScreen :: String -> X ()
spawnInScreen c = runInTerminal [ "-title", c, "-e bash -cl", "\"SCREEN_CONF=" ++ c, "screen -S", c, "-R -D", c ++ "\"" ]

-- | Kill (@-9@) any running dzen and conky processes before executing
--   the default restart command, this is a good @M-q@ replacement.
cleanStart :: MonadIO m => m ()
cleanStart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && "
                  ++ "for pid in `pgrep dzen2`; do kill -9 $pid; done && "
                  ++ "xmonad --recompile && xmonad --restart"
