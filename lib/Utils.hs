--
-- rizumu's xmonad-config
-- git://github.com/rizumu/rizumu-xmonad.git
--
-- Adapted from:
-- https://github.com/davidbeckingsale/xmonad-config
-- https://github.com/pbrisbin/xmonad-config
-- https://github.com/capdevc/nnoell-xmonad
--
-- haddocks: <http://pbrisbin.com/static/docs/haskell/xmonad-config/Utils.html>
--
-------------------------------------------------------------------------------

module Utils
    (
    -- * Config entries
      myWorkspaces
    , myDzenXft
    , myLayout
    , myPP
    , myTheme
    , myStartupHook

    -- * Urgency
    , SpawnSomething(..)
    , myUrgencyHook
    , myUrgencyConfig

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

import Data.List (isInfixOf, isPrefixOf, elemIndex)

import Dzen (DzenConf(..), defaultDzenXft, DzenWidth(..))

import XMonad.Hooks.DynamicLog      (dzenPP, dynamicLogWithPP, PP(..), dzenColor, dzenEscape, wrap, shorten, pad)
import XMonad.Hooks.ManageDocks     (avoidStruts)
import XMonad.Hooks.UrgencyHook     (UrgencyHook(..), UrgencyConfig(..), urgencyConfig, SuppressWhen(OnScreen))
import XMonad.Hooks.SetWMName       (setWMName)
import XMonad.Layout.LayoutHints    (layoutHints)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

myStartupHook :: X ()
myStartupHook = do
          setWMName "LG3D"
          spawn     "exec conky -c ~/.xmonad/data/conky/main"
          spawn     "exec urxvtcd"
          spawn     "[[ -z \"$( pgrep firefox )\" ]] && exec firefox"
          spawn     "[[ -z \"$( pgrep --full '[u]zbl-core.*mail' )\" ]]                 && exec uzbl-tabbed --class=gmail      https://mail.google.com/"
          spawn     "[[ -z \"$( pgrep --full '[u]zbl-core.*google.com/calendar/' )\" ]] && exec uzbl-tabbed --class=gcal       https://www.google.com/calendar/"
          spawn     "[[ -z \"$( pgrep --full '[u]zbl-core.*soundcloud' )\" ]]           && exec uzbl-tabbed --class=soundcloud https://soundcloud.com"
          spawn     "[[ -z \"$( pgrep pidgin )\" ]] && exec pidgin"
          spawn     "[[ -z \"$( pgrep skype )\" ]] && exec skype"

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
myTheme = M.fromList [ ("myActiveBorderColor",   myActiveBorderColor)
                         , ("myInactiveBorderColor", myInactiveBorderColor)
                         , ("myBorderWidth",         show myBorderWidth)
                         ]
myDzenHeight = 18

--Font
myFont = "-misc-fixed-*-*-*-*-11-*-*-*-*-*-*-*"

--- Main Colours
myFgColor = "#DCDCCC"
myBgColor = "#2F2F2F"
myHighlightedFgColor = myFgColor
myHighlightedBgColor = "#F2583E"

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
colorBlack           = "#020202" --Background (Dzen_BG)
colorBlackAlt        = "#1c1c1c" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#161616" --Gray dark
colorWhite           = "#a9a6af" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorMagenta         = "#8e82a2"
colorBlue            = "#3475aa"
colorRed             = "#d74b73"
colorGreen           = "#99cc66"
myArrow              = "^fg(" ++ colorWhiteAlt ++ ")>^fg(" ++ colorBlue ++ ")>^fg(" ++ colorGray ++ ")>"
myNormalBorderColor  = colorBlackAlt
myFocusedBorderColor = colorGray

--- Urgency
myUrgencyHintFgColor = "#000000"
myUrgencyHintBgColor = "#ff6565"

--- }}}

--- Workspaces https://en.wikipedia.org/wiki/List_of_Unicode_characters
myWorkspaces :: [WorkspaceId]
myWorkspaces = [
  " 𐌎 ",  -- shell (grid)
  " λ ",  -- emacs (lambda)
  " Ϣ ",  -- www (web)
  " ⎇ ",  -- mail (send)
  " Φ ",  -- cal (500 years)
  " Ψ ",  -- music (wave)
  " ⇄ ",  -- im (connectivity)
  " ζ ",  -- float (fluid)
  " ꝏ "]  -- scratch (infinity)

-- dzen custom options
myDzenXft = defaultDzenXft
    { --font    = Just myFont
      height  = Just myDzenHeight
    , fgColor = Just myFgColor
    , bgColor = Just myBgColor
    }

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
myLayout = avoidStruts . layoutHints $ layoutHook defaultConfig

-- | @dzenPP@, zenburnish title/layout colors,
--   hiding of the NSP workspace and a nice @ppLayout@
--
-- > logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn d }
--

myPP :: PP
myPP = dzenPP
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
            "Circle"                 -> wrapIcon "dzen_bitmaps/ball.xbm"
            "Dishes"                 -> wrapIcon "dzen_bitmaps/ball.xbm"
            "Spiral"                 -> wrapIcon "dzen_bitmaps/ball.xbm"
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
myUrgencyHook :: SpawnSomething
myUrgencyHook = SpawnSomething urgencytone

-- | Default but still show urgent on visible non-focused workspace.
--
-- > xmonad $ withUrgencyHookC myUrgencyHook myUrgencyConfig $ defaultConfig
--
myUrgencyConfig :: UrgencyConfig
myUrgencyConfig = urgencyConfig { suppressWhen = OnScreen }

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
