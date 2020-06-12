import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Semigroup
import System.Exit (exitSuccess)
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Decoration
import XMonad.Layout.IfMaxAlt
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

main :: IO ()
main = do
  proc <- spawnPipe "xmobar"
  xmonad $ ewmh $ docks $ dynamicProjects projects $ myConfig proc
  where
    myConfig proc =
      addKeys $
      def
        { layoutHook = avoidStruts $ deco vert ||| mono ||| deco horiz
        , logHook = myLogHook proc
        , manageHook = myManageHook
        , startupHook = myStartupHook
        , terminal = "alacritty"
        , modMask = mod4Mask
        , borderWidth = 0
        , workspaces = map projectName projects
        }

mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True

mono :: ModifiedLayout Rename (ModifiedLayout Spacing Full) a
mono = renamed [Replace monoIcon] $ mySpacing Full

vert :: ModifiedLayout Rename (ModifiedLayout Spacing Tall) a
vert = renamed [Replace vertIcon] $ mySpacing $ Tall 1 (3 / 100) 0.65

horiz :: ModifiedLayout Rename (ModifiedLayout Spacing (Mirror Tall)) a
horiz = renamed [Replace horizIcon] $ mySpacing $ Mirror (Tall 1 (3 / 100) 0.65)

deco ::
     Eq a
  => l a
  -> ModifiedLayout Rename (IfMaxAlt l (ModifiedLayout (Decoration NoFrillsDecoration DefaultShrinker) l)) a
deco layout =
  renamed [CutWordsLeft 11] $ IfMaxAlt 1 layout (noFrillsDeco shrinkText topBarTheme layout)

active :: String
active = cyan

topBarTheme :: Theme
topBarTheme =
  def
    { inactiveBorderColor = base03
    , inactiveColor = base03
    , inactiveTextColor = base03
    , activeBorderColor = active
    , activeColor = active
    , activeTextColor = active
    , urgentBorderColor = red
    , urgentTextColor = yellow
    , decoHeight = 5
    }

promptConfig :: XMonad.Prompt.XPConfig
promptConfig =
  def
    { font = "xft:Noto Sans:size=12"
    , bgColor = base03
    , fgColor = base0
    , fgHLight = base03
    , bgHLight = yellow
    , borderColor = base02
    , height = 24
    }

projects :: [Project]
projects =
  [ Project
      {projectName = "Web", projectDirectory = "~/Downloads", projectStartHook = Just launchBrowser}
  , Project
      { projectName = "Xmo"
      , projectDirectory = "~/projects/xmo"
      , projectStartHook =
          Just $ do
            sendMessage NextLayout
            runInTerm "" "stack build"
            runInTerm "" "zsh -i -c vim"
      }
  , Project {projectName = "scratch", projectDirectory = "~/", projectStartHook = Nothing}
  , Project
      { projectName = "Comms"
      , projectDirectory = "~/"
      , projectStartHook =
          Just $ do
            launchOutlook
            launchTeams
      }
  ]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "/home/david/.fehbg"
  spawnOnce "picom -f &"
  spawnOnce "qmenu_registrar &"
  activateProject (head projects)

myLogHook :: Handle -> X ()
myLogHook proc =
  fadeInactiveLogHook 0.9 <+>
  dynamicLogWithPP
    xmobarPP
      { ppOutput = hPutStrLn proc
      , ppTitle = xmobarColor cyan "" . shorten 50
      , ppCurrent = xmobarColor yellow "" . wrap "[" "]"
      , ppSep = "  "
      , ppLayout = \s -> " <fn=1>" ++ s ++ "</fn>"
      , ppOrder = \(ws:l:t:_) -> [l, ws, t]
      , ppSort = mkWsSort getWsCompare
      }

launchBrowser :: X ()
launchBrowser = spawn "brave"

launchEvernote :: X ()
launchEvernote = spawn "chromium --app=http://www.evernote.com/Home.action"

launchOutlook :: X ()
launchOutlook = spawn "chromium --app=https://outlook.office.com/mail/inbox"

launchTeams :: X ()
launchTeams = spawn "chromium --app=https://teams.microsoft.com/_#/calendarv2"

addKeys :: XConfig a -> XConfig a
addKeys conf = addDescrKeys' ((modMask conf, xK_b), displayKeyMap) myKeys conf

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf =
  section
    "Launching and killing"
    [ ("M-S-<Return>", "Launch Terminal", spawn $ XMonad.terminal conf)
    , ("M-S-c", "Close the focused window", kill)
    , ("M-p", "Application launch menu", spawn "rofi -show drun")
    , ("M-S-p", "List running applications", spawn "rofi -show window")
    , ("M-m", "Show application menus", spawn "qmenu_hud")
    ] ++
  section
    "Launch specific applications"
    [ ("M-n", "Launch Evernote", launchEvernote)
    , ("M-o", "Launch Outlook", launchOutlook)
    , ("M-S-o", "Launch Teams", launchTeams)
    ] ++
  section
    "Changing layouts"
    [ ("M-<Space>", "Next Layout", sendMessage NextLayout)
    , ("M-S-<Space>", "Reset the layout", setLayout $ XMonad.layoutHook conf)
    , ("M-n", "Refresh", refresh)
    ] ++
  section
    "Move focus up or down the window stack"
    [ ("M-<Tab>", "Focus down", windows W.focusDown)
    , ("M-S-<Tab>", "Focus up", windows W.focusUp)
    , ("M-j", "Focus down", windows W.focusDown)
    , ("M-k", "Focus up", windows W.focusUp)
    ] ++
  section
    "Modifying the window order"
    [ ("M-<Return>", "Swap with the master", windows W.swapMaster)
    , ("M-S-j", "Swap down", windows W.swapDown)
    , ("M-S-k", "Swap up", windows W.swapUp)
    ] ++
  section
    "Resizing windows"
    [ ("M-h", "Shrink main window", sendMessage Shrink)
    , ("M-l", "Expand main window", sendMessage Expand)
    , ("M-s", "Increase window gaps", incScreenWindowSpacing 5)
    , ("M-S-s", "Decrease window gaps", decScreenWindowSpacing 5)
    ] ++
  section
    "Floating layer support"
    [("M-t", "Push floating to tiled", withFocused $ windows . W.sink)] ++
  section
    "Change the number of windows in the master area"
    [ ("M-,", "Increase master windows", sendMessage (IncMasterN 1))
    , ("M-.", "Decrease master windows", sendMessage (IncMasterN (-1)))
    ] ++
  section
    "Quit, or restart"
    [ ("M-S-q", "Quit", io exitSuccess)
    , ("M-q", "Restart", spawn "xmonad --recompile && xmonad --restart")
    , ("M-<Backspace>", "System shutdown menu", spawn "/home/david/bin/shutdownmenu.sh")
    , ("M-S-z", "Lock", spawn "i3lock-fancy")
    ] ++
  section
    "Projects"
    [ ("M-x", "Switch to project", switchProjectPrompt promptConfig)
    , ("M-S-x", "Shift window to project", shiftToProjectPrompt promptConfig)
    ] ++
  section
    "Switching workspaces"
    [ ("M-" ++ m ++ k, n ++ i, f i)
    | (f, m, n) <-
        [ (toggleOrView, "", "Switch to workspace ")
        , (windows . W.shift, "S-", "Move client to workspace ")
        ]
    , (i, k) <- zip (XMonad.workspaces conf) (map show [1 .. 9 :: Integer])
    ] ++
  section
    "Switching screens"
    [ ( "M-" ++ m ++ key
      , n ++ show sc
      , screenWorkspace (fromInteger sc) >>= flip whenJust (windows . f))
    | (f, m, n) <- [(W.view, "", "Switch to screen "), (W.shift, "S-", "Move client to screen ")]
    , (key, sc) <- zip ["w", "e"] [0 ..]
    ] ++
  section
    "Brightness"
    [ ("<XF86MonBrightnessUp>", "Increase screen brightness", spawn "xbacklight -inc 10")
    , ("<XF86MonBrightnessDown>", "Decrease screen brightness", spawn "xbacklight -dec 10")
    , ( "<XF86KbdBrightnessUp>"
      , "Increase keyboard brightness"
      , spawn "xbacklight -ctrl smc::kbd_backlight -inc 10")
    , ( "<XF86KbdBrightnessDown>"
      , "Decrease keyboard brightness"
      , spawn "xbacklight -ctrl smc::kbd_backlight -dec 10")
    ] ++
  section
    "Wallpaper"
    [ ("M-'", "New random wallpaper", spawn "/home/david/bin/random-wallpaper.sh")
    , ( "M-C-'"
      , "Save current wallpaper"
      , spawn "cp --backup=t /home/david/.wallpaper /home/david/.wallpapers")
    , ( "M-S-'"
      , "Random saved wallpaper"
      , spawn "feh --bg-scale --randomize /home/david/.wallpapers/")
    , ( "M-C-S-'"
      , "Select and manage wallpaper"
      , spawn "feh --action ';feh --bg-scale %f' /home/david/.wallpapers/")
    ] ++
  section
    "Audio"
    [ ("<XF86AudioLowerVolume>", "Lower volume", spawn "amixer -q set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", "Raise volume", spawn "amixer -q set Master 5%+ unmute")
    , ("<XF86AudioMute>", "Mute", spawn "amixer -q set Master mute")
    ]
  where
    section name ks = subtitle name : mkNamedKeymap conf (map (\(a, b, c) -> (a, addName b c)) ks)

displayKeyMap :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
displayKeyMap x =
  addName "Show Keybindings" $ do
    selection <- dmenu (showKm x)
    conf <- ask
    M.findWithDefault (return () :: X ()) (toKeyBinding selection) (keyActions conf)
  where
    selectionMap = zip (filter (not . null) (showKm x)) (map fst x)
    toKeyBinding sel = fromMaybe (0, 0) $ lookup sel selectionMap

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [ className =? "Zenity" --> doFloat
    , className =? "Yad" --> doFloat
    , className =? "Gimp" --> doFloat
    , resource =? "desktop_window" --> doIgnore
    , resource =? "kdesktop" --> doIgnore
    ]

base03 :: String
base03 = "#002b36"

base02 :: String
base02 = "#073642"

base01 :: String
base01 = "#586e75"

base00 :: String
base00 = "#657b83"

base0 :: String
base0 = "#839496"

base1 :: String
base1 = "#93a1a1"

base2 :: String
base2 = "#eee8d5"

base3 :: String
base3 = "#fdf6e3"

yellow :: String
yellow = "#b58900"

orange :: String
orange = "#cb4b16"

red :: String
red = "#dc322f"

magenta :: String
magenta = "#d33682"

violet :: String
violet = "#6c71c4"

blue :: String
blue = "#268bd2"

cyan :: String
cyan = "#2aa198"

green :: String
green = "#859900"

monoIcon :: String
monoIcon = "\xe9e1"

vertIcon :: String
vertIcon = "\xe900"

horizIcon :: String
horizIcon = "\xea1b"
