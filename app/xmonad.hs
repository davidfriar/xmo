import qualified Colors.Gruvbox.Dark as Colors
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (All)
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Layout.Decoration
import XMonad.Layout.IfMaxAlt
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoFrillsDecoration

import XMonad.Layout.PerScreenModifier
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.Loggers (logTitleOnScreen, shortenL, xmobarColorL)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad(NS)
  , NamedScratchpads
  , customFloating
  , namedScratchpadAction
  , namedScratchpadManageHook
  )
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

main :: IO ()
main =
  xmonad $ dynamicSBs barSpawner $ ewmhFullscreen . ewmh $ docks $ dynamicProjects projects myConfig
  where
    myConfig =
      addKeys $
      def
        { layoutHook =
            forScreen 0 reflectHoriz $
            avoidStruts $ mkToggle (single REFLECTX) $ deco vert ||| mono ||| deco horiz
        , logHook = myLogHook
        , manageHook = myManageHook
        , handleEventHook = myEventHook
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
horiz = renamed [Replace horizIcon] $ mySpacing $ Mirror (Tall 2 (3 / 100) 0.65)

deco ::
     Eq a
  => l a
  -> ModifiedLayout Rename (IfMaxAlt l (ModifiedLayout (Decoration NoFrillsDecoration DefaultShrinker) l)) a
deco layout =
  renamed [CutWordsLeft 11] $ IfMaxAlt 1 layout (noFrillsDeco shrinkText topBarTheme layout)

active :: String
active = Colors.blue

topBarTheme :: Theme
topBarTheme =
  def
    { inactiveBorderColor = Colors.background
    , inactiveColor = Colors.background
    , inactiveTextColor = Colors.background
    , activeBorderColor = active
    , activeColor = active
    , activeTextColor = active
    , urgentBorderColor = Colors.red
    , urgentTextColor = Colors.orange
    , decoHeight = 5
    }

promptConfig :: XMonad.Prompt.XPConfig
promptConfig =
  def
    { font = "xft:Noto Sans:size=12"
    , bgColor = Colors.background
    , fgColor = Colors.text
    , fgHLight = Colors.background
    , bgHLight = Colors.yellow
    , borderColor = Colors.backgroundHighlight
    , height = 25
    }

exitPromptConfig :: XMonad.Prompt.XPConfig
exitPromptConfig = promptConfig {bgColor = Colors.yellow}

projects :: [Project]
projects =
  [ Project
      {projectName = "Web", projectDirectory = "~/Downloads", projectStartHook = Just launchBrowser}
  , Project {projectName = "Mail", projectDirectory = "~/", projectStartHook = Just launchOutlook}
  , Project {projectName = "Slack", projectDirectory = "~/", projectStartHook = Just launchSlack}
  , Project {projectName = "Teams", projectDirectory = "~/", projectStartHook = Just launchTeams}
  , Project {projectName = "Misc1", projectDirectory = "~/", projectStartHook = Nothing}
  , Project {projectName = "Misc2", projectDirectory = "~/", projectStartHook = Nothing}
  , Project {projectName = "Misc3", projectDirectory = "~/", projectStartHook = Nothing}
  , Project {projectName = "Misc4", projectDirectory = "~/", projectStartHook = Nothing}
  , Project
      { projectName = "Xmo"
      , projectDirectory = "~/projects/xmo"
      , projectStartHook = Just $ runInShell "stack build && vim"
      }
  ]

projectIconOrName :: String -> String
projectIconOrName name =
  fromMaybe name $
  lookup
    name
    [ ("Web", "\xf0ac")
    , ("Xmo", "\xe777")
    , ("Misc1", "\xf005")
    , ("Misc2", "\xf005")
    , ("Misc3", "\xf005")
    , ("Misc4", "\xf005")
    , ("Mail", "\xf6ef")
    , ("Slack", "\xf9b0")
    , ("Teams", "\xf871")
    ]

centeredFloating :: ManageHook
centeredFloating = customFloating $ W.RationalRect (1 / 20) (1 / 20) (9 / 10) (9 / 10)

scratchpads :: NamedScratchpads
scratchpads =
  [ NS
      "term"
      "zsh -c '/home/david/.local/bin/scratchpad.sh'"
      (title =? "scratchpad")
      centeredFloating
  , NS "spotify" "spotify" (className =? "Spotify") centeredFloating
  -- , NS
  --     "calendar"
  --     "brave --app=https://teams.microsoft.com/_#/calendarv2"
  --     (appName =? "teams.microsoft.com" <&&> className =? "Brave-browser")
  --     centeredFloating
  ]

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D" -- fixes Java, which doesn't whitelist XMonad
  spawnOnce "/home/david/.fehbg"
  spawnOnce "/home/david/.screenlayout/default.sh"
  spawnOnce "picom -f &"
  spawnOnce "qmenu_registrar &"
  activateProject (head projects)

myEventHook :: Event -> X All
myEventHook = dynamicPropertyChange "WM_NAME" (className =? "Spotify" --> centeredFloating)

myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.9

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 =
  pure $ statusBarPropTo "_XMONAD_LOG_0" "/home/david/.local/bin/xmobar -x 0" (myXmobarPP 0)
barSpawner 1 =
  pure $ statusBarPropTo "_XMONAD_LOG_1" "/home/david/.local/bin/xmobar -x 1" (myXmobarPP 1)
barSpawner _ = mempty

myXmobarPP :: ScreenId -> X PP
myXmobarPP screen = do
  wsFormatter <- getWsFormatter
  return
    xmobarPP
      { ppTitle = xmobarColor Colors.cyan "" . shorten 50
      , ppCurrent = xmobarColor Colors.yellow "" . wrap "[" "]" . wsFormatter
      , ppVisible = wrap "(" ")" . wsFormatter
      , ppVisibleNoWindows = flip (.) wsFormatter <$> ppVisibleNoWindows def
      , ppHidden = ppHidden def . wsFormatter
      , ppHiddenNoWindows = ppHiddenNoWindows def . wsFormatter
      , ppSep = "  "
      , ppLayout = \s -> " <fn=1>" ++ s ++ "</fn>"
      , ppOrder = \(ws:l:_:tos:_) -> [l, ws, tos]
      , ppSort = mkWsSort getWsCompare
      , ppExtras = [xmobarColorL Colors.cyan "" $ shortenL 70 $ logTitleOnScreen screen]
      }
  where
    getWsFormatter = formatWs <$> getWsIndex
    formatWs index ws = (formatIndex . index) ws ++ projectIconOrName ws
    formatIndex = maybe "" (show . (+ 1))

launchBrowser :: X ()
launchBrowser = spawn "brave"

launchVim :: X ()
launchVim = runInShell "vim"

launchVimWiki :: X ()
launchVimWiki = runInShell "vim -c VimwikiIndex"

launchFileManager :: X ()
launchFileManager = runInShell "fm"

screenshotRect :: X ()
screenshotRect = spawn "ksnip -r"

screenshotWindow :: X ()
screenshotWindow = spawn "ksnip -a"

launchScreenshotApp :: X ()
launchScreenshotApp = spawn "ksnip"

launchClipmenu :: X ()
launchClipmenu = spawn "CM_LAUNCHER=rofi clipmenu"

-- run a command in the shell, keep shell running after command terminates
-- (assumes that shell initialisation ends with the line 'eval "$RUN"')
runInShell :: String -> X ()
runInShell cmd = do
  term <- asks (terminal . config)
  spawn $ "POWERLEVEL9K_INSTANT_PROMPT=off RUN='" ++ cmd ++ "' " ++ term

launchSiteAsApp :: String -> X ()
launchSiteAsApp url = spawn $ "brave --app=" ++ url

launchEvernote :: X ()
launchEvernote = launchSiteAsApp "http://www.evernote.com/Home.action"

launchOutlook :: X ()
launchOutlook = launchSiteAsApp "https://outlook.office.com/mail/inbox"

launchTeams :: X ()
launchTeams = launchSiteAsApp "https://teams.microsoft.com/"

launchSlack :: X ()
launchSlack = spawn "slack"

editConfigMenu :: X ()
editConfigMenu =
  editFileMenu
    [ ("Vim", "~/.vimrc")
    , ("Alacritty", "~/.config/alacritty/alacritty.yml")
    , ("Zsh", "~/.zshrc")
    , ("Picom", "~/.config/picom/picom.conf")
    , ("Rofi", "~/.config/rofi/theme.rasi")
    , ("Vifm", "~/.config/vifm/vifmrc")
    , ("SurfingKeys", "~/.config/.surfingkeys.js")
    , ("Xmonad", "~/projects/xmo/app/xmonad.hs")
    ]

edit :: String -> X ()
edit file = runInShell $ "vim " ++ file

editFileMenu :: [(String, String)] -> X ()
editFileMenu files = do
  file <- dmenuMap (M.fromList files)
  case file of
    Nothing -> return ()
    (Just f) -> edit f

addKeys :: XConfig a -> XConfig a
addKeys conf = addDescrKeys' ((modMask conf, xK_b), displayKeyMap) myKeys conf

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf =
  section
    "Launching and killing"
    [ ("M-S-<Return>", "Launch Terminal", spawn $ XMonad.terminal conf)
    , ("M-C-<Return>", "Launch Terminal scratchpad", namedScratchpadAction scratchpads "term")
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
    , ("M-f", "Launch File Manager", launchFileManager)
    , ("M-v", "Launch Vim", launchVim)
    , ("M-S-v", "Launch VimWiki", launchVimWiki)
    , ("M-;", "Screenshot rectangle", screenshotRect)
    , ("M-S-;", "Screenshot active window", screenshotWindow)
    , ("M-C-S-;", "Launch screenshot app", launchScreenshotApp)
    , ("M-z", "Launch spotify scratchpad", namedScratchpadAction scratchpads "spotify")
    , ("M-c", "Launch clipmenu", launchClipmenu)
    , ("M-i", "Choose config to edit", editConfigMenu)
    ] ++
  section
    "Changing layouts"
    [ ("M-<Space>", "Next Layout", sendMessage NextLayout)
    , ("M-S-<Space>", "Reset the layout", setLayout $ XMonad.layoutHook conf)
    , ("M-C-<Space>", "Flip layout horizontally", sendMessage $ Toggle REFLECTX)
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
    "Status bar"
    [ ("M-S-b", "Show/Hide the status bar", sendMessage ToggleStruts)
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
    [ ("M-S-q", "Quit", confirmPrompt exitPromptConfig "exit" $ io exitSuccess)
    , ("M-q", "Restart", spawn "xmonad --recompile && xmonad --restart")
    , ("M-<Backspace>", "System shutdown menu", spawn "/home/david/.local/bin/shutdownmenu.sh")
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
    [ ("M-'", "New random wallpaper", spawn "/home/david/.local/bin/random-wallpaper.sh")
    , ( "M-C-'"
      , "Save current wallpaper"
      , spawn "cp --backup=t /home/david/.wallpaper /home/david/.wallpapers")
    , ( "M-S-'"
      , "Random saved wallpaper"
      , spawn "feh --bg-scale --randomize /home/david/.wallpapers/")
    , ( "M-C-S-'"
      , "Select and manage wallpaper"
      , spawn
          "feh --action ';feh --bg-scale %f && cp %f /home/david/.wallpaper' /home/david/.wallpapers/")
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

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Zenity" --> doFloat
    , className =? "Yad" --> doFloat
    , className =? "Gimp" --> doFloat
    , title =? "Microsoft Teams Notification" --> doFloat
    , resource =? "desktop_window" --> doIgnore
    , resource =? "kdesktop" --> doIgnore
    ] <+>
  namedScratchpadManageHook scratchpads

monoIcon :: String
monoIcon = "\xe9e1"

vertIcon :: String
vertIcon = "\xe900"

horizIcon :: String
horizIcon = "\xea1b"
