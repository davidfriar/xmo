import qualified Colors.Gruvbox.Dark as Colors
import System.Environment (getArgs)
import Text.Printf (printf)
import Xmobar

main :: IO ()
main = do
  screen <- getScreen
  xmobar (config screen)

getScreen :: IO Int
getScreen = scr <$> getArgs
  where
    scr ["-x", s] = read s
    scr _ = 0

config :: Int -> Config
config scr =
  defaultConfig
    { font = "xft:Noto Sans Nerd Font:size=11,xft:Noto Sans:size=11"
    , additionalFonts = ["xft:feathericons:size=12"]
    , borderColor = Colors.backgroundHighlight
    , border = TopB
    , bgColor = Colors.background
    , fgColor = Colors.text
    , position = OnScreen scr $ BottomW L 100
    , commands =
        [ Run $
          Cpu
            (args
               [ ("-t", icon CPU ++ "<total>%")
               , ("-L", "3")
               , ("-H", "50")
               , ("--normal", Colors.green)
               , ("--high", Colors.red)
               ])
            100
        , Run $ Memory ["-t", icon MEM ++ "<usedratio>%"] 100
        , Run $ Date "%H:%M %A%e %B" "date" 600
        -- , Run StdinReader
        , Run $ NamedXPropertyLog ("_XMONAD_LOG_" ++ show scr) "XMonadLog"
        -- , Run $
        --   Wireless
        --     "wlo1"
        --     (args [("-t", icon WIFI ++ " <qualitybar>"), ("-f", "◦"), ("-b", " "), ("-W", "5")])
        --     10
        , Run $
          CoreTemp
            (args
               [ ("-t", icon TEMP ++ "<core0>C")
               , ("-L", "40")
               , ("-H", "60")
               , ("-l", Colors.blue)
               , ("-n", Colors.green)
               , ("-h", Colors.red)
               ])
            50
        , Run $
          Volume
            "default"
            "Master"
            (args [("-t", "<status> <volumebar>"), ("-f", "◦"), ("-b", " "), ("-W", "10")] ++
             ["--"] ++
             args
               [ ("-O", icon (SPKR ON))
               , ("-c", Colors.text)
               , ("-o", icon (SPKR OFF))
               , ("-C", Colors.text)
               ])
            10
        , Run $ DiskU [("/", icon DISK ++ "<used>")] [] 600
        , Run $ Com "/home/david/.local/bin/weather.sh" [] "weather" 3000
        ]
    , template = myTemplate
    }

myTemplate :: String
myTemplate = mkTemplate left middle right
  where
    -- left = item "StdinReader"
    left = item "XMonadLog"
    middle =
      concat
        [ button "cpu" (runInTerm False "gotop -l mylayout")
        , button "coretemp" (runInTerm False "bashtop")
        , button "memory" (runInTerm False "free -h -s 60")
        , button "disku" (runInTerm False "ncdu")
        -- , button "wlo1" (runInTerm False "networkmanager_dmenu")
        , button "default:Master" "pavucontrol"
        ]
    right = colored Colors.yellow (item "date") ++ button "weather" (runInTerm True "wego -f emoji")

mkTemplate :: String -> String -> String -> String
mkTemplate left middle right = concat [left, lsep, middle, rsep, right]
  where
    lsep = take 1 (alignSep defaultConfig)
    rsep = drop 1 (alignSep defaultConfig)

item :: String -> String
item s = sep ++ s ++ sep ++ " "
  where
    sep = sepChar defaultConfig

button :: String -> String -> String
button name cmd = printf "<action=`%s`>%s</action>" cmd (item name)

runInTerm :: Bool -> String -> String
runInTerm False = printf "alacritty -e %s"
runInTerm True = printf "RUN='%s' alacritty"

colored :: String -> String -> String
colored = printf "<fc=%s>%s</fc>"

args :: [(String, String)] -> [String]
args xs = concat [[a, b] | (a, b) <- xs]

data Status
  = ON
  | OFF

data Icon
  = CPU
  | MEM
  | TEMP
  | DISK
  | WIFI
  | BATT Status
  | SPKR Status

icon :: Icon -> String
icon i = "<fn=1>" ++ toChar i ++ "</fn>"
  where
    toChar CPU = "\xe951"
    toChar MEM = "\xe9ad"
    toChar TEMP = "\xe9eb"
    toChar DISK = "\xe955"
    toChar WIFI = "\xea0f"
    toChar (BATT ON) = "\xe91e"
    toChar (BATT OFF) = "\xe91d"
    toChar (SPKR ON) = "\xea0c"
    toChar (SPKR OFF) = "\xea0d"
