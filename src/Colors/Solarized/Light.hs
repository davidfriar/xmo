module Colors.Solarized.Light
  ( yellow
  , orange
  , red
  , magenta
  , violet
  , blue
  , cyan
  , green
  , background
  , backgroundHighlight
  , comments
  , neutral
  , text
  , emphasizedText
  ) where

import Colors.Solarized.Base

background :: String
background = base3

backgroundHighlight :: String
backgroundHighlight = base2

comments :: String
comments = base1

neutral :: String
neutral = base0

text :: String
text = base00

emphasizedText :: String
emphasizedText = base01
