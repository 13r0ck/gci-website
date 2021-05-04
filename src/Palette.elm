module Palette exposing (black, gciBlue, gciBlueLight, maxWidth, warning, white)

import Element exposing (..)


maxWidth : number
maxWidth =
    2000


white : Color
white =
    rgb 1 1 1


warning : Color
warning =
    rgb255 204 51 51


black : Color
black =
    rgb 0 0 0


gciBlue : Color
gciBlue =
    rgb255 29 55 108


gciBlueLight : Color
gciBlueLight =
    rgb255 59 85 138
