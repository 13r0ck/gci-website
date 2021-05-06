module Palette exposing (FontSize(..), black, fontSize, gciBlue, gciBlueLight, maxWidth, warning, white)

import Element exposing (..)
import Element.Font as Font


type FontSize
    = Xsm
    | Sm
    | Md
    | Lg
    | Xlg


fontSize : DeviceClass -> FontSize -> Attr decorative msg
fontSize device size =
    Font.size
        (case device of
            Phone ->
                case size of
                    Xsm ->
                        13

                    Sm ->
                        20

                    Md ->
                        25

                    Lg ->
                        38

                    Xlg ->
                        45

            Tablet ->
                case size of
                    Xsm ->
                        13

                    Sm ->
                        20

                    Md ->
                        25

                    Lg ->
                        38

                    Xlg ->
                        45

            Desktop ->
                case size of
                    Xsm ->
                        18

                    Sm ->
                        25

                    Md ->
                        30

                    Lg ->
                        43

                    Xlg ->
                        50

            BigDesktop ->
                case size of
                    Xsm ->
                        18

                    Sm ->
                        25

                    Md ->
                        30

                    Lg ->
                        43

                    Xlg ->
                        50
        )


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
