port module Ports exposing (..)

import Json.Decode as Json



-- Ports


port controlVideo : Bool -> Cmd msg


port recvScroll : (Int -> msg) -> Sub msg


port disableScrolling : Bool -> Cmd msg


port setCursor : Int -> Cmd msg


port save : Json.Value -> Cmd msg


port load : (Json.Value -> msg) -> Sub msg


port onUrlChange : () -> Cmd msg


port showNav : (Bool -> msg) -> Sub msg
