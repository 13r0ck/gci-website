port module Ports exposing (..)

import Json.Decode as Json



-- Ports
port waitForId : String -> Cmd msg

port idLoaded : (String -> msg) -> Sub msg

port idFailed : (String -> msg) -> Sub msg

port controlVideo : Bool -> Cmd msg


port recvScroll : (Int -> msg) -> Sub msg


port disableScrolling : Bool -> Cmd msg


port setCursor : Int -> Cmd msg


port save : Json.Value -> Cmd msg


port load : (Json.Value -> msg) -> Sub msg


port onUrlChange : () -> Cmd msg


port showNav : (Bool -> msg) -> Sub msg
