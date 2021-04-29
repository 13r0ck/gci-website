port module Storage exposing (..)

-- Ports


port controlVideo : Bool -> Cmd msg


port recvScroll : (Int -> msg) -> Sub msg


port disableScrolling : Bool -> Cmd msg


port setPhoneInputCursor : Int -> Cmd msg
