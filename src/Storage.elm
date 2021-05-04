module Storage exposing
    ( BtnOptions(..)
    , NavBarDisplay(..)
    , NavItem
    , Storage
    , changeUrl
    , fromJson
    , init
    , navBtnHover
    , navBtnUnHover
    , onChange
    , setContactUs
    )

import Browser.Navigation as Nav
import Json.Decode as Json exposing (Value, bool, field, int, list, string)
import Json.Encode as E exposing (bool, int, list, object, string)
import Ports exposing (load, save)



-- STORAGE


type alias Storage =
    { navHoverTracker : List NavItem
    , openContactUs : Bool
    }


type NavBarDisplay
    = Enter
    | Show
    | Hide


type alias NavItem =
    { name : String
    , link : String
    , hovered : Bool
    , onClick : BtnOptions
    }


type BtnOptions
    = Url String
    | SetContactUs Bool



-- Converting to JSON


toJson : Storage -> Value
toJson storage =
    E.object
        [ ( "navbar"
          , E.list object
                (List.map
                    (\b ->
                        [ ( "name", E.string b.name )
                        , ( "link", E.string b.link )
                        , ( "hovered", E.bool b.hovered )
                        , ( "onClick"
                          , E.string
                                (case b.onClick of
                                    Url s ->
                                        s

                                    SetContactUs True ->
                                        "contactOpened"

                                    SetContactUs False ->
                                        "contactClosed"
                                )
                          )
                        ]
                    )
                    storage.navHoverTracker
                )
          )
        , ( "openContactUs", E.bool storage.openContactUs )
        ]


decoder : Json.Decoder Storage
decoder =
    Json.map2 Storage
        (Json.field "navbar"
            (Json.list
                (Json.map4 NavItem
                    (Json.field "name" Json.string)
                    (Json.field "link" Json.string)
                    (Json.field "hovered" Json.bool)
                    (Json.field "onClick" Json.string
                        |> Json.andThen
                            (\s ->
                                case s of
                                    "contactOpened" ->
                                        Json.succeed (SetContactUs True)

                                    "contactClosed" ->
                                        Json.succeed (SetContactUs False)

                                    str ->
                                        Json.succeed (Url str)
                            )
                    )
                )
            )
        )
        (Json.field "openContactUs" Json.bool)



-- Converting from JSON


fromJson : Value -> Storage
fromJson json =
    json
        |> Json.decodeValue decoder
        |> Result.withDefault init


init : Storage
init =
    Storage
        [ NavItem "WHO WE ARE" "#" False (Url "/")
        , NavItem "WHAT WE DO" "#" False (Url "/#boxes")
        , NavItem "NEWSROOM" "#" False (Url "/newsroom")
        , NavItem "CONTACT US" "#" False (SetContactUs True)
        ]
        False



-- Updating storage


navBtnHover : Int -> Storage -> Cmd msg
navBtnHover id storage =
    { storage | navHoverTracker = List.indexedMap (setHovered id) storage.navHoverTracker }
        |> toJson
        |> save


navBtnUnHover : Int -> Storage -> Cmd msg
navBtnUnHover id storage =
    { storage | navHoverTracker = List.indexedMap (setUnHovered id) storage.navHoverTracker }
        |> toJson
        |> save


changeUrl : String -> Storage -> Cmd msg
changeUrl url storage =
    Cmd.batch
        [ { storage | navHoverTracker = List.map (\t -> { t | hovered = False }) storage.navHoverTracker } |> toJson |> save
        , Nav.load url
        ]


setContactUs : Bool -> Storage -> Cmd msg
setContactUs state storage =
    { storage | openContactUs = state }
        |> toJson
        |> save



-- LISTENING FOR STORAGE UPDATES


onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> fromJson json |> fromStorage)



-- Helper Functions


setHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setHovered id i data =
    if id == i then
        { data | hovered = True }

    else
        { data | hovered = False }


setUnHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setUnHovered _ _ data =
    { data | hovered = False }
