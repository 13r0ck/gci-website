module Storage exposing
    ( Address
    , BtnOptions(..)
    , ContactDialogState
    , NavBarDisplay(..)
    , NavItem
    , Storage
    , changeUrl
    , contactEmail
    , contactMessage
    , contactName
    , contactPhone
    , contactUsBack
    , contactUsNext
    , fromJson
    , init
    , navBtnHover
    , navBtnUnHover
    , onChange
    , setContactUs
    )

import Browser.Navigation as Nav
import Char exposing (isDigit)
import Email as Email
import Html exposing (option)
import Json.Decode as Json exposing (Value, bool, field, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E exposing (bool, int, list, object, string)
import PhoneNumber
import PhoneNumber.Countries exposing (countryUS)
import Ports exposing (disableScrolling, load, save, setPhoneInputCursor)



-- STORAGE


type alias Storage =
    { navHoverTracker : List NavItem
    , openContactUs : Bool
    , contactDialogState : ContactDialogState
    }


type alias ContactDialogState =
    { name : Maybe String
    , nameError : Bool
    , email : Maybe String
    , emailError : Bool
    , phone : Maybe String
    , phoneError : Bool
    , message : Maybe String
    , messageError : Bool
    , currentPage : Int
    }


type alias Address =
    { street : String
    , city : String
    , phone : String
    , phoneLink : String
    , email : String
    , emailLink : String
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
    let
        us =
            storage.contactDialogState

        nullable : Maybe String -> E.Value
        nullable a =
            case a of
                Nothing ->
                    E.null

                Just str ->
                    E.string str
    in
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
        , ( "contactDialogState"
          , E.object
                [ ( "name", nullable us.name )
                , ( "nameError", E.bool us.nameError )
                , ( "email", nullable us.email )
                , ( "emailError", E.bool us.emailError )
                , ( "phone", nullable us.phone )
                , ( "phoneError", E.bool us.phoneError )
                , ( "message", nullable us.message )
                , ( "messageError", E.bool us.messageError )
                , ( "currentPage", E.int us.currentPage )
                ]
          )
        ]


decoder : Json.Decoder Storage
decoder =
    Json.map3 Storage
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
        (Json.field "contactDialogState" <|
            (Json.succeed ContactDialogState
                |> required "name" (Json.nullable Json.string)
                |> required "nameError" Json.bool
                |> required "email" (Json.nullable Json.string)
                |> required "emailError" Json.bool
                |> required "phone" (Json.nullable Json.string)
                |> required "phoneError" Json.bool
                |> required "message" (Json.nullable Json.string)
                |> required "messageError" Json.bool
                |> optional "currentPage" Json.int 0
            )
        )



-- Converting from JSON


fromJson : Value -> Storage
fromJson json =
    json
        |> Json.decodeValue decoder
        |> Result.withDefault init


init : Storage
init =
    Storage
        [ NavItem "WHO WE ARE" "#" False (Url "/whoweare")
        , NavItem "WHAT WE DO" "#" False (Url "/#boxes")
        , NavItem "NEWSROOM" "#" False (Url "/newsroom")
        , NavItem "CONTACT US" "#" False (SetContactUs True)
        ]
        False
        (ContactDialogState
            Nothing
            False
            Nothing
            False
            Nothing
            False
            Nothing
            False
            0
        )



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


setContactUs : String -> Storage -> Cmd msg
setContactUs state storage =
    { storage
        | openContactUs =
            if state == "True" then
                True

            else
                False
    }
        |> toJson
        |> save


contactUsNext : String -> Storage -> Cmd msg
contactUsNext _ storage =
    (case storage.contactDialogState.currentPage of
        0 ->
            if not (String.trim (Maybe.withDefault "" storage.contactDialogState.name) == "") then
                { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, nameError = False, name = Just (String.trim (Maybe.withDefault "" s.name)) }) }

            else
                { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | nameError = True, name = Just (String.trim (Maybe.withDefault "" s.name)) }) }

        1 ->
            if Email.isValid (Maybe.withDefault "" storage.contactDialogState.email) && validUSNumber (String.right 10 (String.filter isDigit (Maybe.withDefault "" storage.contactDialogState.phone))) then
                { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, phoneError = False, emailError = False }) }

            else if not (Email.isValid (Maybe.withDefault "" storage.contactDialogState.email)) then
                { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | emailError = True, phoneError = False }) }

            else if not (validUSNumber (String.right 10 (String.filter isDigit (Maybe.withDefault "" storage.contactDialogState.phone)))) then
                { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | phoneError = True }) }

            else
                { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, phoneError = False, emailError = False }) }

        2 ->
            if Maybe.withDefault "" storage.contactDialogState.message == "" then
                { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | messageError = True }) }

            else
                { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, messageError = False }) }

        _ ->
            { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1 }) }
    )
        |> toJson
        |> save


contactUsBack : String -> Storage -> Cmd msg
contactUsBack _ storage =
    if storage.contactDialogState.currentPage == 0 then
        Cmd.batch
            [ { storage | openContactUs = False, navHoverTracker = List.map (\b -> { b | hovered = False }) storage.navHoverTracker } |> toJson |> save
            , disableScrolling False
            ]

    else
        { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | currentPage = s.currentPage - 1 }) } |> toJson |> save


contactMessage : String -> Storage -> Cmd msg
contactMessage newMessage storage =
    (if storage.contactDialogState.messageError then
        { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | message = Just newMessage, messageError = newMessage == "" }) }

     else
        { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | message = Just newMessage }) }
    )
        |> toJson
        |> save


contactPhone : String -> Storage -> Cmd msg
contactPhone newPhone storage =
    if storage.contactDialogState.phoneError then
        Cmd.batch
            [ { storage
                | contactDialogState =
                    storage.contactDialogState
                        |> (\s ->
                                { s
                                    | phone =
                                        Just
                                            (if newPhone == "+1 ( " then
                                                ""

                                             else if String.length newPhone < String.length (Maybe.withDefault newPhone s.phone) then
                                                newPhone

                                             else
                                                prettyPhoneNumber newPhone
                                            )
                                    , phoneError = not (validUSNumber (String.right 10 (String.filter isDigit (prettyPhoneNumber newPhone))))
                                }
                           )
              }
                |> toJson
                |> save
            , case storage.contactDialogState.phone of
                Just p ->
                    setPhoneCursor p newPhone

                Nothing ->
                    Cmd.none
            ]

    else
        Cmd.batch
            [ { storage
                | contactDialogState =
                    storage.contactDialogState
                        |> (\s ->
                                { s
                                    | phone =
                                        Just
                                            (if newPhone == "+1 ( " then
                                                ""

                                             else if String.length newPhone < String.length (Maybe.withDefault newPhone s.phone) then
                                                newPhone

                                             else
                                                prettyPhoneNumber newPhone
                                            )
                                }
                           )
              }
                |> toJson
                |> save
            , case storage.contactDialogState.phone of
                Just p ->
                    setPhoneCursor p newPhone

                Nothing ->
                    Cmd.none
            ]


contactEmail : String -> Storage -> Cmd msg
contactEmail newEmail storage =
    (if storage.contactDialogState.emailError then
        { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | email = Just (String.trim newEmail), emailError = not (Email.isValid newEmail) }) }

     else
        { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | email = Just (String.trim newEmail) }) }
    )
        |> toJson
        |> save


contactName : String -> Storage -> Cmd msg
contactName newName storage =
    (if storage.contactDialogState.nameError then
        { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | name = Just newName, nameError = newName == "" }) }

     else
        { storage | contactDialogState = storage.contactDialogState |> (\s -> { s | name = Just newName }) }
    )
        |> toJson
        |> save



-- LISTENING FOR STORAGE UPDATES


onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> fromJson json |> fromStorage)



-- Helper Functions


prettyPhoneNumber : String -> String
prettyPhoneNumber number =
    let
        clean =
            String.filter isDigit (String.replace "+1" "" number)
    in
    case String.length clean of
        0 ->
            "+1 ("

        1 ->
            "+1 (" ++ clean

        2 ->
            "+1 (" ++ clean

        3 ->
            "+1 (" ++ clean ++ ")  "

        4 ->
            "+1 (" ++ String.left 3 clean ++ ")  " ++ String.right 1 clean

        5 ->
            "+1 (" ++ String.left 3 clean ++ ")  " ++ String.right 2 clean

        _ ->
            "+1 (" ++ String.left 3 clean ++ ")  " ++ String.slice 3 6 clean ++ " - " ++ String.slice 6 10 clean


setPhoneCursor : String -> String -> Cmd msg
setPhoneCursor oldPhone newPhone =
    let
        parse val =
            String.toList (String.filter isDigit (String.replace "+1" "" val))

        index a =
            a |> Tuple.first

        one a =
            a |> Tuple.second |> Tuple.first

        two a =
            a |> Tuple.second |> Tuple.second
    in
    -- creates List (index, (oldDigit, newDigit)) and filters for first change returning that number
    -- the first difference is what matters, so we just take the head and return the modified index
    case
        List.head
            (List.filterMap
                (\a ->
                    if not (one a == two a) then
                        Just (index a)

                    else
                        Nothing
                )
                (List.indexedMap Tuple.pair (List.map2 Tuple.pair (parse oldPhone) (parse newPhone)))
            )
    of
        Just i ->
            if String.length oldPhone > String.length newPhone then
                setPhoneInputCursor
                    (case i of
                        0 ->
                            4

                        1 ->
                            5

                        2 ->
                            6

                        3 ->
                            10

                        4 ->
                            11

                        5 ->
                            12

                        n ->
                            n + 10
                    )

            else
                setPhoneInputCursor
                    (case i of
                        0 ->
                            5

                        1 ->
                            6

                        2 ->
                            10

                        3 ->
                            11

                        4 ->
                            12

                        n ->
                            n + 11
                    )

        Nothing ->
            Cmd.none


validUSNumber : String -> Bool
validUSNumber number =
    if number == "" then
        True

    else
        PhoneNumber.valid
            { defaultCountry = countryUS
            , otherCountries = []
            , types = PhoneNumber.anyType
            }
            number


setHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setHovered id i data =
    if id == i then
        { data | hovered = True }

    else
        { data | hovered = False }


setUnHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setUnHovered _ _ data =
    { data | hovered = False }
