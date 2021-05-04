module Shared exposing
    ( Flags
    , Model
    , Msg
    , acol
    , ael
    , arow
    , contactUs
    , init
    , navbar
    , subscriptions
    , update
    )

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (innerShadow, rounded, shadow)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (a, br, video)
import Html.Attributes exposing (alt, attribute, autoplay, class, id, loop, src)
import Html.Events
import Json.Decode as Json
import Palette exposing (black, gciBlue, gciBlueLight, maxWidth, warning, white)
import Ports exposing (recvScroll)
import Request exposing (Request)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage as Storage
    exposing
        ( Address
        , BtnOptions(..)
        , ContactDialogState
        , NavBarDisplay(..)
        , NavItem
        , Storage
        , changeUrl
        , contactUsNext
        , navBtnHover
        , navBtnUnHover
        , setContactUs
        )


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage
    , temp : Temp
    }


type alias Temp =
    { scrolledDistance : Int
    , navbarDisplay : NavBarDisplay
    }


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    let
        _ =
            Debug.log "cds" (Storage.fromJson flags).contactDialogState
    in
    ( { storage =
            Storage.fromJson flags
                |> (\f -> Storage Storage.init.navHoverTracker f.openContactUs f.contactDialogState)
      , temp = { scrolledDistance = 0, navbarDisplay = Show }
      }
    , Cmd.none
    )


type Msg
    = StorageUpdated Storage
    | Scrolled Int


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        Scrolled distance ->
            ( { model
                | temp =
                    model.temp
                        |> (\t ->
                                { t
                                    | scrolledDistance = distance
                                    , navbarDisplay =
                                        if distance > model.temp.scrolledDistance then
                                            Hide

                                        else
                                            Enter
                                }
                           )
              }
            , Cmd.none
            )

        StorageUpdated storage ->
            ( { model | storage = storage }
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Storage.onChange StorageUpdated
        , recvScroll Scrolled
        ]



-- hook in for elm-simple-animation


animatedUi : (List (Attribute msg) -> children -> Element msg) -> Animation -> List (Attribute msg) -> children -> Element msg
animatedUi =
    Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }


ael : Animation -> List (Element.Attribute msg) -> Element msg -> Element msg
ael =
    animatedUi Element.el


arow : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
arow =
    animatedUi Element.row


acol : Animation -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
acol =
    animatedUi Element.column



-- Helper Functions


navbar : List NavItem -> NavBarDisplay -> ((Storage -> Cmd msg) -> b) -> Element b
navbar animationTracker display msgCommand =
    let
        navbarBtn ( id, item ) =
            row
                [ height (px 80)
                , pointer
                , paddingXY 80 0
                , inFront
                    (row
                        [ htmlAttribute <|
                            class
                                (if item.hovered then
                                    "wipe_point_active"

                                 else
                                    "wipe_point"
                                )
                        , width fill
                        , height fill
                        , Background.color white
                        ]
                        [ el [ centerX, centerY, Font.color black ] (text item.name) ]
                    )
                , behindContent
                    (row
                        [ width fill
                        , height fill
                        , Background.color gciBlue
                        , innerShadow { offset = ( 0, 0 ), size = 0.15, blur = 8, color = rgb255 13 25 48 }
                        ]
                        [ el [ centerX, centerY, Font.color white ] (text item.name) ]
                    )
                , Events.onClick
                    (msgCommand
                        (case item.onClick of
                            Url s ->
                                changeUrl s

                            SetContactUs b ->
                                setContactUs
                                    (if b then
                                        "True"

                                     else
                                        "False"
                                    )
                        )
                    )
                , Events.onMouseEnter (msgCommand (navBtnHover id))
                , Events.onMouseLeave (msgCommand (navBtnUnHover id))
                ]
                []

        spacer =
            column
                [ width fill
                , height fill
                , Background.color (rgb 1 1 1)
                ]
                []

        logo =
            el
                [ height (px 80)
                , Background.color white
                , pointer
                , Events.onClick
                    (msgCommand (changeUrl "/"))
                ]
                (image
                    [ height (px 50)
                    , paddingXY 24 0
                    , centerX
                    , centerY
                    ]
                    { src = "/img/logo_sans_ring.svg", description = "Global Circuit Inovations" }
                )
    in
    arow
        (case display of
            Show ->
                Animation.empty

            Hide ->
                Animation.fromTo
                    { duration = 300
                    , options = [ Animation.easeIn ]
                    }
                    [ P.y 0 ]
                    [ P.y -100 ]

            Enter ->
                Animation.fromTo
                    { duration = 300
                    , options = [ Animation.easeIn ]
                    }
                    [ P.y -100 ]
                    [ P.y 0 ]
        )
        [ width fill
        , height shrink
        , Font.family [ Font.sansSerif ]
        , Font.size 15
        , Region.navigation
        , shadow { offset = ( 0, 0 ), size = 0.15, blur = 5, color = black }
        ]
        [ column [ width (fill |> maximum maxWidth), centerX ]
            [ row [ width fill, spaceEvenly ]
                (List.concat
                    [ [ logo, spacer ]
                    , List.map navbarBtn (List.indexedMap Tuple.pair animationTracker)
                    ]
                )
            ]
        ]


contactUs : ContactDialogState -> Address -> ((String -> Storage -> Cmd msg) -> String -> b) -> Element b
contactUs state address msgCommand =
    let
        break =
            html <| br [] []

        contactDialog =
            column [ width fill, height fill ]
                [ case state.currentPage of
                    0 ->
                        column
                            [ width fill, height (px 250), Font.light, spacing 25, htmlAttribute <| class "backgroundGrow" ]
                            [ row [ width fill, alignTop, padding 20 ]
                                [ el [ Font.size 35, centerX ] (text "Nice to meet you! ")
                                , el [ Font.size 45, centerX ] (text "👋")
                                ]
                            , if state.nameError then
                                el [ Font.size 25, centerX, Font.color warning ] (text "Please tell us who you are.")

                              else
                                el [ Font.size 25, centerX ] (text "Can we get a name?")
                            , Input.text
                                [ rounded 100
                                , width (px 400)
                                , centerX
                                , centerY
                                , onEnter (msgCommand Storage.contactUsNext "")
                                , Border.color
                                    (if state.nameError then
                                        warning

                                     else
                                        gciBlue
                                    )
                                , Border.width 2
                                , Font.center
                                ]
                                { onChange = msgCommand Storage.contactName
                                , text = Maybe.withDefault "" state.name
                                , placeholder = Just (Input.placeholder [ Font.center ] (text "First & Last"))
                                , label = Input.labelHidden "Name"
                                }
                            ]

                    1 ->
                        column
                            [ width fill, height (px 300), padding 30, Font.light, spacing 25, htmlAttribute <| class "backgroundGrow" ]
                            [ row [ width fill, alignTop ]
                                [ paragraph [ Font.size 35, centerX, Font.center ]
                                    [ case String.trim (Maybe.withDefault "" state.name) of
                                        "" ->
                                            text "Thanks for reaching out!"

                                        n ->
                                            case List.head (String.split " " n) of
                                                Just first_name ->
                                                    case String.uncons first_name of
                                                        Just ( first, tail ) ->
                                                            text ("Thanks for reaching out " ++ (String.toUpper (String.fromChar first) ++ tail) ++ "!")

                                                        Nothing ->
                                                            text "Thanks for reaching out!"

                                                Nothing ->
                                                    text "Thanks for reaching out!"
                                    ]
                                ]
                            , if state.emailError then
                                el [ Font.size 25, centerX, Font.color warning ] (text "That email seems wrong.")

                              else if state.phoneError then
                                el [ Font.size 25, centerX, Font.color warning ] (text "That phone number seems wrong")

                              else
                                el [ Font.size 25, centerX ] (text "How can we contact you?")
                            , Input.email
                                [ rounded 100
                                , width (px 400)
                                , centerX
                                , Font.center
                                , onEnter (msgCommand Storage.contactUsNext "")
                                , Border.color
                                    (if state.emailError then
                                        warning

                                     else
                                        gciBlue
                                    )
                                , Border.width 2
                                ]
                                { onChange = msgCommand Storage.contactEmail
                                , text = Maybe.withDefault "" state.email
                                , placeholder = Just (Input.placeholder [ Font.center ] (text "name@exmaple.com"))
                                , label = Input.labelHidden "Email"
                                }
                            , Input.text
                                [ rounded 100
                                , width (px 400)
                                , centerX
                                , Font.center
                                , onEnter (msgCommand Storage.contactUsNext "")
                                , htmlAttribute <| id "phoneInput"
                                , Border.color
                                    (if state.phoneError then
                                        warning

                                     else
                                        gciBlue
                                    )
                                , Border.width 2
                                ]
                                { onChange = msgCommand Storage.contactPhone
                                , text = Maybe.withDefault "" state.phone
                                , placeholder = Just (Input.placeholder [ Font.center ] (text "(123) 456 - 7890"))
                                , label = Input.labelHidden "Phone Number"
                                }
                            ]

                    2 ->
                        column
                            [ width fill, height (px 530), padding 30, Font.light, spacing 25, htmlAttribute <| class "backgroundGrow" ]
                            [ row [ width fill, alignTop ]
                                [ if state.messageError then
                                    el [ Font.size 35, centerX, Font.color warning ] (text "Use your words please!")

                                  else
                                    el [ Font.size 35, centerX ] (text "What can we do for you?")
                                ]
                            , Input.multiline
                                [ rounded 20
                                , width (px 500)
                                , height (fill |> maximum 400)
                                , centerX
                                , Border.color
                                    (if state.messageError then
                                        warning

                                     else
                                        gciBlue
                                    )
                                , Border.width 2
                                ]
                                { onChange = msgCommand Storage.contactMessage
                                , text = Maybe.withDefault "" state.message
                                , placeholder =
                                    Just
                                        (Input.placeholder []
                                            (paragraph []
                                                [ text "Hopefully you need High Tech solutions."
                                                , html <| br [] []
                                                , text "Last time we did interior painting it didn't end well ..."
                                                ]
                                            )
                                        )
                                , spellcheck = True
                                , label = Input.labelHidden "Message"
                                }
                            ]

                    3 ->
                        column
                            [ width fill, height (px 100), Font.light, htmlAttribute <| class "backgroundGrow" ]
                            [ row [ width fill, alignTop ]
                                [ el [ Font.size 35, centerX, centerY ] (text "Sent!")
                                , image [ width (px 150), centerX, centerY, padding 10 ] { src = "/img/f16-sticker.png", description = "F-16 sticker" }
                                ]
                            , el [ Font.size 25, centerX ] (paragraph [ Font.center ] [ text "We will reach back out to ", html <| br [] [], text (Maybe.withDefault "you" state.email ++ " soon!") ])
                            ]

                    _ ->
                        row [] []
                , row [ width fill, padding 15, alignBottom ]
                    (if state.currentPage < 3 then
                        [ Input.button
                            [ alignBottom
                            , alignLeft
                            , paddingXY 30 10
                            , rounded 100
                            , Font.color gciBlue
                            , Border.color gciBlue
                            , Border.width 2
                            , mouseOver [ Border.color gciBlueLight, Font.color gciBlueLight ]
                            ]
                            { onPress = Just (msgCommand Storage.contactUsBack ""), label = text "Back" }
                        , Input.button
                            [ alignBottom
                            , alignRight
                            , paddingXY 30 10
                            , rounded 100
                            , Background.color gciBlue
                            , Font.bold
                            , Font.color white
                            , Border.color gciBlue
                            , mouseOver [ Border.color gciBlueLight, Background.color gciBlueLight ]
                            , Border.width 2
                            ]
                            { onPress = Just (msgCommand Storage.contactUsNext ""), label = text "Next" }
                        ]

                     else
                        [ Input.button
                            [ alignBottom
                            , paddingXY 100 10
                            , rounded 100
                            , centerX
                            , Background.color gciBlue
                            , Font.bold
                            , Font.color white
                            , Border.color gciBlue
                            , mouseOver [ Border.color gciBlueLight, Background.color gciBlueLight ]
                            , Border.width 2
                            ]
                            { onPress = Just (msgCommand Storage.setContactUs "False"), label = text "Close" }
                        ]
                    )
                , paragraph
                    [ alignLeft
                    , Font.center
                    , centerY
                    , centerX
                    , padding 10
                    , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                    ]
                    [ text address.street
                    , break
                    , text address.city
                    , break
                    , link [ paddingXY 10 0 ] { url = address.phoneLink, label = text address.phone }
                    , text "|"
                    , link [ paddingXY 10 0 ] { url = address.emailLink, label = text address.email }
                    ]
                ]
    in
    el
        [ width fill
        , height fill
        , htmlAttribute <| class "point_enter_down_long"
        , behindContent
            (el
                [ width fill
                , height fill
                , Background.gradient
                    { angle = degrees 165
                    , steps = [ rgba255 87 83 78 0.7, rgba255 17 24 39 0.9 ]
                    }
                , Events.onClick (msgCommand Storage.setContactUs "False")
                ]
                none
            )
        ]
        (column
            [ Background.color white
            , width (px 600)
            , height (px 600)
            , centerX
            , centerY
            , Border.shadow { blur = 20, color = rgb 0.25 0.25 0.3, offset = ( 0, 0 ), size = 1 }
            , rounded 25
            , clip
            , inFront
                (row [ padding 20, alignRight ]
                    [ Input.button
                        [ alignRight
                        , Font.family [ Font.typeface "icons" ]
                        , Font.size 50
                        , pointer
                        , Font.color white
                        , mouseOver [ Font.color warning ]
                        ]
                        { onPress = Just (msgCommand Storage.setContactUs "False"), label = text "\u{E800}" }
                    ]
                )
            ]
            [ image
                [ width fill
                , height (fillPortion 3)
                , clip
                ]
                { src = "/img/building.png", description = "Picutre of GCI's building" }
            , el [ width fill, height (fillPortion 5) ] contactDialog
            ]
        )


onEnter : msg -> Element.Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Json.field "key" Json.string
                |> Json.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.succeed msg

                        else
                            Json.fail "Not the enter key"
                    )
            )
        )
