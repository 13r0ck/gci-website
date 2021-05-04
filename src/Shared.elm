module Shared exposing
    ( Flags
    , Model
    , Msg
    , acol
    , ael
    , arow
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
import Html.Attributes exposing (alt, attribute, autoplay, class, id, loop, src)
import Json.Decode as Json
import Palette exposing (black, gciBlue, gciBlueLight, maxWidth, warning, white)
import Ports exposing (recvScroll)
import Request exposing (Request)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage as Storage
    exposing
        ( BtnOptions(..)
        , NavBarDisplay(..)
        , NavItem
        , Storage
        , changeUrl
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
    ( { storage = Storage Storage.init.navHoverTracker (Storage.fromJson flags).openContactUs
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
                                setContactUs b
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
