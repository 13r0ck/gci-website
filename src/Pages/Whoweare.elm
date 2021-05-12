module Pages.Whoweare exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Gen.Params.Whoweare exposing (Params)
import Html exposing (br, div, iframe)
import Html.Attributes exposing (attribute, class, id, property, src, style)
import Json.Encode as Encode
import Page
import Palette exposing (FontSize(..), black, fontSize, gciBlue, maxWidth, white)
import Ports exposing (disableScrolling)
import Request
import Shared exposing (Temp, ael, contactUs, footer, navbar)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage exposing (Storage)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update shared.storage
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { showVimeo : Bool
    , simpleBtnHoverTracker : List SimpleBtn
    }


type alias SimpleBtn =
    { id : Int
    , name : String
    , link : String
    , hovered : Bool
    , message : Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { showVimeo = False
      , simpleBtnHoverTracker = [ SimpleBtn 0 "Play" "#" False OpenVimeo ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NavBar (Storage -> Cmd Msg)
    | ContactUs (String -> Storage -> Cmd Msg) String
    | Footer (Storage -> Cmd Msg)
    | OpenVimeo
    | CloseVimeo
    | SimpleBtnHover Int
    | SimpleBtnUnHover Int


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        NavBar cmd ->
            ( model, cmd storage )

        ContactUs cmd str ->
            ( model, cmd str storage )

        Footer cmd ->
            ( model, cmd storage )

        OpenVimeo ->
            ( { model | showVimeo = True }, disableScrolling True )

        CloseVimeo ->
            ( { model | showVimeo = False }, disableScrolling False )

        SimpleBtnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setHovered id) model.simpleBtnHoverTracker }, Cmd.none )

        SimpleBtnUnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setUnHovered id) model.simpleBtnHoverTracker }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        h =
            shared.temp.height

        w =
            shared.temp.width

        device =
            shared.temp.device.class

        isPhone =
            device == Phone
    in
    { title = "GCI - Authorized Reverse Engineering IC Solutions for Obsolescence and High Temperature Environments"
    , attributes =
        [ inFront (navbar shared NavBar)
        , inFront
            (if shared.storage.openContactUs then
                contactUs shared ContactUs

             else
                none
            )
        , inFront
            (if model.showVimeo then
                vimeo shared

             else
                none
            )
        ]
    , element =
        column [ width fill, Region.mainContent, clip ]
            [ column [ width (fill |> maximum maxWidth), centerX, spacing 100 ]
                [ head shared model
                , column
                    [ paddingXY
                        (if isPhone then
                            10

                         else
                            toFloat w * 0.2 |> round
                        )
                        0
                    ]
                    [ mainText shared.temp ]
                ]
            , footer shared Footer
            ]
    }


head : Shared.Model -> Model -> Element Msg
head shared model =
    let
        simpleBtns =
            model.simpleBtnHoverTracker

        h =
            shared.temp.height

        w =
            shared.temp.width

        device =
            shared.temp.device.class

        isPhone =
            device == Phone

        scaleByHeight =
            w // h <= 16 // 9

        playBtn item =
            el
                [ width shrink
                , height fill
                , centerX
                , transparent model.showVimeo
                ]
                (row
                    [ Border.rounded 1000
                    , width
                        (px
                            (if item.hovered then
                                300

                             else
                                120
                            )
                        )
                    , height (px 120)
                    , centerX
                    , centerY
                    , Border.shadow { blur = 20, color = rgba 0 0 0 0.7, offset = ( 0, 0 ), size = 1 }
                    , Background.color gciBlue
                    , Font.color white
                    , fontSize device Xlg
                    , htmlAttribute <| class "backgroundStretch"
                    , Events.onMouseEnter (SimpleBtnHover 0)
                    , Events.onMouseLeave (SimpleBtnUnHover 0)
                    ]
                    (if item.hovered then
                        [ ael
                            (Animation.fromTo
                                { duration = 300
                                , options = []
                                }
                                [ P.opacity 0, P.x 10 ]
                                [ P.opacity 100, P.x 0 ]
                            )
                            [ Font.bold, centerX, padding 10 ]
                            (text "Play")
                        , ael
                            (Animation.fromTo
                                { duration = 300
                                , options = []
                                }
                                [ P.x -70 ]
                                [ P.x 0 ]
                            )
                            [ Font.family [ Font.typeface "icons" ], centerX ]
                            (text "\u{E801}")
                        ]

                     else
                        [ ael
                            (Animation.fromTo
                                { duration = 300
                                , options = []
                                }
                                [ P.x 50 ]
                                [ P.x 0 ]
                            )
                            [ Font.family [ Font.typeface "icons" ], centerX ]
                            (text "\u{E801}")
                        ]
                    )
                )
    in
    Input.button
        [ width fill
        , inFront
            (column
                [ fontSize device XXlg
                , Font.color white
                , Font.extraBold
                , alignBottom
                , padding
                    (if isPhone then
                        3

                     else
                        min 150 (toFloat w * 0.1) |> floor
                    )
                ]
                [ text "We Stop", text "Electronic", text "Obsolescence" ]
            )
        , inFront (row [ centerX, centerY ] (List.map playBtn (List.filter (\a -> a.id == 0) simpleBtns)))
        ]
        { onPress = Just OpenVimeo
        , label =
            image
                [ centerX
                , if scaleByHeight then
                    height (px h)

                  else
                    width (px w)
                ]
                { src = "/img/bourbon_street_video.jpg", description = "Click or tap to play Global circuit innovation's company video" }
        }


vimeo : Shared.Model -> Element Msg
vimeo shared =
    let
        w =
            shared.temp.width

        h =
            shared.temp.height

        videoWidth =
            let
                scale =
                    if isPhone then
                        0.95

                    else
                        0.75
            in
            if h > ((9 * (toFloat w * scale)) / 16 |> round) then
                toFloat w * scale |> floor

            else
                (16 * (toFloat h * 0.9)) / 9 |> round

        device =
            shared.temp.device.class

        isPhone =
            device == Phone
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
                , Events.onClick CloseVimeo
                ]
                none
            )
        ]
        (column
            [ width (px videoWidth)
            , centerX
            , centerY
            , Border.rounded 10
            , clip
            , Border.shadow { blur = 20, color = rgba 0 0 0 0.5, offset = ( 0, 0 ), size = 1 }
            ]
            [ el [ width fill, height fill, centerX, centerY ]
                (html <|
                    div
                        [ style "padding" "56.25% 0 0 0"
                        , style "position" "relative"
                        ]
                        [ iframe
                            [ style "position" "absolute"
                            , style "top" "0"
                            , style "left" "0"
                            , style "width" "100%"
                            , style "height" "100%"
                            , attribute "frameborder" "0"
                            , attribute "allow" "autoplay; fullscreen; picture-in-picture"
                            , property "allowfullscreen" (Encode.bool True)
                            , src "https://player.vimeo.com/video/322836491?autoplay=1&color=1d376c&title=0&byline=0&portrait=0"
                            ]
                            []
                        ]
                )
            ]
        )


mainText : Temp -> Element Msg
mainText temp =
    let
        device =
            temp.device.class

        isPhone =
            device == Phone

        w =
            temp.width
    in
    column
        [ width fill
        , spacing 25
        , padding 25
        , width fill
        , inFront
            (el [ htmlAttribute <| class "cornerClipTopLeft", width fill, height fill, Background.color black ]
                --, inFront (el [htmlAttribute <| class "cornerClipBottomRight", width fill, height fill, Background.color black]
                none
            )
        ]
        [ paragraph [ Font.extraLight, Region.heading 1, fontSize device Xlg ] [ text "Heading 1" ]
        , paragraph [ spacing 10, fontSize device Sm, Font.light ]
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Blandit cursus risus at ultrices mi tempus imperdiet. Ultricies lacus sed turpis tincidunt id aliquet risus feugiat. Vulputate sapien nec sagittis aliquam malesuada bibendum arcu vitae."
            , html <| br [] []
            , html <| br [] []
            , text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Pellentesque elit ullamcorper dignissim cras. Et netus et malesuada fames ac turpis egestas integer."
            ]
        ]


setHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setHovered id i data =
    if id == i then
        { data | hovered = True }

    else
        { data | hovered = False }


setUnHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setUnHovered _ _ data =
    { data | hovered = False }
