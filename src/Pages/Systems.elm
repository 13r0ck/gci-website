module Pages.Systems exposing (Model, Msg, page)

import Browser.Dom exposing (Viewport)
import Debug exposing (toString)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Gen.Params.Systems exposing (Params)
import Html exposing (br, div, iframe)
import Html.Attributes exposing (attribute, class, id, property, src, style)
import Json.Encode as Encode
import Page
import Pages.Home_ exposing (AnimationState, When(..), onScreenItemtoCmd, updateElement)
import Palette exposing (FontSize(..), black, fontSize, gciBlue, maxWidth, warning, white)
import Ports exposing (disableScrolling, recvScroll)
import Request
import Shared exposing (Temp, acol, ael, contactUs, footer, navbar)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage exposing (Storage)
import Task
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
    , animationTracker : Dict String AnimationState
    , subTexts : List SubText
    }


type alias SubText =
    { id : Int
    , title : String
    , image : String
    , text : String
    }


type alias SimpleBtn =
    { id : Int
    , name : String
    , link : String
    , hovered : Bool
    , message : Maybe Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { showVimeo = False
      , simpleBtnHoverTracker =
            [ SimpleBtn 0 "Play" "#" False (Just OpenVimeo)
            , SimpleBtn 1 "What we do" "/#whatwedo" False Nothing
            , SimpleBtn 2 "Contact Us" "" False (Just OpenContactUs)
            , SimpleBtn 3 "Technical Papers" "/technical" False Nothing
            ]
      , animationTracker =
            Dict.fromList
                [ ( "mainText", AnimationState (PercentOfViewport 40) False )
                , ( "bottomButtons", AnimationState (PercentOfViewport 40) False )
                , ( "1", AnimationState (PercentOfViewport 20) False )
                , ( "2", AnimationState (PercentOfViewport 40) False )
                , ( "3", AnimationState (PercentOfViewport 40) False )
                ]
      , subTexts =
            [ SubText 1 "Sub text" "/img/subtext1.jpg" "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut."
            , SubText 2 "Sub text" "/img/subtext2.jpg" "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut."
            , SubText 3 "Sub text" "/img/subtext3.jpg" "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut."
            ]
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
    | Scrolled Int
    | GotElement String (Result Browser.Dom.Error Browser.Dom.Element)
    | OpenContactUs


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

        Scrolled _ ->
            ( model
            , Cmd.batch
                (List.map animationTrackerToCmd (List.filter (\( _, v ) -> v.shouldAnimate == False) (Dict.toList model.animationTracker)))
            )

        GotElement id element ->
            case element of
                Ok e ->
                    ( { model | animationTracker = Dict.fromList (List.map (updateElement id e) (Dict.toList model.animationTracker)) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        OpenContactUs ->
            ( model, Storage.setContactUs "True" storage )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    recvScroll Scrolled



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

        isTablet =
            device == Tablet

        isMobile =
            isPhone || isTablet

        subtext item =
            let
                img =
                    el
                        [ width (fillPortion 2)
                        , clip
                        , centerY
                        , Border.rounded 10
                        , inFront
                            (el
                                [ width fill
                                , height fill
                                , Border.innerShadow { blur = 18, color = rgba 0 0 0 0.3, offset = ( 1, 8 ), size = 8 }
                                ]
                                none
                            )
                        ]
                        (image
                            [ centerX
                            , centerY
                            , width fill
                            ]
                            { src = item.image, description = item.title }
                        )

                content =
                    paragraph [ width fill, fontSize device Sm ] [ text item.text ]
            in
            acol
                (if shouldAnimate (toString item.id) model then
                    Animation.fromTo
                        { duration = 500
                        , options = []
                        }
                        [ P.opacity 0, P.y 100 ]
                        [ P.opacity 100, P.y 0 ]

                 else
                    Animation.empty
                )
                [ width fill, height fill, spacing 20, htmlAttribute <| id (toString item.id), transparent (not (shouldAnimate (toString item.id) model)) ]
                [ el [ Region.heading 3, Font.extraLight, fontSize device Lg ] (text item.title)
                , (if isMobile then
                    column

                   else
                    row
                  )
                    [ width fill, spacing 20 ]
                    (if modBy 2 item.id == 0 || isMobile then
                        [ img, content ]

                     else
                        [ content, img ]
                    )
                ]
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
            [ column [ width (fill |> maximum maxWidth), centerX, spacing 50 ]
                [ head shared model
                , column
                    [ paddingXY
                        (if isPhone then
                            10

                         else
                            toFloat w * 0.2 |> round
                        )
                        0
                    , width (fill |> maximum maxWidth)
                    , spacing 100
                    ]
                    ([ mainText shared.temp (shouldAnimate "mainText" model) ]
                        ++ List.map subtext model.subTexts
                    )
                , bottomButtons shared (List.filter (\b -> b.id > 0) model.simpleBtnHoverTracker) (shouldAnimate "bottomButtons" model)
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
            (link [ width fill, height fill ]
                { url = "/obsolescence#mainText"
                , label =
                    el
                        [ width fill
                        , height fill
                        , Background.gradient
                            { angle = degrees 165
                            , steps = [ rgba255 87 83 78 0.7, rgba255 17 24 39 0.9 ]
                            }
                        , Events.onClick CloseVimeo
                        ]
                        none
                }
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


mainText : Temp -> Bool -> Element Msg
mainText temp animateSelf =
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
            (el [ htmlAttribute <| class "clip_top", width fill, height fill, Background.color gciBlue ]
                none
            )
        , inFront
            (el
                [ htmlAttribute <|
                    class
                        (if animateSelf then
                            "animate_clip_bottom"

                         else
                            "clip_bottom"
                        )
                , width fill
                , height fill
                , Background.color gciBlue
                ]
                none
            )
        , inFront
            (el
                [ htmlAttribute <|
                    class
                        (if animateSelf then
                            "animate_clip_cover"

                         else
                            "clip_cover"
                        )
                , width fill
                , height fill
                , Background.color white
                ]
                none
            )
        ]
        [ paragraph [ Font.extraLight, Region.heading 1, fontSize device Xlg ] [ text "Heading 1" ]
        , paragraph [ spacing 10, fontSize device Sm, Font.light, htmlAttribute <| id "mainText" ]
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Blandit cursus risus at ultrices mi tempus imperdiet. Ultricies lacus sed turpis tincidunt id aliquet risus feugiat. Vulputate sapien nec sagittis aliquam malesuada bibendum arcu vitae."
            , html <| br [] []
            , html <| br [] []
            , text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Pellentesque elit ullamcorper dignissim cras. Et netus et malesuada fames ac turpis egestas integer."
            ]
        ]


bottomButtons : Shared.Model -> List SimpleBtn -> Bool -> Element Msg
bottomButtons shared btns animateSelf =
    let
        h =
            shared.temp.height

        w =
            shared.temp.width

        device =
            shared.temp.device.class

        isPhone =
            device == Phone

        isTablet =
            device == Tablet

        isMobile =
            isPhone || isTablet

        btn item =
            let
                attr =
                    [ centerX
                    , Border.width 5
                    , paddingXY 20 10
                    , Border.rounded 10
                    , fontSize device Md
                    , Font.color gciBlue
                    , Font.bold
                    , htmlAttribute <| class "background_transition"
                    , Border.color gciBlue
                    , Font.center
                    , inFront
                        (el
                            [ htmlAttribute <|
                                class
                                    (if item.hovered then
                                        "point_enter_down"

                                     else
                                        "point_leave_up"
                                    )
                            , centerX
                            , centerY
                            , paddingXY 22 10
                            , fontSize device Md
                            , Font.color white
                            , Border.rounded 5
                            , Font.bold
                            , Background.color gciBlue
                            , if isMobile then
                                width fill

                              else
                                pointer
                            ]
                            (text item.name)
                        )
                    , Events.onMouseEnter (SimpleBtnHover item.id)
                    , Events.onMouseLeave (SimpleBtnUnHover item.id)
                    , if isMobile then
                        width fill

                      else
                        pointer
                    , htmlAttribute <| class "gciBtn"
                    ]
            in
            case item.message of
                Just m ->
                    el
                        (Events.onClick m :: attr)
                        (text item.name)

                Nothing ->
                    link [ centerX, width fill ] { url = item.link, label = el attr (text item.name) }
    in
    acol
        (if animateSelf then
            Animation.fromTo
                { duration = 500
                , options = []
                }
                [ P.opacity 0, P.y 100 ]
                [ P.opacity 100, P.y 0 ]

         else
            Animation.empty
        )
        [ width fill, padding 50, spacing 10, transparent (not animateSelf), htmlAttribute <| id "bottomButtons" ]
        [ paragraph [ Region.heading 4, fontSize device Lg, Font.center, Font.extraLight ] [ text "Want to know more about us?" ]
        , (if isMobile then
            column

           else
            row
          )
            [ centerX, spacing 10 ]
            (List.map btn btns)
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


animationTrackerToCmd : ( String, AnimationState ) -> Cmd Msg
animationTrackerToCmd ( k, _ ) =
    Task.attempt (GotElement k) (Browser.Dom.getElement k)


shouldAnimate : String -> Model -> Bool
shouldAnimate id model =
    case Dict.get id model.animationTracker of
        Just state ->
            state.shouldAnimate

        Nothing ->
            False
