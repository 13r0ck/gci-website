module Pages.Whoweare exposing (Model, Msg, page)

import Browser.Dom exposing (Viewport)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font exposing (letterSpacing)
import Element.Input as Input
import Element.Region as Region
import Gen.Params.Whoweare exposing (Params)
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
            , SimpleBtn 1 "Intellectual property" "/ip" False Nothing
            , SimpleBtn 2 "What we do" "/#whatwedo" False Nothing
            , SimpleBtn 3 "Contact Us" "" False (Just OpenContactUs)
            , SimpleBtn 4 "Technical Papers" "/technical" False Nothing
            ]
      , animationTracker =
            Dict.fromList
                [ ( "mainText", AnimationState (PercentOfViewport 40) False )
                , ( "bottomButtons", AnimationState (PercentOfViewport 40) False )
                , ( "1", AnimationState (PercentOfViewport 20) False )
                , ( "2", AnimationState (PercentOfViewport 40) False )
                , ( "3", AnimationState (PercentOfViewport 40) False )
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
                    [ mainText shared.temp (shouldAnimate "mainText" model) ]
                , leadership shared
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
    in
    image
        [ width fill
        , height (px h)
        , clip
        , inFront (el [ width fill, height fill, Background.color (rgba 0 0 0 0.25) ] none)
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
                [ text "Global", text "Circuit", text "Inovations." ]
            )
        ]
        { src = "/img/building.jpg", description = "Picture of GCI's head quarters" }


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
        [ paragraph [ Font.extraLight, Region.heading 1, fontSize device Xlg ] [ text "Founded in 2006 in Colorado Springs, CO." ]
        , paragraph [ spacing 10, fontSize device Sm, Font.light, htmlAttribute <| id "mainText", width fill ]
            [ text "Global Circuit Innovations (GCI) is a Design & Manufacturing Engineering Solutions House for Electronic Obsolescence"
            , html <| br [] []
            , html <| br [] []
            ]

        {-
           , el [centerX, fontSize device Md, padding 3, Font.light, Border.widthEach {bottom = 1, top = 0, left = 0, right = 0}] (text "Our Core Beliefs")
           , paragraph [ spacing 10, fontSize device Sm, Font.light, htmlAttribute <| id "mainText", width fill]
               [ el [Font.regular] (text "Performance: ")
               , text "Electronics solutions is the foundation for all we do and executional excellence is a core value of our Team."
               , html <| br [] []
               , html <| br [] []
               , el [Font.regular] (text "Passion: ")
               , text "Passion is at the heart of our company. We are continuously moving forward, innovating, and improving."
               , html <| br [] []
               , html <| br [] []
               , el [Font.regular] (text "Integrity: ")
               , text "We are honest, open, ethical, and fair. People trust us to adhere to our word."
               , html <| br [] []
               , html <| br [] []
               , el [Font.regular] (text "Diversity: ")
               , text "We know it takes people with different ideas, strengths, interests, and cultural backgrounds to make our company succeed. We encourage healthy debate and differences of opinion."
               , html <| br [] []
               , html <| br [] []
               , el [Font.regular] (text "Accountability: ")
               , text "Measuring ourselves against the highest standards of integrity and fiscal responsibility."
               ]
        -}
        ]


leadership : Shared.Model -> Element Msg
leadership shared =
    let
        device =
            shared.temp.device.class
    in
    column
        [ width fill
        , htmlAttribute <| class "circuit_board"
        , Border.innerShadow { blur = 10, color = rgba 0 0 0 0.3, offset = ( -5, 5 ), size = 5 }
        ]
        [ column [ spacing 3, padding 50, width fill, Background.color (rgba 1 1 1 0) ]
            [ el [ Font.bold, fontSize device Xsm, Font.center, centerX ] (text "Global Circuit Innovations")
            , el [ Font.extraLight, Font.letterSpacing 5, Font.center, centerX, Font.underline, fontSize device Xlg ] (text "Leadership")
            ]
        , wrappedRow [ centerX ]
            [ column
                [ width (px 300)
                , height (px 300)
                , Border.rounded 20
                , clip
                , centerX
                , Border.shadow { blur = 10, color = rgba 0 0 0 0.3, offset = ( -5, 5 ), size = 5 }
                , Background.color white
                ]
                [ el [ Background.color warning, width fill, height fill ] none
                , el [ height fill, width fill ] none
                ]
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
