module Pages.Home_ exposing (Model, Msg, page, AnimationState, When(..), onScreenItemtoCmd, updateElement)

import Browser.Dom exposing (Viewport)
import Browser.Events exposing (Visibility(..), onResize, onVisibilityChange)
import Char exposing (isDigit)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (innerShadow, rounded, shadow, widthEach)
import Element.Events as EE
import Element.Font as Font exposing (center)
import Element.Input as Input
import Element.Region as Region
import Gen.Params.Home_ exposing (Params)
import Html exposing (a, br, video, wbr)
import Html.Attributes exposing (alt, attribute, autoplay, class, id, loop, src)
import Html.Events
import Json.Decode as Decode
import Page
import Palette exposing (FontSize(..), black, fontSize, gciBlue, gciBlueLight, maxWidth, warning, white)
import PhoneNumber
import PhoneNumber.Countries exposing (countryUS)
import Ports exposing (controlVideo, recvScroll)
import Request
import Shared exposing (Msg(..), acol, ael, arow, contactUs, footer, navbar)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage exposing (Address, ContactDialogState, NavBarDisplay(..))
import Swiper exposing (SwipingState)
import Task
import Time exposing (..)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update shared
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { getMouse : Bool
    , swipingState : SwipingState
    , userVisible : Bool
    , showContactUs : Bool
    , testimonial_viewNum : Int
    , animationTracker : Dict String AnimationState
    , onScreenTracker : List OnScreenItem
    , simpleBtnHoverTracker : List SimpleBtn
    , testimonials : List Testimonial
    , boxes : List BoxesItem
    , name : String
    , localShared : Shared.Model
    }


type alias Testimonial =
    { title : String
    , img : String
    , quote : String
    , attribution : String
    , job : String
    }


type alias NavItem =
    { name : String
    , link : String
    , hovered : Bool
    , message : Msg
    }


type alias OnScreenItem =
    { id : String
    , onScreen : Bool
    }


type alias SimpleBtn =
    { id : Int
    , name : String
    , link : String
    , hovered : Bool
    , message : Msg
    }


type alias BoxesItem =
    { name : String
    , link : String
    , img_default : String
    , img_hover : String
    , hovered : Bool
    , class : String
    }


type alias AnimationState =
    { when : When
    , shouldAnimate : Bool
    }


type When
    = PercentOfViewport Float
    | LeavesTop
    | Middle


type Direction
    = Up
    | Down
    | Left
    | Right


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { getMouse = False
      , swipingState = Swiper.initialSwipingState
      , userVisible = True
      , showContactUs = False
      , name = ""
      , testimonial_viewNum = 1
      , animationTracker =
            Dict.fromList
                [ ( "gciBar", AnimationState Middle False )
                , ( "whatwedo", AnimationState (PercentOfViewport 30) False )
                , ( "grayQuote", AnimationState (PercentOfViewport 50) False )
                , ( "testimonials", AnimationState Middle False )
                , ( "cleanRoom", AnimationState (PercentOfViewport 40) False )
                ]
      , onScreenTracker =
            [ OnScreenItem "earthVideo" True
            ]
      , simpleBtnHoverTracker =
            [ SimpleBtn 0 "Contact Us" "#" False OpenContactUs
            ]
      , testimonials =
            [ Testimonial "Sikorsky Aircraft Corperation" "/img/sky.jpg" "Global Circuit Innovations provided a form, fit and function solution to keep our Black Hawk helicopters flying." "- Peter Kubik" "(Senior Staff Engineer)"
            , Testimonial "USAF" "/img/helicopter0.jpg" "GCI offers cost effective, proven obsolescence solutions to keep planes flying and save the US Air Force tens of millions of dollars." "- Jeffery Sillart" "(USAF Lead-Engineer, F-16)"
            , Testimonial "Sikorsky Aircraft Corperation" "/img/sky.jpg" "This is the spot for a third testimonial" "- Brock" "(Cool Dude)"
            ]
      , boxes =
            [ BoxesItem "Electronic Obsolescence Solutions" "/obsolescence" "/img/plane1.png" "/img/plane2.png" False "point_idle"
            , BoxesItem "Electronic Systems" "/systems" "/img/circuit1.png" "/img/circuit2.png" False "point_idle"
            , BoxesItem "Oil and Gas High Temp Electronics" "/oil" "img/oil1.png" "/img/oil2.png" False "point_idle"
            , BoxesItem "Research and Development" "/dev" "img/oil1.png" "/img/oil2.png" False "point_idle"
            ]
      , localShared = shared
      }
    , controlVideo True |> Effect.fromCmd
    )



-- UPDATE


type Msg
    = Scrolled Int
    | TestimonialSwiped Swiper.SwipeEvent
    | TestimonialLeft
    | TestimonialRight
    | GotMouse Direction
    | GotElement String (Result Browser.Dom.Error Browser.Dom.Element)
    | GotOnScreenItem String (Result Browser.Dom.Error Browser.Dom.Element)
    | OpenContactUs
    | BoxHover Int
    | BoxUnHover Int
    | SimpleBtnHover Int
    | SimpleBtnUnHover Int
    | VisibilityChanged Visibility
    | ModifyLocalShared Shared.Model
    | WindowResized Int Int


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Scrolled distance ->
            ( { model
                | localShared =
                    model.localShared
                        |> (\l ->
                                { l
                                    | scrolledDistance = distance
                                    , navbarDisplay =
                                        if abs (distance - l.scrolledDistance) > 10 then
                                            if distance > l.scrolledDistance then
                                                Hide

                                            else
                                                Enter

                                        else
                                            l.navbarDisplay
                                }
                           )
              }
            , Effect.batch
                (List.map animationTrackerToCmd (List.filter (\( _, v ) -> v.shouldAnimate == False) (Dict.toList model.animationTracker))
                    ++ List.map (\i -> onScreenItemtoCmd i.id) model.onScreenTracker
                )
            )

        GotElement id element ->
            case element of
                Ok e ->
                    ( { model | animationTracker = Dict.fromList (List.map (updateElement id e) (Dict.toList model.animationTracker)) }, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        GotOnScreenItem id element ->
            case element of
                Ok e ->
                    ( { model | onScreenTracker = List.map (updateOnScreenElement id e) model.onScreenTracker }
                    , if isOnScreen "earthVideo" (List.map (updateOnScreenElement id e) model.onScreenTracker) then
                        controlVideo True |> Effect.fromCmd
                        -- Play

                      else
                        controlVideo False |> Effect.fromCmd
                      -- Pause
                    )

                Err _ ->
                    ( model, Effect.none )

        VisibilityChanged visibility ->
            if visibility == Visible then
                ( { model | userVisible = True }, controlVideo True |> Effect.fromCmd )
                -- Play

            else
                ( { model | userVisible = False }, controlVideo False |> Effect.fromCmd )

        -- Pause
        BoxHover id ->
            ( { model | boxes = List.indexedMap (setHovered id) model.boxes, getMouse = True }, Effect.none )

        BoxUnHover id ->
            ( { model | boxes = List.indexedMap (setUnHovered id) model.boxes, getMouse = True }, Effect.none )

        SimpleBtnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setHovered id) model.simpleBtnHoverTracker }, Effect.none )

        SimpleBtnUnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setUnHovered id) model.simpleBtnHoverTracker }, Effect.none )

        GotMouse direction ->
            ( { model | getMouse = False, boxes = List.map (updateBoxes direction) model.boxes }, Effect.none )

        TestimonialLeft ->
            ( { model
                | testimonial_viewNum =
                    if not (model.testimonial_viewNum == 0) then
                        model.testimonial_viewNum - 1

                    else
                        model.testimonial_viewNum
              }
            , Effect.none
            )

        TestimonialRight ->
            ( { model
                | testimonial_viewNum =
                    if not (model.testimonial_viewNum == List.length model.testimonials - 1) then
                        model.testimonial_viewNum + 1

                    else
                        model.testimonial_viewNum
              }
            , Effect.none
            )

        TestimonialSwiped event ->
            let
                test fn =
                    Tuple.second (fn event model.swipingState)
            in
            if test Swiper.hasSwipedLeft then
                ( { model
                    | testimonial_viewNum =
                        if not (model.testimonial_viewNum == 0) then
                            model.testimonial_viewNum - 1

                        else
                            model.testimonial_viewNum
                    , swipingState = Tuple.first (Swiper.hasSwipedLeft event model.swipingState)
                  }
                , Effect.none
                )

            else if test Swiper.hasSwipedRight then
                ( { model
                    | testimonial_viewNum =
                        if not (model.testimonial_viewNum == List.length model.testimonials - 1) then
                            model.testimonial_viewNum + 1

                        else
                            model.testimonial_viewNum
                    , swipingState = Tuple.first (Swiper.hasSwipedRight event model.swipingState)
                  }
                , Effect.none
                )

            else
                ( { model | swipingState = Tuple.first (Swiper.hasSwipedDown event model.swipingState) }, Effect.none )

        ModifyLocalShared newSharedState ->
            ( { model | localShared = newSharedState }
            , if not (newSharedState.contactDialogState == model.localShared.contactDialogState) then
                Effect.batch
                    [ Shared.UpdateModel newSharedState |> Effect.fromShared
                    , newSharedState.contactDialogState |> Storage.toJson |> Ports.save |> Effect.fromCmd
                    ]

              else
                Shared.UpdateModel newSharedState |> Effect.fromShared
            )

        OpenContactUs ->
            let
                withOpen state =
                    { state | contactDialogState = state.contactDialogState |> (\c -> { c | showContactUs = True }) }
            in
            ( { model | localShared = withOpen model.localShared }, Shared.UpdateModel (withOpen model.localShared) |> Effect.fromShared )

        WindowResized w h ->
            let
                newModel share =
                    { share | device = classifyDevice { width = w, height = h }, width = w, height = h }
            in
            ( { model | localShared = newModel model.localShared }, Shared.UpdateModel (newModel model.localShared) |> Effect.fromShared )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        parseMouse x y =
            if abs x > abs y then
                if x > 0 then
                    GotMouse Left

                else
                    GotMouse Right

            else if y > 0 then
                GotMouse Down

            else
                GotMouse Up
    in
    if model.userVisible then
        if model.getMouse then
            Sub.batch
                [ Browser.Events.onMouseMove
                    (Decode.map2 parseMouse
                        (Decode.field "movementX" Decode.int)
                        (Decode.field "movementY" Decode.int)
                    )
                , recvScroll Scrolled
                , Browser.Events.onResize WindowResized
                ]

        else
            Sub.batch
                [ onVisibilityChange (\v -> VisibilityChanged v)
                , recvScroll Scrolled
                , Browser.Events.onResize WindowResized
                ]

    else
        Sub.batch
            [ onVisibilityChange (\v -> VisibilityChanged v)
            , recvScroll Scrolled
            , Browser.Events.onResize WindowResized
            ]



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "GCI - Authorized Reverse Engineering IC Solutions for Obsolescence and High Temperature Environments"
    , attributes =
        [ inFront (navbar model.localShared ModifyLocalShared)
        , inFront (point_down (shouldAnimate "testimonials" model))
        , inFront
            (if shared.contactDialogState.showContactUs then
                contactUs model.localShared ModifyLocalShared

             else
                none
            )
        ]
    , element =
        column [ width fill, Region.mainContent, htmlAttribute <| id "home", clip ]
            [ column [ width (fill |> maximum maxWidth), centerX, spacing 25 ]
                [ head shared
                , innovations (shouldAnimate "testimonials" model) shared
                , testimonials model.testimonials model.testimonial_viewNum (shouldAnimate "testimonials" model) shared
                , grayQuote (shouldAnimate "grayQuote" model) shared
                , boxes (shouldAnimate "whatwedo" model) model.boxes shared
                , cleanRoom (shouldAnimate "cleanRoom" model) model.simpleBtnHoverTracker shared
                ]
            , footer shared ModifyLocalShared
            ]
    }



-- Helper Functions


updateElement : String -> Browser.Dom.Element -> ( String, AnimationState ) -> ( String, AnimationState )
updateElement id element ( k, v ) =
    if id == k && not v.shouldAnimate then
        case v.when of
            PercentOfViewport p ->
                ( id
                , { when = PercentOfViewport p
                  , shouldAnimate = element.element.y + (element.element.height * (p / 100)) <= element.viewport.y + element.viewport.height
                  }
                )

            Middle ->
                ( id
                , { when = Middle
                  , shouldAnimate = element.element.y <= element.viewport.y + (element.viewport.height * 0.5)
                  }
                )

            LeavesTop ->
                ( id
                , { when = LeavesTop
                  , shouldAnimate = element.element.y + element.element.height < element.viewport.y
                  }
                )

    else
        ( k, v )


animationTrackerToCmd : ( String, AnimationState ) -> Effect Msg
animationTrackerToCmd ( k, _ ) =
    Task.attempt (GotElement k) (Browser.Dom.getElement k) |> Effect.fromCmd


onScreenItemtoCmd : String -> Effect Msg
onScreenItemtoCmd id =
    Task.attempt (GotOnScreenItem id) (Browser.Dom.getElement id) |> Effect.fromCmd


updateBoxes : Direction -> BoxesItem -> BoxesItem
updateBoxes direction box =
    if box.hovered then
        { box
            | class =
                case direction of
                    Up ->
                        "point_enter_up"

                    Down ->
                        "point_enter_down"

                    Left ->
                        "point_enter_left"

                    Right ->
                        "point_enter_right"
        }

    else if String.contains "enter" box.class then
        { box
            | class =
                case direction of
                    Up ->
                        "point_leave_up"

                    Down ->
                        "point_leave_down"

                    Left ->
                        "point_leave_left"

                    Right ->
                        "point_leave_right"
        }

    else
        box


setHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setHovered id i data =
    if id == i then
        { data | hovered = True }

    else
        { data | hovered = False }


setUnHovered : Int -> Int -> { a | hovered : Bool } -> { a | hovered : Bool }
setUnHovered _ _ data =
    { data | hovered = False }


updateOnScreenElement : String -> Browser.Dom.Element -> OnScreenItem -> OnScreenItem
updateOnScreenElement id e item =
    if id == item.id then
        { item
            | onScreen =
                (e.element.y > e.viewport.y + e.viewport.height && e.element.y + e.element.height < e.viewport.y)
                    || (e.element.y + e.element.height > e.viewport.y && e.element.y < e.viewport.y + e.viewport.height)
        }

    else
        item


isOnScreen : String -> List OnScreenItem -> Bool
isOnScreen id list =
    List.any (\item -> item.onScreen == True) (List.filter (\item -> item.id == id) list)


shouldAnimate : String -> Model -> Bool
shouldAnimate id model =
    case Dict.get id model.animationTracker of
        Just state ->
            state.shouldAnimate

        Nothing ->
            False


point_down : Bool -> Element msg
point_down scrolled =
    let
        bounce =
            el
                [ Background.color white
                , padding 20
                , Border.rounded 20
                , Border.shadow { blur = 8, color = rgba 0 0 0 0.5, offset = ( -5, 8 ), size = 1 }
                ]
                (ael
                    (Animation.steps
                        { startAt = [ P.y -10 ]
                        , options = [ Animation.loop, Animation.easeInOutQuad ]
                        }
                        [ Animation.step 550 [ P.y 10 ]
                        , Animation.step 700 [ P.y -10 ]
                        ]
                    )
                    []
                    (image [ width (px 40), height (px 40) ] { src = "/img/down_arrow.svg", description = "down arrow" })
                 --link [ width (px 40), height (px 40) ] { url = "/#testimonials", label = image [ width fill, height fill ] { src = "/img/down_arrow.svg", description = "down arrow" } }
                )
    in
    acol
        (if scrolled then
            Animation.fromTo
                { duration = 500
                , options = []
                }
                [ P.opacity 1, P.y 0 ]
                [ P.opacity 0, P.y 20 ]

         else
            Animation.empty
        )
        [ centerX
        , alignBottom
        , height (px 150)
        ]
        [ row [ height (px 50) ] []
        , if scrolled then
            bounce

          else
            link []
                { url = "/#testimonials", label = bounce }
        ]


head : Shared.Model -> Element msg
head shared =
    let
        w =
            shared.width

        h =
            shared.height

        glassLogo =
            image
                [ width
                    (px
                        (if shared.device.class == Phone then
                            w * 2

                         else
                            w
                        )
                    )
                , height (px h)
                , centerX
                , centerY
                ]
                { src = "/img/glass.png", description = "GCI logo on glass" }

        logo =
            el
                [ width (px w)
                , height (px videoHeight)
                , centerX
                , centerY
                ]
                (image
                    [ width
                        (if shared.device.class == Phone then
                            fill

                         else
                            px (w // 2) |> maximum (maxWidth // 2)
                        )
                    , centerX
                    , centerY
                    , padding
                        (if shared.device.class == Phone then
                            floor (toFloat w * 0.1)

                         else
                            0
                        )
                    ]
                    { src = "/img/logo_sans_ring.svg", description = "Global Circuit Inovations" }
                )

        earthVideo =
            html <|
                video
                    [ src "/videos/earth_720p.webm"
                    , alt "Earth from Space"
                    , autoplay True
                    , loop True
                    , if scaleByHeight then
                        Html.Attributes.height h

                      else
                        Html.Attributes.width w
                    , attribute "poster" "/img/earthVideo.jpg"
                    , id "earthVideo"
                    ]
                    []

        videoHeight =
            h

        scaleByHeight =
            w // videoHeight <= 16 // 9
    in
    row [ width fill, height (px videoHeight), Background.color (rgb 0 0 0), clip ]
        [ el
            [ width fill
            , Background.color (rgb 0 0 0)
            , inFront glassLogo
            , inFront logo
            , clip
            ]
            (el
                [ centerX
                , centerY
                ]
                earthVideo
            )
        ]


innovations : Bool -> Shared.Model -> Element msg
innovations animateSelf shared =
    let
        device =
            shared.device.class

        isPhone =
            device == Phone

        isDesktop =
            device == Desktop || device == BigDesktop
    in
    acol
        (if animateSelf then
            Animation.fromTo
                { duration = 500
                , options = []
                }
                [ P.opacity 0 ]
                [ P.opacity 100 ]

         else
            Animation.empty
        )
        [ centerX
        , padding 10
        , spacing 10
        , htmlAttribute <| id "testimonials"
        , transparent (not animateSelf)
        ]
        [ paragraph
            [ centerX
            , Font.light
            , fontSize device Xlg
            , Font.center
            ]
            [ text "Our Innovations are Your Solutions." ]
        ]


testimonials : List Testimonial -> Int -> Bool -> Shared.Model -> Element Msg
testimonials ts viewNum animateSelf shared =
    let
        numberToShow =
            if isPhone then
                1

            else
                shared.width // floor (toFloat testimonial_width * 1.2)

        testimonial_width =
            if isPhone then
                toFloat shared.width * 0.7 |> floor

            else
                370

        device =
            shared.device.class

        isPhone =
            device == Phone

        isDesktop =
            device == Desktop || device == BigDesktop

        quotePadding =
            if isPhone then
                5

            else
                48

        testimonial i t =
            acol
                (if animateSelf then
                    Animation.fromTo
                        { duration = (i + 1) * 300
                        , options = []
                        }
                        [ P.opacity 0, P.y 100 ]
                        [ P.opacity 100, P.y 0 ]

                 else
                    Animation.empty
                )
                [ width
                    (if numberToShow >= List.length ts then
                        fill |> maximum testimonial_width

                     else
                        px testimonial_width
                    )
                , height fill
                , Background.color white
                , rounded 10
                , clip
                , centerX
                , Border.shadow { blur = 8, color = rgba 0 0 0 0.2, offset = ( -5, 8 ), size = 1 }
                , transparent (not animateSelf)
                ]
                [ row [ Background.image t.img, height (px 200), width fill ] []
                , column [ paddingEach { top = quotePadding, bottom = quotePadding, left = quotePadding, right = quotePadding }, height fill, width fill ]
                    [ paragraph [ Font.medium, Font.center, fontSize device Sm, paddingXY 0 18 ] [ text ("\"" ++ t.quote ++ "\"") ]
                    , paragraph [ Font.extraLight, Font.alignRight, fontSize device Xsm, alignBottom ] [ text t.attribution ]
                    , paragraph [ Font.extraLight, Font.alignRight, fontSize device Xsm, alignBottom ] [ text t.job ]
                    ]
                ]
    in
    if numberToShow >= List.length ts then
        row [ width (fill |> maximum (toFloat maxWidth * 0.9 |> ceiling)), centerX, height shrink, spacing 48, paddingXY 48 0 ] (List.indexedMap testimonial ts)

    else
        row [ width fill ]
            [ ael
                (if viewNum == 0 || not animateSelf then
                    Animation.fromTo
                        { duration = 200
                        , options = []
                        }
                        [ P.opacity 100, P.y 0 ]
                        [ P.opacity 0, P.y 10 ]

                 else
                    Animation.fromTo
                        { duration = 200
                        , options = []
                        }
                        [ P.opacity 0, P.y 10 ]
                        [ P.opacity 100, P.y 0 ]
                )
                [ centerX ]
                (Input.button []
                    { onPress =
                        if viewNum == 0 then
                            Nothing

                        else
                            Just TestimonialLeft
                    , label = image [ width (px 30), height (px 30), centerY, centerX, mouseOver [ moveLeft 5 ] ] { src = "/img/left.svg", description = "left button" }
                    }
                )
            , el [ padding 10, centerX ]
                (el
                    ([ width (px (testimonial_width * numberToShow + 48 * (numberToShow - 1) + 10))
                     , height fill
                     , centerX
                     , centerY
                     , clip
                     ]
                        ++ List.map (\a -> htmlAttribute <| a) (Swiper.onSwipeEvents TestimonialSwiped)
                    )
                    (row [ moveLeft (toFloat (testimonial_width * viewNum + 48 * viewNum)), width (fill |> maximum (toFloat maxWidth * 0.9 |> ceiling)), centerX, height shrink, spacing 48, paddingEach { left = 10, bottom = 20, top = 20, right = 0 }, htmlAttribute <| class "animateTransform" ] (List.indexedMap testimonial ts))
                )
            , ael
                (if viewNum + numberToShow >= List.length ts || not animateSelf then
                    Animation.fromTo
                        { duration = 200
                        , options = []
                        }
                        [ P.opacity 100, P.y 0 ]
                        [ P.opacity 0, P.y 10 ]

                 else
                    Animation.fromTo
                        { duration = 200
                        , options = []
                        }
                        [ P.opacity 0, P.y 10 ]
                        [ P.opacity 100, P.y 0 ]
                )
                [ centerX ]
                (Input.button []
                    { onPress =
                        if viewNum + numberToShow >= List.length ts then
                            Nothing

                        else
                            Just TestimonialRight
                    , label = image [ width (px 30), height (px 30), centerY, centerX, mouseOver [ moveRight 5 ] ] { src = "/img/right.svg", description = "right button" }
                    }
                )
            ]


blur : Element msg
blur =
    row [ width fill, height fill, Background.color (rgba 1 1 1 0.1) ] []


onEnter : msg -> Element.Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


cleanRoom : Bool -> List SimpleBtn -> Shared.Model -> Element Msg
cleanRoom animateSelf simpleBtns shared =
    let
        device =
            shared.device.class

        isPhone =
            device == Phone

        btn item =
            el
                [ centerX
                , Border.width 5
                , paddingXY 20 10
                , Border.rounded 10
                , fontSize device Md
                , Font.color white
                , Font.bold
                , htmlAttribute <| class "background_transition"
                , Border.color white
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
                        , Font.color gciBlue
                        , rounded 5
                        , Font.bold
                        , Background.color white
                        ]
                        (text item.name)
                    )
                , EE.onMouseEnter (SimpleBtnHover 0)
                , EE.onMouseLeave (SimpleBtnUnHover 0)
                , EE.onClick item.message
                , pointer
                , htmlAttribute <| class "gciBtn"
                ]
                (text item.name)
    in
    row
        [ height (px 400)
        , width fill
        , Background.image "/img/clean_room2.jpg"
        , transparent (not animateSelf)
        , htmlAttribute <| id "cleanRoom"
        , htmlAttribute <|
            class
                (if animateSelf then
                    "point_enter_left_long"

                 else
                    "point_idle"
                )
        ]
        [ if isPhone then
            none

          else
            column [ width fill ] []
        , column [ width (fillPortion 2), height fill ]
            [ paragraph
                [ width fill
                , alignRight
                , Font.extraBold
                , fontSize device Lg
                , Font.color white
                , Font.center
                , centerY
                ]
                [ text "Interested?"
                , html <| br [] []
                , text "See how we can help!"
                , html <| br [] []
                ]
            , row [ centerX, centerY, padding 24 ] (List.map btn (List.filter (\a -> a.id == 0) simpleBtns))
            ]
        ]


boxes : Bool -> List BoxesItem -> Shared.Model -> Element Msg
boxes animateSelf content shared =
    let
        device =
            shared.device.class

        isPhone =
            device == Phone

        isTablet =
            device == Tablet

        isMobile =
            isPhone || isTablet

        w =
            shared.width

        maxW =
            min w maxWidth

        eachWidth =
            max 250 ((toFloat maxW * 0.9 |> floor) // List.length content)

        box ( id, item ) =
            link []
                { url = item.link
                , label =
                    arow
                        (if animateSelf then
                            Animation.fromTo
                                { duration = (id + 1) * 500
                                , options = []
                                }
                                [ P.opacity 0, P.y 100 ]
                                [ P.opacity 100, P.y 0 ]

                         else
                            Animation.empty
                        )
                        [ width (px eachWidth)
                        , height (px eachWidth)
                        , Background.image item.img_default
                        , inFront
                            (el
                                [ width fill
                                , height fill
                                , Background.image item.img_hover
                                , htmlAttribute <| class item.class
                                ]
                                (paragraph
                                    [ fontSize device Md
                                    , Font.alignLeft
                                    , Font.light
                                    , alignBottom
                                    , alignLeft
                                    , padding 20
                                    ]
                                    [ text item.name ]
                                )
                            )
                        , EE.onMouseEnter (BoxHover id)
                        , EE.onMouseLeave (BoxUnHover id)
                        ]
                        [ paragraph
                            [ fontSize device Sm
                            , Font.alignLeft
                            , Font.bold
                            , Font.color white
                            , alignBottom
                            , alignLeft
                            , padding 10
                            ]
                            [ text item.name ]
                        ]
                }
    in
    column
        [ centerX
        , transparent (not animateSelf)
        , htmlAttribute <| id "whatwedo"
        ]
        [ ael
            (if animateSelf then
                Animation.fromTo
                    { duration = 1000
                    , options = []
                    }
                    [ P.opacity 0 ]
                    [ P.opacity 100 ]

             else
                Animation.empty
            )
            [ centerX
            , paddingEach { top = 64, bottom = 24, left = 0, right = 0 }
            , fontSize device Lg
            , Font.extraLight
            ]
            (paragraph [ Font.center ]
                [ text "What do we do? "
                , html <|
                    if isMobile then
                        br [] []

                    else
                        wbr [] []
                , text
                    ((if isPhone then
                        "Tap"

                      else
                        "Click"
                     )
                        ++ " below to find out more."
                    )
                ]
            )
        , el [ width (px (eachWidth * (shared.width // eachWidth))), centerX ] (wrappedRow [ centerX ] (List.map box (List.indexedMap Tuple.pair content)))
        , paragraph [ centerX, Font.light, Font.center, fontSize device Md, padding 30, width (fill |> maximum 800) ] [ text "GCI provides solutions for otherwise obsolete electronic systems. Keeping assets fully operational for many decades in the future." ]
        ]


grayQuote : Bool -> Shared.Model -> Element msg
grayQuote animateSelf shared =
    let
        device =
            shared.device.class

        w =
            shared.width

        isPhone =
            device == Phone

        dynamicPadding =
            if isPhone then
                10

            else
                toFloat (min shared.width maxWidth) * 0.1 |> round
    in
    column
        [ width
            (px
                (if w > maxWidth then
                    (toFloat maxWidth * 0.9) |> ceiling

                 else
                    w
                )
            )
        , height fill
        , centerX
        , paddingXY 0 100
        , Background.gradient { angle = degrees 180, steps = [ white, white, rgb255 214 218 219 ] }
        , htmlAttribute <| id "grayQuote"
        ]
        [ ael
            (if animateSelf then
                Animation.fromTo
                    { duration = 1000
                    , options = []
                    }
                    [ P.opacity 0 ]
                    [ P.opacity 100 ]

             else
                Animation.empty
            )
            [ centerY ]
            (paragraph
                [ paddingXY dynamicPadding 0
                , Font.alignLeft
                , Font.extraLight
                , fontSize device Xlg
                , Font.color (rgb255 85 96 134)
                , transparent (not animateSelf)
                ]
                [ text "Broad Expertise in Electronic Systems." ]
            )
        , ael
            (if animateSelf then
                Animation.fromTo
                    { duration = 1000
                    , options = []
                    }
                    [ P.opacity 0 ]
                    [ P.opacity 100 ]

             else
                Animation.empty
            )
            [ centerY ]
            (paragraph
                [ Font.alignLeft
                , centerX
                , centerY
                , Font.light
                , Font.color (rgb255 85 96 134)
                , fontSize device Md
                , transparent (not animateSelf)
                , paddingXY dynamicPadding 40
                ]
                [ text "Global Circuit Innovation's expertise has a range of digital and analog security over many decades. This knowledge base is applied to develop electronic obsolescence solutions for legacy systems. Our device physics skills and experience enables us to provide environmental hardening for extremely high temperature applications."
                , html <| br [] []
                , el [ Font.bold, fontSize device Xsm ] (text "Cage: 7DGP6 | Duns: 80126549")
                ]
            )
        ]
