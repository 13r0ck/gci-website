module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom exposing (Viewport)
import Browser.Events exposing (Visibility(..), onResize, onVisibilityChange)
import Char exposing (isDigit)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (innerShadow, rounded, shadow)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Email as Email
import Gen.Params.Home_ exposing (Params)
import Html exposing (a, br, video)
import Html.Attributes exposing (alt, attribute, autoplay, class, id, loop, src)
import Html.Events
import Json.Decode as Decode
import Page
import Palette exposing (black, gciBlue, gciBlueLight, warning, white)
import PhoneNumber
import PhoneNumber.Countries exposing (countryUS)
import Request
import Shared exposing (Address, NavItem, contactUs)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage exposing (controlVideo, disableScrolling, recvScroll, setPhoneInputCursor)
import Task
import Time exposing (..)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { viewPort : Viewport
    , getMouse : Bool
    , hideNavbar : Bool
    , userVisible : Bool
    , showContactUs : Bool
    , animationTracker : Dict String AnimationState
    , onScreenTracker : List OnScreenItem
    , simpleBtnHoverTracker : List SimpleBtn
    , navHoverTracker : List NavItem
    , socialMedia : List SocialMediaItem
    , certifications : List CertificationItem
    , testimonials : List Testimonial
    , boxes : List BoxesItem
    , currentYear : Int
    , address : Address
    }


type alias CertificationItem =
    { src : String
    , description : String
    }



-- expects social media images to be a font. I reccommend using fontello to build the font


type alias SocialMediaItem =
    { char : String
    , hoverColor : Element.Color
    , link : String
    }


type alias Testimonial =
    { title : String
    , img : String
    , quote : String
    , attribution : String
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


init : Shared.Model -> ( Model, Cmd Msg )
init sharedModel =
    let
        emptyViewport =
            { scene =
                { width = 0
                , height = 0
                }
            , viewport =
                { x = 0
                , y = 0
                , width = 0
                , height = 0
                }
            }
    in
    ( { viewPort = emptyViewport
      , getMouse = False
      , hideNavbar = False
      , userVisible = True
      , showContactUs = False
      , currentYear = 0
      , address = sharedModel.address
      , contactDialogState = sharedModel.contactDialogState
      , navHoverTracker = sharedModel.navHoverTracker
      , animationTracker =
            Dict.fromList
                [ ( "gciBar", AnimationState Middle False )
                , ( "boxes", AnimationState Middle False )
                , ( "grayQuote", AnimationState (PercentOfViewport 100) False )
                , ( "testimonials", AnimationState Middle False )
                , ( "cleanRoom", AnimationState (PercentOfViewport 40) False )
                ]
      , onScreenTracker =
            [ OnScreenItem "earthVideo" True
            ]
      , simpleBtnHoverTracker =
            [ SimpleBtn 0 "Contact Us" "#" False OpenContactUs
            ]
      , socialMedia =
            [ SocialMediaItem "\u{F09A}" (rgb255 59 89 152) "#"
            , SocialMediaItem "\u{F099}" (rgb255 29 161 242) "#"
            , SocialMediaItem "\u{F30C}" (rgb255 0 119 181) "#"
            , SocialMediaItem "\u{F16A}" (rgb255 255 0 0) "#"
            ]
      , certifications =
            [ CertificationItem "/img/platinum_certified-v2_white.svg" "AS9100:2016 - ISO 9001:2015 Certified"
            , CertificationItem "/img/ANAB-certified_white.svg" "AS9100:2016 - ISO 9001:2015 Certified"
            ]
      , testimonials =
            [ Testimonial "Sikorsky Aircraft Corperation" "/img/sky.jpg" "Global Circuit Innovations provided a form, fit and function solution to keep out Black Hawk helicopters flying." "- Peter Kubik\n(Engineering Manager)"
            , Testimonial "USAF" "/img/helicopter0.jpg" "GCI offers cost effective, proven obsolescence solutions to keep planes flying and save the US Air Force tens of millions of dollars." "- Jeffery Sillart\n(USAF Lead-Engineer)"
            , Testimonial "Sikorsky Aircraft Corperation" "/img/sky.jpg" "Global Circuit Innovations provided a form, fit and function solution to keep out Black Hawk helicopters flying." "- Peter Kubik (Engineering Manager)"
            ]
      , boxes =
            [ BoxesItem "Electronic Obsolescence Solutions" "#" "/img/plane1.png" "/img/plane2.png" False "point_idle"
            , BoxesItem "Electronic Systems" "#" "/img/circuit1.png" "/img/circuit2.png" False "point_idle"
            , BoxesItem "Oil and Gas High Temperature Electronics" "#" "img/oil1.png" "/img/oil2.png" False "point_idle"
            ]
      }
    , Cmd.batch [ Task.perform GotViewport Browser.Dom.getViewport, Task.perform GotYear currentYear ]
    )



-- UPDATE


type Msg
    = GotViewport Viewport
    | GetViewport
    | Scrolled Int
    | GotMouse Direction
    | GotElement String (Result Browser.Dom.Error Browser.Dom.Element)
    | GotOnScreenItem String (Result Browser.Dom.Error Browser.Dom.Element)
    | GotYear Int
    | BoxHover Int
    | BoxUnHover Int
    | SimpleBtnHover Int
    | SimpleBtnUnHover Int
    | VisibilityChanged Visibility
    | OpenContactUs
    | CloseContactUs
    | ContactDialogName String
    | ContactDialogEmail String
    | ContactDialogPhone String
    | ContactDialogMessage String
    | ContactDialogBack
    | ContactDialogNext
    | SharedMsg Shared.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewport ->
            ( { model
                | hideNavbar =
                    if model.hideNavbar then
                        viewport.viewport.y >= model.viewPort.viewport.y

                    else
                        viewport.viewport.y > model.viewPort.viewport.y
                , viewPort = viewport
              }
            , Cmd.batch
                (List.map animationTrackerToCmd (List.filter (\( _, v ) -> v.shouldAnimate == False) (Dict.toList model.animationTracker))
                    ++ List.map (\i -> onScreenItemtoCmd i.id) model.onScreenTracker
                )
            )

        Scrolled _ ->
            ( model, Task.perform GotViewport Browser.Dom.getViewport )

        GetViewport ->
            ( model, Task.perform GotViewport Browser.Dom.getViewport )

        GotElement id element ->
            case element of
                Ok e ->
                    ( { model | animationTracker = Dict.fromList (List.map (updateElement id e) (Dict.toList model.animationTracker)) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotOnScreenItem id element ->
            case element of
                Ok e ->
                    ( { model | onScreenTracker = List.map (updateOnScreenElement id e) model.onScreenTracker }
                    , if isOnScreen "earthVideo" (List.map (updateOnScreenElement id e) model.onScreenTracker) then
                        controlVideo True
                        -- Play

                      else
                        controlVideo False
                      -- Pause
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotYear year ->
            ( { model | currentYear = year }, Cmd.none )

        VisibilityChanged visibility ->
            if visibility == Visible then
                ( { model | userVisible = True }, controlVideo True )
                -- Play

            else
                ( { model | userVisible = False }, controlVideo False )

        -- Pause
        SharedMsg message ->
            case message of
                Shared.Msg.NavHover id ->
                    ( { model | navHoverTracker = List.indexedMap (setHovered id) model.navHoverTracker }, Cmd.none )

                Shared.Msg.NavUnHover id ->
                    ( { model | navHoverTracker = List.indexedMap (setUnHovered id) model.navHoverTracker }, Cmd.none )

        BoxHover id ->
            ( { model | boxes = List.indexedMap (setHovered id) model.boxes, getMouse = True }, Cmd.none )

        BoxUnHover id ->
            ( { model | boxes = List.indexedMap (setUnHovered id) model.boxes, getMouse = True }, Cmd.none )

        SimpleBtnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setHovered id) model.simpleBtnHoverTracker }, Cmd.none )

        SimpleBtnUnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setUnHovered id) model.simpleBtnHoverTracker }, Cmd.none )

        GotMouse direction ->
            ( { model | getMouse = False, boxes = List.map (updateBoxes direction) model.boxes }, Cmd.none )

        OpenContactUs ->
            ( { model | showContactUs = True }, disableScrolling True )

        CloseContactUs ->
            ( { model | showContactUs = False, navHoverTracker = List.map (\b -> { b | hovered = False }) model.navHoverTracker }, disableScrolling False )

        ContactDialogName newName ->
            if model.contactDialogState.nameError then
                ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | name = Just newName, nameError = newName == "" }) }, Cmd.none )

            else
                ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | name = Just newName }) }, Cmd.none )

        ContactDialogEmail newEmail ->
            if model.contactDialogState.emailError then
                ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | email = Just (String.trim newEmail), emailError = not (Email.isValid newEmail) }) }, Cmd.none )

            else
                ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | email = Just (String.trim newEmail) }) }, Cmd.none )

        {- TODO:
           Set cursor position relative after char edit for cursor position to make sense if editing not last char

        -}
        ContactDialogPhone newPhone ->
            if model.contactDialogState.phoneError then
                ( { model
                    | contactDialogState =
                        model.contactDialogState
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
                , case model.contactDialogState.phone of
                    Just p ->
                        setPhoneCursor p newPhone

                    Nothing ->
                        Cmd.none
                )

            else
                ( { model
                    | contactDialogState =
                        model.contactDialogState
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
                , case model.contactDialogState.phone of
                    Just p ->
                        setPhoneCursor p newPhone

                    Nothing ->
                        Cmd.none
                )

        ContactDialogMessage newMessage ->
            if model.contactDialogState.messageError then
                ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | message = Just newMessage, messageError = newMessage == "" }) }, Cmd.none )

            else
                ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | message = Just newMessage }) }, Cmd.none )

        ContactDialogNext ->
            case model.contactDialogState.currentPage of
                0 ->
                    if not (String.trim (Maybe.withDefault "" model.contactDialogState.name) == "") then
                        ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, nameError = False, name = Just (String.trim (Maybe.withDefault "" s.name)) }) }, Cmd.none )

                    else
                        ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | nameError = True, name = Just (String.trim (Maybe.withDefault "" s.name)) }) }, Cmd.none )

                1 ->
                    if Email.isValid (Maybe.withDefault "" model.contactDialogState.email) && validUSNumber (String.right 10 (String.filter isDigit (Maybe.withDefault "" model.contactDialogState.phone))) then
                        ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, phoneError = False, emailError = False }) }, Cmd.none )

                    else if not (Email.isValid (Maybe.withDefault "" model.contactDialogState.email)) then
                        ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | emailError = True, phoneError = False }) }, Cmd.none )

                    else if not (validUSNumber (String.right 10 (String.filter isDigit (Maybe.withDefault "" model.contactDialogState.phone)))) then
                        ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | phoneError = True }) }, Cmd.none )

                    else
                        ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, phoneError = False, emailError = False }) }, Cmd.none )

                2 ->
                    if Maybe.withDefault "" model.contactDialogState.message == "" then
                        ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | messageError = True }) }, Cmd.none )

                    else
                        ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1, messageError = False }) }, Cmd.none )

                _ ->
                    ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage + 1 }) }, Cmd.none )

        ContactDialogBack ->
            if model.contactDialogState.currentPage == 0 then
                ( { model | showContactUs = False, navHoverTracker = List.map (\b -> { b | hovered = False }) model.navHoverTracker }, disableScrolling False )

            else
                ( { model | contactDialogState = model.contactDialogState |> (\s -> { s | currentPage = s.currentPage - 1 }) }, Cmd.none )



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
                , onResize (\_ _ -> GetViewport)
                , recvScroll Scrolled
                ]

        else
            Sub.batch
                [ onVisibilityChange (\v -> VisibilityChanged v)
                , recvScroll Scrolled
                , onResize (\_ _ -> GetViewport)
                ]

    else
        Sub.batch
            [ onVisibilityChange (\v -> VisibilityChanged v)
            , recvScroll Scrolled
            , onResize (\_ _ -> GetViewport)
            ]



-- VIEW


view : Model -> View Msg
view model =
    let
        current_width =
            model.viewPort.viewport.width |> ceiling

        current_height =
            model.viewPort.viewport.height |> ceiling
    in
    { title = "GCI - Authorized Reverse Engineering IC Solutions for Obsolescence and High Temperature Environments"
    , attributes =
        [ inFront (navbar model.navHoverTracker model.hideNavbar)
        , inFront (point_down (shouldAnimate "testimonials" model))
        , inFront
            (if model.showContactUs then
                map (\msg -> SharedMsg msg) (contactUs model.contactDialogState model.address)

             else
                none
            )
        ]
    , element =
        column [ width fill, Region.mainContent ]
            [ column [ width (fill |> maximum maxWidth), centerX, spacing 25 ]
                [ head current_width current_height
                , innovations (shouldAnimate "testimonials" model)
                , testimonials model.testimonials (shouldAnimate "testimonials" model)
                , grayQuote current_width (shouldAnimate "grayQuote" model)
                , boxes current_width (shouldAnimate "boxes" model) model.boxes
                , cleanRoom (shouldAnimate "cleanRoom" model) model.simpleBtnHoverTracker
                ]
            , footer model.certifications model.address model.navHoverTracker model.socialMedia model.currentYear
            ]
    }



-- Helper Functions


currentYear : Task.Task x Int
currentYear =
    Task.map2 Time.toYear Time.here Time.now


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


animationTrackerToCmd : ( String, AnimationState ) -> Cmd Msg
animationTrackerToCmd ( k, _ ) =
    Task.attempt (GotElement k) (Browser.Dom.getElement k)


onScreenItemtoCmd : String -> Cmd Msg
onScreenItemtoCmd id =
    Task.attempt (GotOnScreenItem id) (Browser.Dom.getElement id)


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
                            5

                        1 ->
                            6

                        2 ->
                            7

                        3 ->
                            12

                        4 ->
                            13

                        5 ->
                            14

                        n ->
                            n + 12
                    )

            else
                setPhoneInputCursor
                    (case i of
                        0 ->
                            6

                        1 ->
                            7

                        2 ->
                            12

                        3 ->
                            13

                        4 ->
                            14

                        n ->
                            n + 13
                    )

        Nothing ->
            Cmd.none


prettyPhoneNumber : String -> String
prettyPhoneNumber number =
    let
        clean =
            String.filter isDigit (String.replace "+1" "" number)
    in
    case String.length clean of
        0 ->
            "+1 ( "

        1 ->
            "+1 ( " ++ clean

        2 ->
            "+1 ( " ++ clean

        3 ->
            "+1 ( " ++ clean ++ " )  "

        4 ->
            "+1 ( " ++ String.left 3 clean ++ " )  " ++ String.right 1 clean

        5 ->
            "+1 ( " ++ String.left 3 clean ++ " )  " ++ String.right 2 clean

        _ ->
            "+1 ( " ++ String.left 3 clean ++ " )  " ++ String.slice 3 6 clean ++ " - " ++ String.slice 6 10 clean


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
        , ael
            (Animation.steps
                { startAt = [ P.y 0 ]
                , options = [ Animation.loop, Animation.easeInOutQuad ]
                }
                [ Animation.step 550 [ P.y 20 ]
                , Animation.step 700 [ P.y 0 ]
                ]
            )
            []
            (image [ width (px 40), height (px 40), Font.color gciBlue ] { src = "/img/down_arrow.svg", description = "down arrow" })
        ]


head : Int -> Int -> Element msg
head w h =
    let
        glassLogo =
            image [ width (px w), height videoHeight, centerX, centerY ] { src = "/img/glass.png", description = "GCI logo on glass" }

        logo =
            el
                [ width (px w)
                , height videoHeight
                , centerX
                , centerY
                ]
                (image
                    [ width (px (w // 2) |> maximum (maxWidth // 2))
                    , centerX
                    , centerY
                    ]
                    { src = "/img/logo_sans_ring.svg", description = "Global Circuit Inovations" }
                )

        earthVideo =
            html <|
                video
                    [ src "/videos/earth_1080p.webm"
                    , alt "Earth from Space"
                    , autoplay True
                    , loop True
                    , Html.Attributes.width w
                    , attribute "muted" "True"
                    , attribute "poster" "/img/earthVideo.jpg"
                    , id "earthVideo"
                    ]
                    []

        videoHeight =
            px <| ceiling <| toFloat h * 0.8
    in
    row [ width fill, height videoHeight, Background.color (rgb 0 0 0), clip ]
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


innovations : Bool -> Element msg
innovations animateSelf =
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
        [ el [ centerX, Font.extraLight, Font.size 50 ] (text "Our Innovations are Your Solutions")
        , paragraph [ centerX, Font.medium ] [ text "We don't just do stuff, we do stuff really good. Like super good. We are very cool. Pinky Promise." ]
        ]


testimonials : List Testimonial -> Bool -> Element msg
testimonials ts animateSelf =
    let
        testimonial i t =
            acol
                (if animateSelf then
                    Animation.fromTo
                        { duration = (i + 1) * 500
                        , options = []
                        }
                        [ P.opacity 0, P.y 100 ]
                        [ P.opacity 100, P.y 0 ]

                 else
                    Animation.empty
                )
                [ width fill
                , height fill
                , Background.color white
                , rounded 10
                , clip
                , centerX
                , Border.shadow { blur = 8, color = rgb 0.8 0.8 0.9, offset = ( -5, 8 ), size = 1 }
                , transparent (not animateSelf)
                ]
                [ row [ Background.image t.img, height (px 200), width fill ] []
                , column [ paddingEach { top = 24, bottom = 48, left = 48, right = 48 }, spacing 18, height fill ]
                    [ paragraph [ Font.medium, Font.center ] [ text ("\"" ++ t.quote ++ "\"") ]
                    , paragraph [ Font.extraLight, Font.alignRight, Font.size 18, alignBottom ] [ text t.attribution ]
                    ]
                ]
    in
    row [ width (fill |> maximum (toFloat maxWidth * 0.9 |> ceiling)), centerX, height shrink, spacing 48, paddingXY 48 0 ] (List.indexedMap testimonial ts)


blur : Element msg
blur =
    row [ width fill, height fill, Background.color (rgba 1 1 1 0.1) ] []


footer : List CertificationItem -> Address -> List NavItem -> List SocialMediaItem -> Int -> Element Msg
footer certifications address navbtns socials year =
    let
        footerNavBtn item =
            el [ mouseOver [ Font.color gciBlue ], pointer, padding 10, Events.onClick item.message ] (text item.name)

        footerSocailBtn item =
            el
                [ Font.family [ Font.typeface "icons" ]
                , mouseOver [ Font.color item.hoverColor ]
                , pointer
                , padding 10
                ]
                (text item.char)

        spacer =
            el [ paddingXY 28 10 ] (text "|")

        footerCertification item =
            image [ width (fill |> maximum 400) ] { src = item.src, description = item.description }
    in
    el
        [ width fill
        , height fill
        , Region.footer
        , Background.color (rgb255 70 70 72)
        , Border.color gciBlue
        , Border.widthEach { top = 8, bottom = 0, left = 0, right = 0 }
        ]
        (column
            [ Font.color white, centerX ]
            [ row [ padding 20, spacing 40, centerX ]
                (List.map footerCertification certifications)
            , row
                [ Font.bold, Font.size 15, padding 20, centerX ]
                (List.map footerNavBtn navbtns ++ spacer :: List.map footerSocailBtn socials)
            , el [ width fill, Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 } ]
                (paragraph [ centerX, width shrink, Font.size 18, padding 20 ]
                    [ el [ padding 10 ] (text address.street)
                    , el [ padding 10 ] (text address.city)
                    , link [ padding 10 ] { label = text address.phone, url = address.phoneLink }
                    , link [ padding 10 ] { label = text address.email, url = address.emailLink }
                    ]
                )
            , row
                [ spacing 10
                , Font.size 12
                , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
                , paddingXY 200 20
                , centerX
                , alignBottom
                ]
                [ el [] (text ("©" ++ String.fromInt year ++ " Global Circuit Innovations, Inc."))
                , el [ mouseOver [ Font.color gciBlue ], pointer ] (text "Accesability")
                , el [ mouseOver [ Font.color gciBlue ], pointer ] (text "Sitemap")
                , el [ mouseOver [ Font.color gciBlue ], pointer ] (text "Terms of Use")
                , el [ mouseOver [ Font.color gciBlue ], pointer ] (text "Privacy")
                , download [ mouseOver [ Font.color gciBlue ], pointer ]
                    { url = "/download/press.zip"
                    , label = text "Press Materials"
                    }
                , spacer
                , newTabLink []
                    { url = "https://regaltechsupport.com"
                    , label =
                        row
                            [ spacing 5
                            , paddingXY 5 5
                            , mouseOver [ Font.color (rgb 255 165 0) ]
                            ]
                            [ image [ width (px 20) ] { src = "/img/regaltechsupport.ico", description = "Regal Tech Support, LLC Logo" }
                            , text "Website made by Regal Tech Support"
                            ]
                    }
                ]
            ]
        )


cleanRoom : Bool -> List SimpleBtn -> Element Msg
cleanRoom animateSelf simpleBtns =
    let
        btn item =
            el
                [ centerX
                , Border.width 5
                , paddingXY 20 10
                , Border.rounded 10
                , Font.size 30
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
                        , Font.size 30
                        , Font.color gciBlue
                        , rounded 5
                        , Font.bold
                        , Background.color white
                        ]
                        (text item.name)
                    )
                , Events.onMouseEnter (SimpleBtnHover 0)
                , Events.onMouseLeave (SimpleBtnUnHover 0)
                , Events.onClick item.message
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
        [ column [ width fill ] []
        , column [ width (fillPortion 2), height fill ]
            [ paragraph
                [ width fill
                , alignRight
                , Font.extraBold
                , Font.size 40
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


boxes : Int -> Bool -> List BoxesItem -> Element Msg
boxes w animateSelf content =
    let
        maxW =
            min w maxWidth

        eachWidth =
            (toFloat maxW * 0.9 |> floor) // List.length content

        box ( id, item ) =
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
                    (row
                        [ width fill
                        , height fill
                        , Background.image item.img_hover
                        , htmlAttribute <| class item.class
                        ]
                        [ paragraph
                            [ Font.size 45
                            , Font.alignLeft
                            , Font.light
                            , alignBottom
                            , alignLeft
                            , padding 20
                            ]
                            [ text item.name ]
                        ]
                    )
                , Events.onMouseEnter (BoxHover id)
                , Events.onMouseLeave (BoxUnHover id)
                ]
                [ paragraph
                    [ Font.size 25
                    , Font.alignLeft
                    , Font.color white
                    , alignBottom
                    , alignLeft
                    , padding 10
                    ]
                    [ text item.name ]
                ]

        btn item =
            el
                [ centerX
                , Border.width 5
                , paddingXY 20 10
                , Border.rounded 10
                , Font.size 30
                , Font.color gciBlue
                , Font.bold
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
                        , paddingXY 20 10
                        , Font.size 30
                        , Font.color white
                        , Font.bold
                        , Background.color gciBlue
                        ]
                        (text "What We Do")
                    )
                , Events.onMouseEnter (SimpleBtnHover 0)
                , Events.onMouseLeave (SimpleBtnUnHover 0)
                , htmlAttribute <| class "gciBtn"
                ]
                (text "What We Do")
    in
    column
        [ centerX
        , htmlAttribute <| id "boxes"
        , transparent (not animateSelf)
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
            , Font.size 32
            , Font.extraLight
            ]
            (text "What do we do? Great Technology.")
        , row [] (List.map box (List.indexedMap Tuple.pair content))
        , paragraph [ centerX, Font.light, Font.center, Font.size 28, width (px 900), padding 20 ] [ text "GCI provides solutions for otherwise obsolite electronic systems. Keeping assets fully operational for many decades in the future." ]
        ]


grayQuote : Int -> Bool -> Element msg
grayQuote w animateSelf =
    column
        [ width
            (px
                (if w > maxWidth then
                    (toFloat maxWidth * 0.9) |> ceiling

                 else
                    w
                )
            )
        , height (px 500)
        , centerX
        , Background.gradient { angle = degrees 180, steps = [ white, rgb255 214 218 219 ] }
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
                [ paddingXY 200 0
                , Font.alignLeft
                , Font.extraLight
                , Font.size 50
                , Font.color (rgb255 95 106 144)
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
                , Font.color (rgb255 95 106 144)
                , Font.size 20
                , transparent (not animateSelf)
                , paddingXY 200 30
                ]
                [ text "Global Circuit Innovation's expertise has a range of digital and analog security over many decades. This knowledge base is applied to develop electronic obsolescence solutions for legacy systems. Our device physics skills and experience enables us to provide environmental hardening for extremely high temperature applications." ]
            )
        ]
