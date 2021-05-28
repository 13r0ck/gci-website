module Pages.Electronics exposing (Model, Msg, page)

import Browser.Dom exposing (Viewport)
import Browser.Events
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region exposing (description)
import Gen.Params.Electronics exposing (Params)
import Html exposing (br, div, iframe)
import Html.Attributes exposing (attribute, class, id, property, src, style)
import Json.Encode as Encode
import Page
import Pages.Home_ exposing (AnimationState, When(..), onScreenItemtoCmd, updateElement)
import Palette exposing (FontSize(..), black, fontSize, gciBlue, maxWidth, warning, white)
import Ports exposing (disableScrolling, recvScroll)
import Request
import Shared exposing (acol, ael, contactUs, footer, navbar, reset)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage exposing (NavBarDisplay(..))
import Task
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
    { showVimeo : Bool
    , simpleBtnHoverTracker : List SimpleBtn
    , animationTracker : Dict String AnimationState
    , subTexts : List SubText
    , localShared : Shared.Model
    }


type alias SubText =
    { id : Int
    , title : String
    , image : String
    , description : String
    , text : String
    }


type alias SimpleBtn =
    { id : Int
    , name : String
    , link : String
    , hovered : Bool
    , message : Maybe Msg
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { showVimeo = False
      , simpleBtnHoverTracker =
            [ SimpleBtn 0 "Play" "#" False (Just OpenVimeo)
            , SimpleBtn 1 "Intellectual property" "/ip" False Nothing
            , SimpleBtn 2 "What we do" "/#whatwedo" False Nothing
            , SimpleBtn 3 "Contact Us" "" False (Just OpenContactUs)
            , SimpleBtn 4 "Technical Papers" "/papers" False Nothing
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
            [ SubText 1 "Something" "/img/subtext6.jpg" "" "Decades of experience in failure analysis combined with an engineering team committed to innovative solutions, allows GCI to develop solutions for a wide array of electronic applications."
            , SubText 2 "Sub text" "/img/subtext5.jpg" "" "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut."
            , SubText 3 "Sub text" "/img/subtext4.jpg" "" "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut."
            ]
      , localShared = reset shared
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = OpenVimeo
    | CloseVimeo
    | SimpleBtnHover Int
    | SimpleBtnUnHover Int
    | Scrolled Int
    | GotElement String (Result Browser.Dom.Error Browser.Dom.Element)
    | OpenContactUs
    | WindowResized Int Int
    | ModifyLocalShared Shared.Model


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenVimeo ->
            ( { model | showVimeo = True }, disableScrolling True |> Effect.fromCmd )

        CloseVimeo ->
            ( { model | showVimeo = False }, disableScrolling False |> Effect.fromCmd )

        SimpleBtnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setHovered id) model.simpleBtnHoverTracker }, Effect.none )

        SimpleBtnUnHover id ->
            ( { model | simpleBtnHoverTracker = List.indexedMap (setUnHovered id) model.simpleBtnHoverTracker }, Effect.none )

        Scrolled distance ->
            let
                modifyNavbarDisplay state =
                    model.localShared
                        |> (\l ->
                                { l
                                    | navbarDisplay = state
                                    , scrolledDistance = distance
                                    , showMobileNav =
                                        if state == Hide then
                                            False

                                        else
                                            l.showMobileNav
                                }
                           )
            in
            ( if abs (distance - model.localShared.scrolledDistance) > 3 then
                if distance > model.localShared.scrolledDistance then
                    { model | localShared = modifyNavbarDisplay Hide }

                else
                    { model | localShared = modifyNavbarDisplay Enter }

              else
                model
            , Effect.batch
                (List.map animationTrackerToCmd (List.filter (\( _, v ) -> v.shouldAnimate == False) (Dict.toList model.animationTracker)))
            )

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

        GotElement id element ->
            case element of
                Ok e ->
                    ( { model | animationTracker = Dict.fromList (List.map (updateElement id e) (Dict.toList model.animationTracker)) }, Effect.none )

                Err _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ recvScroll Scrolled
        , Browser.Events.onResize WindowResized
        ]



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        h =
            shared.height

        w =
            shared.width

        device =
            shared.device.class

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
                        [ width (fillPortion 4)
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
                        (if item.description == "" then
                            image
                                [ centerX
                                , centerY
                                , width fill
                                ]
                                { src = item.image, description = item.title }

                         else
                            el [ inFront (el [ fontSize device Xsm, Font.center, Font.light, padding 10, width fill, alignBottom, Background.color (rgba 1 1 1 0.85) ] (text item.description)) ]
                                (image
                                    [ centerX
                                    , centerY
                                    , width fill
                                    ]
                                    { src = item.image, description = item.title }
                                )
                        )

                content =
                    paragraph [ width (fillPortion 3), fontSize device Sm, Font.light ] (List.concat (List.intersperse [ html <| br [] [], html <| br [] [] ] (item.text |> String.split "\n" |> List.map (\t -> [ text t ]))))
            in
            acol
                (if shouldAnimate (String.fromInt item.id) model then
                    Animation.fromTo
                        { duration = 500
                        , options = []
                        }
                        [ P.opacity 0, P.y 100 ]
                        [ P.opacity 100, P.y 0 ]

                 else
                    Animation.empty
                )
                [ width fill, height fill, spacing 20, htmlAttribute <| id (String.fromInt item.id), transparent (not (shouldAnimate (String.fromInt item.id) model)) ]
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
        [ inFront (navbar model.localShared ModifyLocalShared)
        , inFront
            (if shared.contactDialogState.showContactUs then
                contactUs model.localShared ModifyLocalShared

             else
                none
            )
        ]
    , element =
        column [ width fill, Region.mainContent, clip ]
            [ column [ width (fill |> maximum maxWidth), centerX, spacing 50 ]
                [ head model.localShared
                , column
                    [ paddingXY
                        (if isPhone then
                            10

                         else
                            100
                        )
                        0
                    , width (fill |> maximum maxWidth)
                    , spacing 100
                    ]
                    (mainText shared (shouldAnimate "mainText" model)
                        :: List.map subtext model.subTexts
                    )
                , bottomButtons shared (List.filter (\b -> b.id > 0) model.simpleBtnHoverTracker) (shouldAnimate "bottomButtons" model)
                ]
            , footer model.localShared ModifyLocalShared
            ]
    }


head : Shared.Model -> Element Msg
head shared =
    let
        h =
            shared.height

        w =
            shared.width

        device =
            shared.device.class

        isPhone =
            device == Phone
    in
    image
        [ width fill
        , height (px h)
        , clip
        , inFront (el [ width fill, height fill, Background.color (rgba 0 0 0 0.3) ] none)
        , inFront
            (paragraph
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
                (List.intersperse (html <| br [] []) [ text "GCI", text "Delivers", text "Electronic Solutions." ])
            )
        ]
        { src = "/img/electronics.jpg", description = "Photo of circuit manufacturing." }


mainText : Shared.Model -> Bool -> Element Msg
mainText shared animateSelf =
    let
        device =
            shared.device.class

        isPhone =
            device == Phone

        w =
            shared.width
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
        [ paragraph [ Font.extraLight, Region.heading 1, fontSize device Xlg ] [ text "GCI’s technology can generate a broad range of solutions." ]
        , paragraph [ spacing 10, fontSize device Sm, Font.light, htmlAttribute <| id "mainText" ]
            [ text "Our engineering experience enables us to provide solutions for electronics ranging from a resistor assembly on a B-52 Windshield Wiper Speed Control to a Variable Frequency Driver solution in a Patriot Missile battery ECU."
            , html <| br [] []
            , html <| br [] []
            ]
        ]


bottomButtons : Shared.Model -> List SimpleBtn -> Bool -> Element Msg
bottomButtons shared btns animateSelf =
    let
        h =
            shared.height

        w =
            shared.width

        device =
            shared.device.class

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


animationTrackerToCmd : ( String, AnimationState ) -> Effect Msg
animationTrackerToCmd ( k, _ ) =
    Task.attempt (GotElement k) (Browser.Dom.getElement k) |> Effect.fromCmd


shouldAnimate : String -> Model -> Bool
shouldAnimate id model =
    case Dict.get id model.animationTracker of
        Just state ->
            state.shouldAnimate

        Nothing ->
            False
