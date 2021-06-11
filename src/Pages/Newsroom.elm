module Pages.Newsroom exposing (Model, Msg, page)

import Browser.Dom exposing (Viewport)
import Browser.Events
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Gen.Params.Newsroom exposing (Params)
import Html exposing (br)
import Html.Attributes exposing (id)
import Http exposing (Error(..))
import Json.Decode as Json
import Json.Encode as Encode
import Page
import Pages.Home_ exposing (AnimationState, When(..), onScreenItemtoCmd, updateElement)
import Palette exposing (FontSize(..), black, fontSize, gciBlue, gciBlueLight, maxWidth, warning, white)
import Ports exposing (idLoaded, recvScroll)
import Request
import Shared exposing (FormResponse, acol, contactUs, footer, navbar, reset)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Storage exposing (NavBarDisplay(..), SendState(..))
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
    { localShared : Shared.Model
    , posts : List Posts
    , postRecvError : Bool
    , animationTracker : Dict String AnimationState
    , loadingState : LoadingState
    , postIndex : Int
    }


type LoadingState
    = StartLoading
    | RecvPosts
    | RecvImg
    | LoadingFailed
    | LoadingDone


type alias Post =
    { id : Int
    , title : String
    , images : List String
    , content : String
    , posttime : String
    }


type alias Posts =
    { posts : List Post
    , show : Bool
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { localShared = reset shared
      , posts = []
      , postRecvError = False
      , animationTracker =
            Dict.fromList []
      , loadingState = StartLoading
      , postIndex = 0
      }
    , Http.post
        { url = "http://localhost:8000/newsroom/posts?i=0&range=3"
        , body = Http.emptyBody
        , expect =
            Http.expectJson GotPosts
                (Json.list
                    (Json.map5 Post
                        (Json.field "id" Json.int)
                        (Json.field "title" Json.string)
                        (Json.field "images" (Json.list Json.string))
                        (Json.field "content" Json.string)
                        (Json.field "posttime" Json.string)
                    )
                )
        }
        |> Effect.fromCmd
    )



-- UPDATE


type Msg
    = Scrolled Int
    | ModifyLocalShared Shared.Model
    | WindowResized Int Int
    | OpenContactUs
    | Submited (Result Http.Error FormResponse)
    | GotPosts (Result Http.Error (List Post))
    | GotElement String (Result Browser.Dom.Error Browser.Dom.Element)
    | IdLoaded String
    | IdFailed String


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
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
                    { model | localShared = modifyNavbarDisplay Hide, animationTracker = Dict.update "spinner" (Maybe.map (\_ -> AnimationState (PercentOfViewport 1) False)) model.animationTracker }

                else
                    { model | localShared = modifyNavbarDisplay Enter }

              else
                model
            , Effect.batch
                ((if shouldAnimate "spinner" model && not (model.loadingState == LoadingDone) then
                    Http.post
                        { url = "http://localhost:8000/newsroom/posts?i=" ++ String.fromInt model.postIndex ++ "&range=3"
                        , body = Http.emptyBody
                        , expect =
                            Http.expectJson GotPosts
                                (Json.list
                                    (Json.map5 Post
                                        (Json.field "id" Json.int)
                                        (Json.field "title" Json.string)
                                        (Json.field "images" (Json.list Json.string))
                                        (Json.field "content" Json.string)
                                        (Json.field "posttime" Json.string)
                                    )
                                )
                        }
                        |> Effect.fromCmd

                  else
                    Effect.none
                 )
                    :: List.map animationTrackerToCmd (List.filter (\( _, v ) -> v.shouldAnimate == False) (Dict.toList model.animationTracker))
                )
            )

        ModifyLocalShared newSharedState ->
            let
                nullable : Maybe String -> Encode.Value
                nullable a =
                    case a of
                        Nothing ->
                            Encode.null

                        Just str ->
                            Encode.string str
            in
            ( { model | localShared = newSharedState }
            , if not (newSharedState.contactDialogState == model.localShared.contactDialogState) then
                Effect.batch
                    (if newSharedState.contactDialogState.send == Send then
                        [ Shared.UpdateModel newSharedState |> Effect.fromShared
                        , newSharedState.contactDialogState |> Storage.toJson |> Ports.save |> Effect.fromCmd
                        , Http.post
                            { url = "https://formspree.io/f/xdoygpvp"
                            , body =
                                Http.jsonBody <|
                                    Encode.object
                                        [ ( "name", Encode.string newSharedState.contactDialogState.name )
                                        , ( "email", nullable newSharedState.contactDialogState.email )
                                        , ( "telephone", nullable newSharedState.contactDialogState.phone )
                                        , ( "message", nullable newSharedState.contactDialogState.message )
                                        ]
                            , expect = Http.expectJson Submited (Json.map2 FormResponse (Json.field "next" Json.string) (Json.field "ok" Json.bool))
                            }
                            |> Effect.fromCmd
                        ]

                     else
                        [ Shared.UpdateModel newSharedState |> Effect.fromShared
                        , newSharedState.contactDialogState |> Storage.toJson |> Ports.save |> Effect.fromCmd
                        ]
                    )

              else
                Shared.UpdateModel newSharedState |> Effect.fromShared
            )

        Submited response ->
            let
                newSharedState =
                    model.localShared
                        |> (\local ->
                                { local
                                    | contactDialogState =
                                        local.contactDialogState
                                            |> (\state ->
                                                    { state
                                                        | send =
                                                            case response of
                                                                Ok _ ->
                                                                    SendOk

                                                                Err _ ->
                                                                    SendError
                                                    }
                                               )
                                }
                           )
            in
            ( { model | localShared = newSharedState }
            , Effect.batch
                [ Shared.UpdateModel newSharedState |> Effect.fromShared
                , newSharedState.contactDialogState |> Storage.toJson |> Ports.save |> Effect.fromCmd
                ]
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

        GotPosts response ->
            case response of
                Ok newPosts ->
                    ( if List.isEmpty newPosts then
                        ( { model | loadingState = LoadingDone}, Effect.none )
                    else
                        ( { model | posts = model.posts ++ [ Posts newPosts False ], postIndex = model.postIndex + List.length newPosts, loadingState = RecvPosts }, newPosts |> List.head |> Maybe.withDefault (Post 1 "" [ "" ] "" "") |> .images |> List.head |> Maybe.withDefault "" |> Ports.waitForId |> Effect.fromCmd )
                    )

                Err _ ->
                    ( { model | postRecvError = True, loadingState = LoadingFailed }, Effect.none )

        IdLoaded _ ->
            ( { model
                | posts = model.posts |> List.map (\p -> { p | show = True })
                , loadingState = RecvImg
                , animationTracker =
                    if Dict.isEmpty model.animationTracker then
                        Dict.fromList
                            (( "spinner", AnimationState (PercentOfViewport 1) False )
                                :: List.indexedMap
                                    (\i p ->
                                        ( String.fromInt p.id
                                        , AnimationState (PercentOfViewport 20)
                                            (if i == 0 then
                                                True

                                             else
                                                False
                                            )
                                        )
                                    )
                                    (model.posts |> List.foldl (\a b -> b ++ a.posts) [])
                            )

                    else
                        Dict.union model.animationTracker
                            (model.posts |> List.filter (\p -> not p.show) |> List.foldl (\a b -> b ++ a.posts) []
                                |> List.indexedMap
                                    (\i p ->
                                        ( String.fromInt p.id
                                        , AnimationState (PercentOfViewport 20)
                                            (if i == 0 then
                                                True

                                             else
                                                False
                                            )
                                        )
                                    )
                                |> Dict.fromList
                            )
              }
            , Effect.none
            )

        IdFailed id ->
            ( model, Ports.waitForId id |> Effect.fromCmd )

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
        , idLoaded IdLoaded
        , Ports.idFailed IdFailed
        ]



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        device =
            shared.device.class

        h =
            shared.height

        w =
            shared.width

        isPhone =
            shared.device.class == Phone

        post item =
            let
                img =
                    el
                        [ width fill
                        , clip
                        , centerY
                        , htmlAttribute <| id (String.fromInt item.id)
                        , Border.rounded 10
                        , Background.image "/img/logo_sans_ring.svg"
                        , height (shrink |> minimum 90)
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
                            , htmlAttribute <| id (item.images |> List.head |> Maybe.withDefault "")
                            ]
                            { src = "http://localhost:8000/newsroom/images/" ++ (List.head item.images |> Maybe.withDefault "logo_sans_ring.svg"), description = "" }
                        )

                content =
                    column [ width fill, spacing 10 ]
                        [ paragraph [ Region.heading 3, Font.extraLight, fontSize device Lg ] [ text item.title ]
                        , paragraph [ fontSize device Xsm, Font.color (rgb 0.1 0.1 0.13) ] [ item.posttime |> String.split "T" |> List.head |> Maybe.withDefault "" |> String.split "-" |> prettyDate |> text ]
                        , paragraph [ width fill, fontSize device Sm, Font.light ] (List.concat (List.intersperse [ html <| br [] [] ] (item.content |> String.split "\n" |> List.map (\t -> [ text t ]))))
                        ]
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
                [ column
                    [ width fill, spacing 20 ]
                    [ img, content ]
                ]

        loadingSpinner =
            column
                [ centerX
                , height
                    (if (List.any (\p -> p.show) model.posts) then
                        shrink

                     else
                        px h
                    )
                , htmlAttribute <| id "spinner"
                ]
                [ image
                    [ width (px 120)
                    , height
                        (px
                            (if model.loadingState == LoadingFailed || model.loadingState == LoadingDone then
                                0

                             else
                                120
                            )
                        )
                    , clip
                    , centerX
                    , inFront (image [ width (px 80), height (px 80), centerX, centerY ] { src = "/img/logo_sans_text.svg", description = "logo" })
                    ]
                    { src = "/img/loading.svg", description = "Loading..." }
                , el [ centerX, Font.center, padding 10]
                    (text
                        (case model.loadingState of
                            StartLoading ->
                                "Loading..."

                            RecvPosts ->
                                "Finishing up..."

                            RecvImg ->
                                ""

                            LoadingFailed ->
                                "Failed. Please check your internet connection and try again."
                            LoadingDone ->
                                "Showing all posts."
                        )
                    )
                ]

        posts =
            column [ width fill ]
                (List.map
                    (\postList ->
                        column
                            (if postList.show then
                                [ width fill, spacing 100, paddingEach {top = 0, bottom = 100, left = 0, right = 0} ]

                             else
                                [ height (px 0), clip ]
                            )
                            (List.map (\p -> post p) postList.posts)
                    )
                    model.posts
                )
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
        , clip
        ]
    , element =
        column [ width fill, Region.mainContent ]
            [ column [ width (fill |> maximum (min w maxWidth)), centerX, spacing 25 ]
                [ column
                    [ paddingXY
                        (if isPhone then
                            10

                         else
                            100
                        )
                        0
                    , centerX
                    , width (fill |> maximum (toFloat maxWidth * 0.5 |> round))
                    , spacing 50
                    ]
                    [ el [ height (px 50) ] none
                    , el [ Region.heading 1, Font.extraLight, Font.extraLight, fontSize device Xlg, centerX ] (text "Newsroom")
                    , posts
                    , loadingSpinner
                    ]
                ]
            , footer model.localShared ModifyLocalShared
            ]
    }


shouldAnimate : String -> Model -> Bool
shouldAnimate id model =
    case Dict.get id model.animationTracker of
        Just state ->
            state.shouldAnimate

        Nothing ->
            False


animationTrackerToCmd : ( String, AnimationState ) -> Effect Msg
animationTrackerToCmd ( k, _ ) =
    Task.attempt (GotElement k) (Browser.Dom.getElement k) |> Effect.fromCmd


prettyDate : List String -> String
prettyDate list =
    let
        year =
            List.head list |> Maybe.withDefault ""

        month =
            List.tail list |> Maybe.withDefault [ "", "" ] |> List.head |> Maybe.withDefault ""

        day =
            List.tail list |> Maybe.withDefault [ "", "" ] |> List.tail |> Maybe.withDefault [ "" ] |> List.head |> Maybe.withDefault ""
    in
    (case month of
        "01" ->
            "January"

        "02" ->
            "Febuary"

        "03" ->
            "March"

        "04" ->
            "April"

        "05" ->
            "May"

        "06" ->
            "June"

        "07" ->
            "July"

        "08" ->
            "August"

        "09" ->
            "September"

        "10" ->
            "October"

        "11" ->
            "November"

        "12" ->
            "December"

        _ ->
            ""
    )
        ++ " "
        ++ day
        ++ ", "
        ++ year
