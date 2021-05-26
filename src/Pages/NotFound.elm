module Pages.NotFound exposing (Model, Msg, page)

import Browser.Events
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Gen.Params.NotFound exposing (Params)
import Html.Attributes exposing (id)
import Page
import Palette exposing (FontSize(..), black, fontSize, gciBlue, gciBlueLight, maxWidth, warning, white)
import Ports exposing (recvScroll)
import Request
import Shared exposing (contactUs, footer, navbar, reset)
import Storage exposing (NavBarDisplay(..))
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
    { localShared : Shared.Model }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { localShared = reset shared }, Effect.none )



-- UPDATE


type Msg
    = Scrolled Int
    | ModifyLocalShared Shared.Model
    | WindowResized Int Int
    | OpenContactUs


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Scrolled distance ->
            let
                modifyNavbarDisplay state =
                    model.localShared |> (\l -> {l| navbarDisplay = state, scrolledDistance = distance, showMobileNav = (if state == Hide then False else l.showMobileNav)})
            in
            ((if abs (distance - model.localShared.scrolledDistance) > 3 then
                if distance > model.localShared.scrolledDistance then
                    {model | localShared = modifyNavbarDisplay Hide}
                else
                    {model | localShared = modifyNavbarDisplay Enter}
            else
                model
            )
            , Effect.none
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
        device =
            shared.device.class

        h =
            shared.height

        w =
            shared.width
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
            [ column [ width (fill |> maximum (min w maxWidth)), centerX, spacing 25, height (0.66 * toFloat h |> round |> px) ]
                [ column
                    [ Region.heading 1
                    , centerX
                    , centerY
                    ]
                    [ el [ Font.extraLight, centerX, centerY, Font.extraLight, fontSize device Xlg ] (text "Page Not Found!")
                    , paragraph [ fontSize device Md, centerX, Font.center ] [ text "Maybe this link was to our old website?" ]
                    , el [ height (px 50) ] none
                    , link
                        [ Background.color gciBlue
                        , Border.rounded 100
                        , centerX
                        , centerY
                        , padding 30
                        , width (fill |> maximum w)
                        , Font.color white
                        , Font.center
                        , fontSize device Md
                        , mouseOver [ Background.color gciBlueLight ]
                        ]
                        { url = "/", label = text "Return to Home" }
                    ]
                ]
            , footer model.localShared ModifyLocalShared
            ]
    }
