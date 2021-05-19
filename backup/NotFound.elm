module Pages.NotFound exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Gen.Params.NotFound exposing (Params)
import Html.Attributes exposing (id)
import Page
import Palette exposing (FontSize(..), black, fontSize, gciBlue, gciBlueLight, maxWidth, warning, white)
import Request
import Shared exposing (contactUs, footer, navbar)
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
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NavBar (Storage -> Cmd Msg)
    | ContactUs (String -> Storage -> Cmd Msg) String
    | Footer (Storage -> Cmd Msg)


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        NavBar cmd ->
            ( model, cmd storage )

        ContactUs cmd str ->
            ( model, cmd str storage )

        Footer cmd ->
            ( model, cmd storage )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        device =
            shared.temp.device.class

        h =
            shared.temp.height

        w =
            shared.temp.width
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
            , footer shared Footer
            ]
    }
