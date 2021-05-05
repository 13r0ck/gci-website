module Pages.Obsolescence exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Region as Region
import Gen.Params.Whatwedo exposing (Params)
import Html.Attributes exposing (id)
import Page
import Palette exposing (maxWidth)
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
    { title = "GCI - Authorized Reverse Engineering IC Solutions for Obsolescence and High Temperature Environments"
    , attributes =
        [ inFront (navbar shared.storage.navHoverTracker shared.temp.navbarDisplay NavBar)
        , inFront
            (if shared.storage.openContactUs then
                contactUs shared.storage.contactDialogState shared.temp.address ContactUs

             else
                none
            )
        ]
    , element =
        column [ width fill, Region.mainContent ]
            [ column [ width (fill |> maximum maxWidth), centerX, spacing 25 ]
                [ row [ htmlAttribute <| id "placeholder", width fill, Background.gradient { angle = degrees 0, steps = [ rgb 1 0 0, rgb 0 1 0, rgb 0 0 1 ] }, height (px 1000) ] []
                ]
            , footer shared.temp.certifications shared.temp.address shared.storage.navHoverTracker shared.temp.socialMedia shared.temp.currentYear Footer
            ]
    }
