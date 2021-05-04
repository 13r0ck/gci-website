module Pages.Whoweare exposing (Model, Msg, page)

import Element exposing (..)
import Gen.Params.Whatwedo exposing (Params)
import Page
import Request
import Shared exposing (navbar)
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


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        NavBar cmd ->
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
        [ inFront (navbar shared.storage.navHoverTracker shared.temp.navbarDisplay NavBar) ]
    , element =
        column [ width fill, height (px 500) ] []
    }
