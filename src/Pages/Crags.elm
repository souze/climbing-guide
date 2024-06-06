module Pages.Crags exposing (Model, Msg, page)

import Bridge
import ClimbTypes exposing (Crag, Route, Sector)
import Components.IconButtons
import Effect exposing (Effect)
import Element exposing (Element)
import Element.Font as Font
import Html
import Lamdera
import Page exposing (Page)
import Route as PageRoute
import Shared
import String exposing (replace)
import View exposing (View)


page : Shared.Model -> PageRoute.Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared.crags
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp
    | AddCrag


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        AddCrag ->
            ( model
            , Effect.sendCmd <| Lamdera.sendToBackend Bridge.AddCrag
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Maybe (List Crag) -> Model -> View Msg
view crags model =
    View "Klippor" [] (viewBody crags)


viewBody : Maybe (List Crag) -> Element Msg
viewBody maybeCrags =
    case maybeCrags of
        Just crags ->
            Element.column
                [ Element.padding 50
                , Element.spacing 30
                ]
                ((crags |> List.map viewCrag)
                    ++ [ Components.IconButtons.addButton "Lägg till ny klippa" AddCrag
                       ]
                )

        Nothing ->
            Element.text "Laddar klippor..."


viewCrag : Crag -> Element Msg
viewCrag crag =
    Element.link
        [ Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        , Font.size 32
        ]
        { url = "crags/" ++ String.fromInt crag.id
        , label = Element.text (crag.name |> fillEmpty "Odöpt klippa")
        }


fillEmpty : String -> String -> String
fillEmpty replacement original =
    if String.trim original == "" then
        replacement

    else
        original
