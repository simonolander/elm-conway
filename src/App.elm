module App exposing (main)

import Array exposing (Array)
import Collage
import Element
import Html
import Types exposing (..)
import World exposing (stepWorld)
import Time

init : (Model, Cmd Msg)
init =
    let
        world = World.emptyWorld 70 40
    in
        (
            { state = Paused
            , world = world
            },
            World.randomizeWorld world
        )


view : Model -> Html.Html msg
view model =
    Html.div
        []
        [ Collage.collage 1400 800
            [ World.renderWorld model.world
                |> Collage.scale 20
                |> Collage.move (-690, -390)
            ] |> Element.toHtml
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            ( { model | world = stepWorld model.world }, Cmd.none)
        ReceiveWorld world ->
            ( { model | world = world }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (100*Time.millisecond) Tick
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

