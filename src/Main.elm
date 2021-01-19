module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Random
import Task
import Time exposing (Posix)


type alias Model =
    { seed : Random.Seed
    , intialInt : Int
    }


type Msg
    = InitRandom Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Random.initialSeed 0) 0
    , Cmd.batch
        [ Task.perform InitRandom Time.now
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitRandom now ->
            let
                time =
                    Time.posixToMillis now

                seed =
                    Random.initialSeed time
            in
            ( { model
                | seed = seed
                , intialInt = time
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


randomHeightAndWidth : Random.Seed -> ( ( Int, Int ), Random.Seed )
randomHeightAndWidth seed =
    let
        ( height, nextSeed ) =
            Random.step (Random.int 1 10) seed

        ( width, finalSeed ) =
            Random.step (Random.int 1 10) nextSeed
    in
    ( ( height * 10, width * 10 ), finalSeed )


randomPosition : Int -> Int -> Random.Seed -> ( Int, Int )
randomPosition w h seed =
    let
        ( right, nextSeed ) =
            Random.step (Random.int 1 10) seed

        ( bottom, _ ) =
            Random.step (Random.int 1 10) nextSeed

        pos =
            ( clamp 0 (100 - w) (right * 10), clamp 0 (100 - h) (bottom * 10) )
    in
    pos


view : Model -> Html Msg
view model =
    let
        ( ( width, height ), nextSeed ) =
            randomHeightAndWidth model.seed

        ( right, bottom ) =
            randomPosition width height nextSeed
    in
    div [ class "wrapper" ]
        [ div [] [ text ("Time Int: " ++ String.fromInt model.intialInt) ]
        , div [] [ text ("Height: " ++ String.fromInt height) ]
        , div [] [ text ("Width: " ++ String.fromInt width) ]
        , div [] [ text ("Right: " ++ String.fromInt right) ]
        , div [] [ text ("Bottom: " ++ String.fromInt bottom) ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
