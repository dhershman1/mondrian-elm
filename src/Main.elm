module Main exposing (main)

import Browser
import Html exposing (Html, div, main_, section, span, text)
import Html.Attributes exposing (class, style, width)
import Random
import Task
import Time exposing (Posix)


type alias Model =
    { seed : Random.Seed
    , intialInt : Int
    , blocks : Int
    }


type Msg
    = InitRandom Posix


init : Int -> ( Model, Cmd Msg )
init blockCount =
    ( Model (Random.initialSeed 0) 0 blockCount
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


randomPosition : Int -> Int -> Random.Seed -> ( ( Int, Int ), Random.Seed )
randomPosition w h seed =
    let
        ( right, nextSeed ) =
            Random.step (Random.int 1 10) seed

        ( bottom, finalSeed ) =
            Random.step (Random.int 1 10) nextSeed

        pos =
            ( clamp 0 (100 - w) (right * 10), clamp 0 (100 - h) (bottom * 10) )
    in
    ( pos, finalSeed )


createDivs : Int -> List (Html Msg) -> Random.Seed -> List (Html Msg)
createDivs count divs seed =
    if count > 0 then
        let
            newCount =
                count - 1

            ( ( width, height ), nextSeed ) =
                randomHeightAndWidth seed

            ( ( right, bottom ), finalSeed ) =
                randomPosition width height nextSeed

            d =
                div
                    [ style "width" (String.fromInt width ++ "%")
                    , style "height" (String.fromInt height ++ "%")
                    , style "right" (String.fromInt right ++ "%")
                    , style "bottom" (String.fromInt bottom ++ "%")
                    ]
                    []
        in
        createDivs newCount (d :: divs) finalSeed

    else
        divs


view : Model -> Html Msg
view model =
    main_ [ class "wrapper" ]
        [ span [ class "seed" ] [ text ("Current Seed: " ++ String.fromInt model.intialInt) ]
        , section [ class "blocks" ] (createDivs model.blocks [] model.seed)
        ]


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
