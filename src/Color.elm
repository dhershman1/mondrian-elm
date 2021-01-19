module Color exposing (..)

import Random exposing (Seed)


type Color
    = Red
    | Blue
    | Yellow
    | Orange
    | Transparent


toString : Color -> String
toString c =
    case c of
        Red ->
            "red"

        Blue ->
            "blue"

        Yellow ->
            "yellow"

        Orange ->
            "orange"

        Transparent ->
            "transparents"


fromString : String -> Color
fromString c =
    case c of
        "red" ->
            Red

        "blue" ->
            Blue

        "yellow" ->
            Yellow

        "orange" ->
            Orange

        _ ->
            Transparent


randomColor : Seed -> ( Color, Seed )
randomColor seed =
    Random.step
        (Random.weighted ( 25, Red )
            [ ( 25, Blue )
            , ( 25, Yellow )
            , ( 25, Orange )
            ]
        )
        seed
