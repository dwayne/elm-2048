module App.Data.Tile.Position exposing
    ( Position
    , availablePositions
    , decoder
    , encode
    , selectAtMost2
    )

import App.Lib.Random exposing (selectAtMostN)
import Json.Decode as JD
import Json.Encode as JE
import Random
import Set exposing (Set)


type alias Position =
    ( Int, Int )


availablePositions : List Position -> List Position
availablePositions unavailablePositions =
    unavailablePositions
        |> Set.fromList
        |> Set.diff allPositions
        |> Set.toList


selectAtMost2 : List Position -> Random.Generator (List Position)
selectAtMost2 =
    availablePositions >> selectAtMostN 2 >> Random.map Tuple.first


allPositions : Set Position
allPositions =
    Set.fromList
        [ ( 1, 1 )
        , ( 1, 2 )
        , ( 1, 3 )
        , ( 1, 4 )
        , ( 2, 1 )
        , ( 2, 2 )
        , ( 2, 3 )
        , ( 2, 4 )
        , ( 3, 1 )
        , ( 3, 2 )
        , ( 3, 3 )
        , ( 3, 4 )
        , ( 4, 1 )
        , ( 4, 2 )
        , ( 4, 3 )
        , ( 4, 4 )
        ]


encode : Position -> JE.Value
encode ( row, col ) =
    JE.object
        [ ( "row", JE.int row )
        , ( "col", JE.int col )
        ]


decoder : JD.Decoder Position
decoder =
    JD.map2 Tuple.pair
        (JD.field "row" dimensionDecoder)
        (JD.field "col" dimensionDecoder)


dimensionDecoder : JD.Decoder Int
dimensionDecoder =
    JD.int
        |> JD.andThen
            (\n ->
                if n >= 1 && n <= 4 then
                    JD.succeed n

                else
                    JD.fail <| "expected one of 1, 2, 3, or 4: " ++ String.fromInt n
            )
