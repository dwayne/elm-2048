module App.Data.Tile exposing
    ( Info
    , Tile
    , age
    , composite
    , decoder
    , encode
    , getPosition
    , is2048
    , merged
    , new
    , old
    , toInfo
    , toPoints
    )

import App.Data.Points as Points exposing (Points)
import App.Data.Tile.Position as Position exposing (Position)
import App.Data.Tile.Value as Value exposing (Value)
import Json.Decode as JD
import Json.Encode as JE


type Tile
    = Tile Config


type alias Config =
    { kind : Kind
    , id : Int
    , value : Value
    , position : Position
    }


type Kind
    = New
    | Composite
    | Merged Action
    | Old Action


type Action
    = Stay
    | MoveFrom Position


new : Int -> Value -> Position -> Tile
new id value position =
    Tile <| Config New id value position


composite : Int -> Value -> Position -> Tile
composite id value position =
    Tile <| Config Composite id value position


merged : Int -> Value -> Position -> Position -> Tile
merged id value from to =
    let
        action =
            if from == to then
                Stay

            else
                MoveFrom from
    in
    Tile <| Config (Merged action) id value to


old : Int -> Value -> Position -> Position -> Tile
old id value from to =
    let
        action =
            if from == to then
                Stay

            else
                MoveFrom from
    in
    Tile <| Config (Old action) id value to


getPosition : Tile -> Position
getPosition (Tile { position }) =
    position


is2048 : Tile -> Bool
is2048 (Tile { value }) =
    Value.is2048 value


age : Tile -> Maybe Tile
age (Tile config) =
    case config.kind of
        Merged _ ->
            Nothing

        _ ->
            Just <| Tile { config | kind = Old Stay }


type alias Info =
    { kind : String
    , id : Int
    , value : Value
    , from : Position
    , to : Position
    }


toInfo : Tile -> Info
toInfo (Tile { kind, id, value, position }) =
    let
        ( kindAsString, from ) =
            case kind of
                New ->
                    ( "new", position )

                Composite ->
                    ( "composite", position )

                Merged action ->
                    ( "merged"
                    , case action of
                        Stay ->
                            position

                        MoveFrom from_ ->
                            from_
                    )

                Old action ->
                    ( "old"
                    , case action of
                        Stay ->
                            position

                        MoveFrom from_ ->
                            from_
                    )
    in
    { kind = kindAsString
    , id = id
    , value = value
    , from = from
    , to = position
    }


toPoints : Tile -> Points
toPoints (Tile { kind, value }) =
    case kind of
        Composite ->
            Points.fromValue value

        _ ->
            Points.zero


encode : List Tile -> JE.Value
encode =
    let
        isEncodeable (Tile { kind }) =
            case kind of
                Merged _ ->
                    False

                _ ->
                    True
    in
    List.filter isEncodeable >> JE.list encodeTile


encodeTile : Tile -> JE.Value
encodeTile (Tile { id, value, position }) =
    JE.object
        [ ( "id", JE.int id )
        , ( "value", Value.encode value )
        , ( "position", Position.encode position )
        ]


decoder : JD.Decoder Tile
decoder =
    JD.map3 new
        (JD.field "id" JD.int)
        (JD.field "value" Value.decoder)
        (JD.field "position" Position.decoder)
