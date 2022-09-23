module App.View.Grid exposing (view)


import App.Data.Grid as Grid exposing (Grid)
import App.Data.Tile.Value as Value exposing (Value)
import Html as H
import Html.Attributes as HA
import Html.Keyed as HK


view : Int -> Grid -> H.Html msg
view currentId grid =
  H.div [ HA.class "grid" ]
    [ H.div [ HA.class "grid__background" ]
        [ H.div [ HA.class "grid__cells" ]
            [ H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            , H.div [ HA.class "grid__cell" ] []
            ]
        , viewGridTiles currentId <| Grid.toTransparentTiles grid
        ]
    ]


viewGridTiles : Int -> List Grid.TransparentTile -> H.Html msg
viewGridTiles currentId =
  HK.node "div" [ HA.class "grid_tiles" ] << List.indexedMap (viewGridTile currentId)


viewGridTile : Int -> Int -> Grid.TransparentTile -> (String, H.Html msg)
viewGridTile currentId index { kind, value, position } =
  let
    (r, c) =
      position
  in
  ( String.fromInt <| currentId + index
  , H.div
      [ HA.class "grid__tile"
      , HA.class <| "grid__tile--" ++ String.fromInt r ++ "-" ++ String.fromInt c
      ]
      [ viewTile kind value ]
  )


viewTile : String -> Value -> H.Html msg
viewTile kind value =
  let
    valueAsString =
      Value.toString value

    valueAsInt =
      Value.toInt value

    valueClassName =
      (++) "tile--" <|
        if valueAsInt <= 2048 then
          valueAsString
        else if valueAsInt <= 8192 then
          "super-1"
        else if valueAsInt <= 65536 then
          "super-2"
        else
          "super-3"
  in
  H.div
    [ HA.class "tile"
    , HA.class <| "tile--" ++ kind
    , HA.class valueClassName
    ]
    [ H.div [ HA.class "tile__value" ] [ H.text valueAsString ] ]
