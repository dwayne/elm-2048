module App.View.Grid exposing (view)


import App.Data.Grid as Grid exposing (Grid)
import App.Data.Tile.Value as Value exposing (Value)
import Html as H
import Html.Attributes as HA
import Html.Keyed as HK


view : Grid -> H.Html msg
view grid =
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
        , viewGridTiles <| Grid.toTiles grid
        ]
    ]


viewGridTiles : List Grid.Tile -> H.Html msg
viewGridTiles =
  HK.node "div" [ HA.class "grid_tiles" ] << List.map viewGridTile


viewGridTile : Grid.Tile -> (String, H.Html msg)
viewGridTile tile =
  let
    info =
      case tile of
        Grid.New { id, value, position } ->
          { status = "new"
          , id = id
          , value = value
          , position = position
          }

    (r, c) =
      info.position
  in
  ( String.fromInt info.id
  , H.div
      [ HA.class "grid__tile"
      , HA.class <| "grid__tile--" ++ String.fromInt r ++ "-" ++ String.fromInt c
      ]
      [ viewTile info.status info.value ]
  )


viewTile : String -> Value -> H.Html msg
viewTile status value =
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
    , HA.class <| "tile--" ++ status
    , HA.class valueClassName
    ]
    [ H.div [ HA.class "tile__value" ] [ H.text valueAsString ] ]
