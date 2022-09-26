module App.View.Grid exposing
  ( State, init
  , Msg, update
  , subscriptions
  , view
  )


import Animation exposing (Animation)
import App.Data.Grid as Grid exposing (Grid)
import App.Data.Tile as Tile exposing (Tile)
import App.Data.Tile.Value as Value exposing (Value)
import Browser.Events as BE
import Html as H
import Html.Attributes as HA
import Html.Keyed as HK


type State =
  State
    { clock : Animation.Clock
    , animatedTiles : List AnimatedTile
    }


type alias AnimatedTile =
  { factor : Animation
  , tile : Tile
  }


init : Grid -> State
init grid =
  State
    { clock = 0
    , animatedTiles =
        grid
          |> Grid.toTiles
          |> List.map toAnimatedTile
    }


toAnimatedTile : Tile -> AnimatedTile
toAnimatedTile tile =
  { factor =
      Animation.animation 0
        |> Animation.duration gridTileMoveDuration
  , tile = tile
  }


gridTileMoveDuration : Float
gridTileMoveDuration =
  -- NOTE: See sass/_config.scss for the value of --grid-tile-move-duration.
  -- They MUST match.
  100


type Msg
  = Tick Animation.TimeDelta


update : Msg -> State -> State
update msg (State state) =
  case msg of
    Tick dt ->
      State { state | clock = state.clock + dt }


subscriptions : Sub Msg
subscriptions =
  BE.onAnimationFrameDelta Tick


view : State -> H.Html msg
view (State { animatedTiles, clock }) =
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
        , viewGridTiles clock animatedTiles
        ]
    ]


viewGridTiles : Animation.Clock -> List AnimatedTile -> H.Html msg
viewGridTiles clock =
  HK.node "div" [ HA.class "grid_tiles" ] << List.map (viewGridTile clock)


viewGridTile : Animation.Clock -> AnimatedTile -> (String, H.Html msg)
viewGridTile clock { factor, tile } =
  let
    k =
      String.fromFloat <| Animation.animate clock factor

    { kind, id, value, from, to } =
      Tile.toInfo tile

    (fromRow, fromCol) =
      (from.row, from.col)

    (toRow, toCol) =
      (to.row, to.col)

    (fromLeft, fromTop) =
      ( "(" ++ String.fromInt fromCol ++ " * var(--grid-gap) + " ++ String.fromInt (fromCol - 1) ++ " * var(--tile-size)" ++ ")"
      , "(" ++ String.fromInt fromRow ++ " * var(--grid-gap) + " ++ String.fromInt (fromRow - 1) ++ " * var(--tile-size)" ++ ")"
      )

    (toLeft, toTop) =
      ( "(" ++ String.fromInt toCol ++ " * var(--grid-gap) + " ++ String.fromInt (toCol - 1) ++ " * var(--tile-size)" ++ ")"
      , "(" ++ String.fromInt toRow ++ " * var(--grid-gap) + " ++ String.fromInt (toRow - 1) ++ " * var(--tile-size)" ++ ")"
      )

    (left, top) =
      ( "calc(" ++ fromLeft ++ " + " ++ k ++ " * " ++ "(" ++ toLeft ++ " - " ++ fromLeft ++ "))"
      , "calc(" ++ fromTop ++ " + " ++ k ++ " * " ++ "(" ++ toTop ++ " - " ++ fromTop ++ "))"
      )
  in
  ( String.fromInt id
  , H.div
      [ HA.class "grid__tile"
      , HA.style "left" left
      , HA.style "top" top
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
