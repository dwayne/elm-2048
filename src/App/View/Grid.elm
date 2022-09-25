module App.View.Grid exposing
  ( init
  , ViewState, view
  , Msg, update
  , subscriptions
  )


import Animation exposing (Animation)
import App.Data.Grid as Grid exposing (Grid)
import App.Data.Tile as Tile exposing (Tile)
import App.Data.Tile.Value as Value exposing (Value)
import Browser.Events as BE
import Html as H
import Html.Attributes as HA
import Html.Keyed as HK


type ViewState =
  ViewState State


type alias State =
  { animatedTiles : List AnimatedTile
  , clock : Animation.Clock
  }


type alias AnimatedTile =
  { tile : Tile
  , factor : Animation
  }


toAnimatedTile : Tile -> AnimatedTile
toAnimatedTile tile =
  { tile = tile
  , factor =
      Animation.animation 0
        |> Animation.duration 100
        -- |> Animation.duration 5000
  }


init : List Tile -> ViewState
init tiles =
  ViewState
    { animatedTiles =
        List.map toAnimatedTile tiles
    , clock = 0
    }


type Msg
  = Tick Animation.TimeDelta


update : Msg -> ViewState -> ViewState
update msg (ViewState state) =
  case msg of
    Tick dt ->
      ViewState { state | clock = state.clock + dt }


subscriptions : Sub Msg
subscriptions =
  BE.onAnimationFrameDelta Tick


view : ViewState -> H.Html msg
view (ViewState { animatedTiles, clock }) =
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
viewGridTile clock { tile, factor } =
  let
    { kind, state, action } =
      Tile.toInfo tile

    (from, to) =
      case action of
        Tile.Stay ->
          (state.position, state.position)

        Tile.MoveFrom fromPosition ->
          (fromPosition, state.position)

    k =
      String.fromFloat <| Animation.animate clock factor

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
  ( String.fromInt state.id
  , H.div
      [ HA.class "grid__tile"
      -- , HA.class <| "grid__tile--" ++ String.fromInt r ++ "-" ++ String.fromInt c
      , HA.style "left" left
      , HA.style "top" top
      ]
      [ viewTile kind state.value ]
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
