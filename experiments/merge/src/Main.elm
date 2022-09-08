module Main exposing (main)


import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }


-- MODEL


type alias Model =
  { tiles : List Tile
  }


type alias Tile =
  { position : (Int, Int)
  , value : Int
  , kind : Kind
  }


type Kind
  = New
  | Stale
  | Used
  | Merged


init : Model
init =
  Model
    [ Tile (1, 1) 2 New
    , Tile (1, 3) 2 New
    ]


-- UPDATE


type Msg
  = ClickedMerge


update : Msg -> Model -> Model
update msg model =
  case msg of
    ClickedMerge ->
      { model
      | tiles =
          -- The problem is that order matters.
          -- This will not work as intended.
          -- [ Tile (1, 4) 2 Used
          -- , Tile (1, 4) 4 Merged
          -- , Tile (1, 4) 2 Used
          -- ]
          -- But, this works!
          [ Tile (1, 4) 2 Used
          , Tile (1, 4) 2 Used
          , Tile (1, 4) 4 Merged
          ]
          -- Q: How to fix?
      }


-- VIEW


view : Model -> H.Html Msg
view { tiles } =
  H.div []
    [ H.div [ HA.class "row" ] <|
        List.map viewTile tiles
    , H.p [] [ H.button [ HE.onClick ClickedMerge ] [ H.text "Merge" ] ]
    ]


viewTile : Tile -> H.Html msg
viewTile { position, value, kind } =
  let
    (r, c) =
      position
  in
  H.div
    [ HA.class "tile"
    , HA.class <| "tile-" ++ String.fromInt r ++ "-" ++ String.fromInt c
    , HA.class <| "tile--" ++ String.fromInt value
    , HA.class <|
        case kind of
          New ->
            "tile--new"

          Stale ->
            "tile--stale"

          Used ->
            "tile--used"

          Merged ->
            "tile--merged"
    ]
    [ H.div [ HA.class "tile__inner" ] [] ]
