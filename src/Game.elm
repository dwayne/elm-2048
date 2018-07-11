module Game exposing (main)

import Html exposing (..)
import Html.Events as Events
import Keyboard
import Random
import Svg exposing (Svg)
import Svg.Attributes

import Game.Config as C
import Game.Grid as Grid exposing (Grid, Tile, Direction(..), Movement)
import Game.List exposing (cartesianMap)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { score : Int
  , grid : Grid
  }

init : (Model, Cmd Msg)
init =
  emptyModel ! [ newGrid ]

emptyModel : Model
emptyModel =
  { score = 0
  , grid = Grid.empty
  }


-- UPDATE


type Msg
  = KeyDown Keyboard.KeyCode
  | NewGame
  | NewGrid Grid
  | NextGrid Grid

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown code ->
      let
        direction : Maybe Direction
        direction =
          case code of
            37 ->
              Just Left

            38 ->
              Just Up

            39 ->
              Just Right

            40 ->
              Just Down

            _ ->
              Nothing

        movement : Maybe Movement
        movement =
          Maybe.map (\dir -> Grid.move dir model.grid) direction
      in
        case movement of
          Nothing ->
            model ! []

          Just { grid, score, moved } ->
            if moved then
              let
                newModel =
                  { model | score = model.score + score, grid = grid }
              in
                if Grid.hasWinningTile grid then
                  Debug.log "You win!" newModel ! []
                else
                  newModel ! [ nextGrid grid ]
            else
              model ! []

    NewGame ->
      emptyModel ! [ newGrid ]

    NewGrid grid ->
      { model | grid = grid } ! []

    NextGrid grid ->
      let
        newModel =
          { model | grid = grid }
      in
        if Grid.hasMoves grid then
          newModel ! []
        else
          Debug.log "Game over!" newModel ! []

newGrid : Cmd Msg
newGrid =
  Random.generate NewGrid Grid.start

nextGrid : Grid -> Cmd Msg
nextGrid grid =
  Random.generate NextGrid (Grid.next grid)


-- VIEW


view : Model -> Html Msg
view { score, grid } =
  div []
    [ viewHeader score
    , viewGrid grid
    ]

viewHeader : Int -> Html Msg
viewHeader score =
  div []
    [ h1 [] [ text "Elm 2048" ]
    , p [] [ text <| "Score: " ++ (toString score) ]
    , p [] [ button [ Events.onClick NewGame ] [ text "New Game" ] ]
    ]

viewGrid : Grid -> Html msg
viewGrid grid =
  let
    size =
      toString C.gridSize
  in
    Svg.svg
      [ Svg.Attributes.width size
      , Svg.Attributes.height size
      , Svg.Attributes.viewBox ("0 0 " ++ size ++ " " ++ size)
      , Svg.Attributes.style ("background: " ++ C.gridColor)
      ]
      [ viewCells
      , viewTiles (Grid.toList grid)
      ]

viewCells : Svg msg
viewCells =
  Svg.g [ Svg.Attributes.fill C.cellColor ]
    <| cartesianMap C.cellCount C.cellCount viewCell

viewCell : Int -> Int -> Svg msg
viewCell row col =
  let
    x =
      C.cellDistance col

    y =
      C.cellDistance row

    size =
      toString C.cellSize
  in
    Svg.rect
      [ Svg.Attributes.x (toString x)
      , Svg.Attributes.y (toString y)
      , Svg.Attributes.width size
      , Svg.Attributes.height size
      ]
      []

viewTiles : List Tile -> Svg msg
viewTiles tiles =
  Svg.g [] (List.map viewTile tiles)

viewTile : Tile -> Svg msg
viewTile { row, col, value } =
  let
    x =
      C.cellDistance col

    y =
      C.cellDistance row

    size =
      toString C.cellSize

    halfCellSize =
      C.cellSize // 2

    textX =
      x + halfCellSize

    textY =
      y + halfCellSize

    info =
      C.tileInfo value
  in
    Svg.g []
      [ Svg.rect
          [ Svg.Attributes.x (toString x)
          , Svg.Attributes.y (toString y)
          , Svg.Attributes.width size
          , Svg.Attributes.height size
          , Svg.Attributes.fill info.color
          ]
          []
      , Svg.text_
          [ Svg.Attributes.x (toString textX)
          , Svg.Attributes.y (toString textY)
          , Svg.Attributes.fontSize info.fontSize
          , Svg.Attributes.fontWeight "bold"
          , Svg.Attributes.textAnchor "middle"
          , Svg.Attributes.dominantBaseline "central"
          , Svg.Attributes.fill info.textColor
          ]
          [ Svg.text (toString value) ]
      ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Keyboard.downs KeyDown
