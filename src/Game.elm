module Game exposing (main)

import Html exposing (..)
import Html.Events as Events
import Keyboard
import Random
import Svg exposing (Svg)
import Svg.Attributes

import Game.Config as C exposing (Outcome(..))
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


type Model
  = Playing GameInfo
  | Gameover Outcome GameInfo

type alias GameInfo =
  { score : Int
  , grid : Grid
  }

init : (Model, Cmd Msg)
init =
  Playing emptyInfo ! [ newGrid ]

emptyInfo : GameInfo
emptyInfo =
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
  case model of
    Playing info ->
      updatePlaying msg info

    Gameover outcome info ->
      updateGameover msg outcome info

updatePlaying : Msg -> GameInfo -> (Model, Cmd Msg)
updatePlaying msg info =
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
          Maybe.map (\dir -> Grid.move dir info.grid) direction
      in
        case movement of
          Nothing ->
            Playing info ! []

          Just { grid, score, moved } ->
            if moved then
              let
                newInfo =
                  { info | grid = grid, score = info.score + score }
              in
                if Grid.hasWinningTile grid then
                  Gameover Winner newInfo ! []
                else
                  Playing newInfo ! [ nextGrid grid ]
            else
              Playing info ! []

    NewGame ->
      Playing emptyInfo ! [ newGrid ]

    NewGrid grid ->
      Playing { info | grid = grid } ! []

    NextGrid grid ->
      let
        newInfo =
          { info | grid = grid }
      in
        if Grid.hasMoves grid then
          Playing newInfo ! []
        else
          Gameover Loser newInfo ! []

updateGameover : Msg -> Outcome -> GameInfo -> (Model, Cmd Msg)
updateGameover msg outcome info =
  case msg of
    NewGame ->
      Playing emptyInfo ! [ newGrid ]

    _ ->
      Gameover outcome info ! []

newGrid : Cmd Msg
newGrid =
  Random.generate NewGrid Grid.start

nextGrid : Grid -> Cmd Msg
nextGrid grid =
  Random.generate NextGrid (Grid.next grid)

-- VIEW

view : Model -> Html Msg
view model =
  case model of
    Playing info ->
      viewPlaying info

    Gameover outcome info ->
      viewGameover outcome info

viewPlaying : GameInfo -> Html Msg
viewPlaying { score, grid } =
  div []
    [ viewHeader score
    , viewGrid Nothing grid
    ]

viewGameover : Outcome -> GameInfo -> Html Msg
viewGameover outcome { score, grid } =
  div []
    [ viewHeader score
    , viewGrid (Just outcome) grid
    ]

viewHeader : Int -> Html Msg
viewHeader score =
  div []
    [ h1 [] [ text "Elm 2048" ]
    , p [] [ text <| "Score: " ++ (toString score) ]
    , p [] [ button [ Events.onClick NewGame ] [ text "New Game" ] ]
    ]

viewGrid : Maybe Outcome -> Grid -> Html msg
viewGrid outcome grid =
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
      , outcome
          |> Maybe.map viewOutcome
          |> Maybe.withDefault (Svg.text "")
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

viewOutcome : Outcome -> Svg msg
viewOutcome outcome =
  let
    size =
      toString C.gridSize

    halfGridSize =
      C.gridSize // 2

    textX =
      halfGridSize

    textY =
      halfGridSize

    info =
      C.outcomeInfo outcome
  in
    Svg.g []
      [ Svg.rect
          [ Svg.Attributes.x "0"
          , Svg.Attributes.y "0"
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
          [ Svg.text info.message ]
      ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Playing _ ->
      Keyboard.downs KeyDown

    Gameover _ _ ->
      Sub.none
