module Game exposing (main)

import Html exposing (..)
import Html.Events as Events
import Svg exposing (Svg)
import Svg.Attributes


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }


-- MODEL


type alias Model =
  { score : Int
  , grid : Grid
  }

type alias Grid =
  List Tile

type alias Tile =
  { row : Int
  , col : Int
  , value : Int
  }

init : (Model, Cmd Msg)
init =
  emptyModel ! []

emptyModel : Model
emptyModel =
  { score = 0
  , grid = []
  }


-- UPDATE


type Msg
  = NewGame

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewGame ->
      emptyModel ! []


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
viewGrid _ =
  let
    size =
      toString gridSize
  in
    Svg.svg
      [ Svg.Attributes.width size
      , Svg.Attributes.height size
      , Svg.Attributes.viewBox ("0 0 " ++ size ++ " " ++ size)
      , Svg.Attributes.style ("background: " ++ gridColor)
      ]
      [ viewCells ]

viewCells : Svg msg
viewCells =
  Svg.g [ Svg.Attributes.fill cellColor ]
    <| cartesianMap cellCount cellCount viewCell

viewCell : Int -> Int -> Svg msg
viewCell row col =
  let
    x =
      cellDistance col

    y =
      cellDistance row

    size =
      toString cellSize
  in
    Svg.rect
      [ Svg.Attributes.x (toString x)
      , Svg.Attributes.y (toString y)
      , Svg.Attributes.width size
      , Svg.Attributes.height size
      ]
      []


-- CONFIG


-- The number of cells per row (or equivalently, per column).
cellCount : Int
cellCount = 4

-- The length (in px) of one side of a cell.
cellSize : Int
cellSize = 100

-- The length (in px) of the spacing between adjacent cells.
cellSpacing : Int
cellSpacing = 15

-- Calculate the number of pixels away the cell in
-- row/column n (0 <= n < cellCount) is from the
-- top/left of the grid.
cellDistance : Int -> Int
cellDistance n =
  cellSpacing + n * (cellSize + cellSpacing)

cellColor : String
cellColor = "rgba(238, 228, 218, 0.35)"

-- The length (in px) of one side of the grid.
gridSize : Int
gridSize =
  cellCount * (cellSize + cellSpacing) + cellSpacing

gridColor : String
gridColor = "#bbada0"


-- HELPERS


-- Given n >= 0, m >= 0 and a function, f, it returns a list of the form
-- [ f 0 0, f 0 1, ..., f i j, ..., f (n - 1) (m - 1) ] such that 0 <= i < n
-- and 0 <= j < m.
--
-- For e.g.
--
-- cartesianMap 2 3 (,)
-- => [ (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2) ]
--
-- cartesianMap 2 3 (+)
-- => [ 0, 1, 2, 1, 2, 3 ]
cartesianMap : Int -> Int -> (Int -> Int -> a) -> List a
cartesianMap n m f =
  let
    iter i j =
      if i == n then
        []
      else if j == m then
        iter (i+1) 0
      else
        f i j :: iter i (j+1)
  in
    iter 0 0
