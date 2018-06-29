module Game exposing (main)

import Html exposing (..)
import Svg exposing (Svg)
import Svg.Attributes

main : Html msg
main =
  view model

-- MODEL

type alias Model =
  { score : Int
  }

model : Model
model =
  { score = 0
  }

-- VIEW

view : Model -> Html msg
view { score } =
  div []
    [ h1 [] [ text "Elm 2048" ]
    , p [] [ text <| "Score: " ++ (toString score) ]
    , p [] [ button [] [ text "New Game" ] ]
    , viewGrid
    ]

-- GRID

cellCount : Int
cellCount = 4

cellSize : Int
cellSize = 100

cellSpacing : Int
cellSpacing = 15

-- Calculate the number of pixels away the cell in row/column
-- n (0 <= n < cellCount) is from the top/left of the grid.
cellDistance : Int -> Int
cellDistance n =
  cellSpacing + n * (cellSize + cellSpacing)

cellColor : String
cellColor = "rgba(238, 228, 218, 0.35)"

gridSize : Int
gridSize =
  cellCount * (cellSize + cellSpacing) + cellSpacing

gridColor : String
gridColor = "#bbada0"

viewGrid : Html msg
viewGrid =
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
      [ viewCells
      ]

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

-- HELPERS

-- For e.g.
-- cartesianMap 2 3 (,) = [ (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2) ]
cartesianMap : Int -> Int -> (Int -> Int -> a) -> List a
cartesianMap n m f =
  let
    loop i j =
      if i == n then
        []
      else if j == n then
        loop (i+1) 0
      else
        f i j :: loop i (j+1)
  in
    loop 0 0
