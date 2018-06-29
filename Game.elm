module Game exposing (main)

import Html exposing (..)

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
    , div [] [ text "The grid will appear here." ]
    ]
