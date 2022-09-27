module App.View.Main exposing (Options, view)


import App.Data.Grid as Grid
import App.View.Footer as Footer
import App.View.Grid as Grid
import App.View.Header as Header
import App.View.Introduction as Introduction
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD


type alias Options msg =
  { header : Header.Options msg
  , gridState : Grid.State
  , onMove : Grid.Direction -> msg
  , onNewGame : msg
  }


view : Options msg -> H.Html msg
view { header, gridState, onMove, onNewGame } =
  H.main_
    [ HA.class "main"
    , HA.tabindex -1
    , HA.autofocus True
    , onKeyDown onMove
    ]
    [ H.div [ HA.class "main__body" ]
        [ H.div [ HA.class "main__header" ] [ Header.view header ]
        , H.div [ HA.class "main__introduction" ] [ Introduction.view onNewGame ]
        , H.div [ HA.class "main__grid" ] [ Grid.view gridState ]
        , H.div [ HA.class "main__footer" ] [ Footer.view ]
        ]
    ]


onKeyDown : (Grid.Direction -> msg) -> H.Attribute msg
onKeyDown toMsg =
  directionDecoder
    |> JD.map (\d -> (toMsg d, True))
    |> HE.preventDefaultOn "keydown"


directionDecoder : JD.Decoder Grid.Direction
directionDecoder =
  JD.field "key" JD.string
    |> JD.andThen
        (\key ->
          case key of
            "ArrowRight" ->
              JD.succeed Grid.Right

            "ArrowLeft" ->
              JD.succeed Grid.Left

            "ArrowDown" ->
              JD.succeed Grid.Down

            "ArrowUp" ->
              JD.succeed Grid.Up

            _ ->
              JD.fail ""
        )
