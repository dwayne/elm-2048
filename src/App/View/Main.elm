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
  , message : Grid.Message msg
  , gridState : Grid.State
  , onMove : Grid.Direction -> msg
  , onNewGame : msg
  }


view : Options msg -> H.Html msg
view { header, message, gridState, onMove, onNewGame } =
  H.main_
    [ HA.class "main"
    , HA.tabindex 0
    , HA.autofocus True
    , onKeyDown onMove onNewGame
    ]
    [ H.div [ HA.class "main__body" ]
        [ H.div [ HA.class "main__header" ] [ Header.view header ]
        , H.div [ HA.class "main__introduction" ] [ Introduction.view onNewGame ]
        , H.div [ HA.class "main__grid" ] [ Grid.view message gridState ]
        , H.div [ HA.class "main__footer" ] [ Footer.view ]
        ]
    ]


onKeyDown : (Grid.Direction -> msg) -> msg -> H.Attribute msg
onKeyDown onMove onNewGame =
  let
    keyDecoder =
      JD.field "key" JD.string
        |> JD.andThen
            (\key ->
              case (key, String.toUpper key) of
                -- Arrow keys: Up, Right, Down, Left
                ("ArrowUp", _) ->
                  JD.succeed <| onMove Grid.Up

                ("ArrowRight", _) ->
                  JD.succeed <| onMove Grid.Right

                ("ArrowDown", _) ->
                  JD.succeed <| onMove Grid.Down

                ("ArrowLeft", _) ->
                  JD.succeed <| onMove Grid.Left

                -- Vim: KLJH
                (_, "K") ->
                  JD.succeed <| onMove Grid.Up

                (_, "L") ->
                  JD.succeed <| onMove Grid.Right

                (_, "J") ->
                  JD.succeed <| onMove Grid.Down

                (_, "H") ->
                  JD.succeed <| onMove Grid.Left

                -- WDSA
                (_, "W") ->
                  JD.succeed <| onMove Grid.Up

                (_, "D") ->
                  JD.succeed <| onMove Grid.Right

                (_, "S") ->
                  JD.succeed <| onMove Grid.Down

                (_, "A") ->
                  JD.succeed <| onMove Grid.Left

                -- Restart
                (_, "R") ->
                  JD.succeed onNewGame

                _ ->
                  JD.fail ""
            )
  in
  keyDecoder
    |> JD.map (\msg -> (msg, True))
    |> HE.preventDefaultOn "keydown"
