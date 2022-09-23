module App.View.Main exposing (Options, view)


import App.Data.Grid exposing (Grid)
import App.View.Footer as Footer
import App.View.Grid as Grid
import App.View.Header as Header
import App.View.Introduction as Introduction
import Html as H
import Html.Attributes as HA


type alias Options msg =
  { header : Header.Options msg
  , onNewGame : msg
  , currentId : Int
  , grid : Grid
  }


view : Options msg -> H.Html msg
view { header, onNewGame, currentId, grid } =
  H.main_ [ HA.class "main" ]
    [ H.div [ HA.class "main__header" ] [ Header.view header ]
    , H.div [ HA.class "main__introduction" ] [ Introduction.view onNewGame ]
    , H.div [ HA.class "main__grid" ] [ Grid.view currentId grid ]
    , H.div [ HA.class "main__footer" ] [ Footer.view ]
    ]
