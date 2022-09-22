module App.View.Grid exposing (view)


import Html as H
import Html.Attributes as HA


view : H.Html msg
view =
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
        ]
    ]
