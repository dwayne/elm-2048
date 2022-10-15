module App.Lib.Html.Attributes exposing (toAttributes)

import Html as H


toAttributes :
    List (H.Attribute msg)
    -> List (Maybe (H.Attribute msg))
    -> List (H.Attribute msg)
toAttributes base extra =
    base ++ List.filterMap identity extra
