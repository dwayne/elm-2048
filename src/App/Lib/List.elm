module App.Lib.List exposing (compareBy, zip)

{- Returns the order of the first non-equal comparison.

   If there are no comparisons or all of them are equal then EQ is returned.
-}


compareBy : List ( comparable, comparable ) -> Order
compareBy comparisons =
    case comparisons of
        [] ->
            EQ

        ( a, b ) :: restComparisons ->
            case compare a b of
                EQ ->
                    compareBy restComparisons

                order ->
                    order


zip : List a -> List b -> List ( a, b )
zip list1 list2 =
    case ( list1, list2 ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: rest1, y :: rest2 ) ->
            ( x, y ) :: zip rest1 rest2
