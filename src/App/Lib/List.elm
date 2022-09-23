module App.Lib.List exposing (zip)


zip : List a -> List b -> List (a, b)
zip list1 list2 =
  case (list1, list2) of
    ([], _) ->
      []

    (_, []) ->
      []

    (x :: rest1, y :: rest2) ->
      (x, y) :: zip rest1 rest2
