module App.Lib.Random exposing (select, selectAtMostN)


-- NOTE: Thanks to https://github.com/elm-community/random-extra/blob/3.2.0/src/Random/List.elm#L15-L80
-- for help with the implementation of select and selectAtMostN.


import Random


select : List a -> Random.Generator (Maybe a, List a)
select list =
  if List.isEmpty list then
    Random.constant (Nothing, list)
  else
    let
      front i =
        List.take i list

      back i =
        List.drop (i + 1) list

      lastIndex =
        List.length list - 1

      indexGen =
        Random.int 0 lastIndex
    in
    indexGen
      |> Random.map (\i -> (get i list, List.append (front i) (back i)))



selectAtMostN : Int -> List a -> Random.Generator (List a, List a)
selectAtMostN n list =
  if n < 1 then
    Random.constant ([], list)
  else
    select list
      |> Random.andThen
          (\(maybeA, rest) ->
              case maybeA of
                Nothing ->
                  Random.constant ([], list)

                Just a ->
                  selectAtMostN (n - 1) rest
                    |> Random.map (Tuple.mapFirst ((::) a))
          )


get : Int -> List a -> Maybe a
get i list =
  list
    |> List.drop i
    |> List.head
