module Main exposing (main)


import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }


-- MODEL


type alias Model =
  { testCase : TestCase
  , grid : Grid
  }


type alias TestCase =
  { before : Grid
  , after : Grid
  }


type alias Grid =
  List Tile


type alias Tile =
  { kind : Kind
  , position : Position
  , value : Int
  , action : Action
  }


type Kind
  = New
  | Stale
  | Composite


type alias Position =
  (Int, Int)


type Action
  = Stay
  | MoveTo Position


testCase1 : TestCase
testCase1 =
  { before =
      [ Tile New (1, 1) 2 Stay
      , Tile New (2, 1) 2 Stay
      , Tile New (2, 3) 4 Stay
      , Tile New (3, 1) 2 Stay
      , Tile New (3, 3) 2 Stay
      , Tile New (4, 4) 2 Stay
      ]
  , after =
      [ Tile Stale (1, 1) 2 <| MoveTo (1, 4)
      , Tile Stale (2, 1) 2 <| MoveTo (2, 3)
      , Tile Stale (2, 3) 4 <| MoveTo (2, 4)
      , Tile Stale (3, 1) 2 <| MoveTo (3, 4)
      , Tile Stale (3, 3) 2 <| MoveTo (3, 4)
      , Tile Stale (4, 4) 2 Stay
      , Tile Composite (3, 4) 4 Stay
      -- N.B. The action for Composite will always be Stay.
      -- Similarly, for New. Means we can reorganize the Tile type.
      ]
  }


testCase2 : TestCase
testCase2 =
  { before =
      [ Tile New (1, 4) 2 Stay
      , Tile New (2, 2) 4 Stay
      , Tile New (2, 4) 2 Stay
      , Tile New (3, 2) 2 Stay
      , Tile New (3, 4) 2 Stay
      , Tile New (4, 1) 2 Stay
      ]
  , after =
      [ Tile Stale (1, 4) 2 <| MoveTo (1, 1)
      , Tile Stale (2, 2) 4 <| MoveTo (2, 1)
      , Tile Stale (2, 4) 2 <| MoveTo (2, 2)
      , Tile Stale (3, 2) 2 <| MoveTo (3, 1)
      , Tile Stale (3, 4) 2 <| MoveTo (3, 1)
      , Tile Stale (4, 1) 2 Stay
      , Tile Composite (3, 1) 4 Stay
      ]
  }


testCase3 : TestCase
testCase3 =
  { before =
      [ Tile New (1, 1) 2 Stay
      , Tile New (1, 2) 2 Stay
      , Tile New (1, 3) 2 Stay
      , Tile New (3, 2) 4 Stay
      , Tile New (3, 3) 2 Stay
      , Tile New (4, 4) 2 Stay
      ]
  , after =
      [ Tile Stale (1, 1) 2 <| MoveTo (4, 1)
      , Tile Stale (1, 2) 2 <| MoveTo (3, 2)
      , Tile Stale (1, 3) 2 <| MoveTo (4, 3)
      , Tile Stale (3, 2) 4 <| MoveTo (4, 2)
      , Tile Stale (3, 3) 2 <| MoveTo (4, 3)
      , Tile Stale (4, 4) 2 Stay
      , Tile Composite (4, 3) 4 Stay
      ]
  }


testCase4 : TestCase
testCase4 =
  { before =
      [ Tile New (1, 4) 2 Stay
      , Tile New (2, 2) 4 Stay
      , Tile New (2, 3) 2 Stay
      , Tile New (4, 1) 2 Stay
      , Tile New (4, 2) 2 Stay
      , Tile New (4, 3) 2 Stay
      ]
  , after =
      [ Tile Stale (1, 4) 2 Stay
      , Tile Stale (2, 2) 4 <| MoveTo (1, 2)
      , Tile Stale (2, 3) 2 <| MoveTo (1, 3)
      , Tile Stale (4, 1) 2 <| MoveTo (1, 1)
      , Tile Stale (4, 2) 2 <| MoveTo (2, 2)
      , Tile Stale (4, 3) 2 <| MoveTo (1, 3)
      , Tile Composite (1, 3) 4 Stay
      , Tile New (4, 1) 2 Stay
      , Tile New (4, 4) 4 Stay
      ]
  }


testCase5 : TestCase
testCase5 =
  { before =
      [ Tile New (1, 1) 2 Stay
      , Tile New (1, 2) 4 Stay
      , Tile New (1, 3) 2 Stay
      , Tile New (1, 4) 4 Stay
      , Tile New (2, 1) 2 Stay
      , Tile New (2, 2) 2 Stay
      , Tile New (2, 3) 2 Stay
      , Tile New (2, 4) 2 Stay
      , Tile New (3, 1) 4 Stay
      , Tile New (3, 2) 2 Stay
      , Tile New (3, 3) 2 Stay
      , Tile New (3, 4) 4 Stay
      , Tile New (4, 1) 2 Stay
      , Tile New (4, 2) 4 Stay
      , Tile New (4, 3) 2 Stay
      , Tile New (4, 4) 2 Stay
      ]
  , after =
      [ Tile Stale (1, 1) 2 Stay
      , Tile Stale (1, 2) 4 Stay
      , Tile Stale (1, 3) 2 Stay
      , Tile Stale (1, 4) 4 Stay
      , Tile Stale (2, 1) 2 <| MoveTo (2, 3)
      , Tile Stale (2, 2) 2 <| MoveTo (2, 3)
      , Tile Stale (2, 3) 2 <| MoveTo (2, 4)
      , Tile Stale (2, 4) 2 Stay
      , Tile Stale (3, 1) 4 <| MoveTo (3, 2)
      , Tile Stale (3, 2) 2 <| MoveTo (3, 3)
      , Tile Stale (3, 3) 2 Stay
      , Tile Stale (3, 4) 4 Stay
      , Tile Stale (4, 1) 2 <| MoveTo (4, 2)
      , Tile Stale (4, 2) 4 <| MoveTo (4, 3)
      , Tile Stale (4, 3) 2 <| MoveTo (4, 4)
      , Tile Stale (4, 4) 2 Stay
      , Tile Composite (2, 3) 4 Stay
      , Tile Composite (2, 4) 4 Stay
      , Tile Composite (3, 3) 4 Stay
      , Tile Composite (4, 4) 4 Stay
      ]
  }


init : Model
init =
  { testCase = testCase1
  , grid = testCase1.before
  }


-- UPDATE


type Msg
  = ClickedTest
  | ClickedLoad1
  | ClickedLoad2
  | ClickedLoad3
  | ClickedLoad4
  | ClickedLoad5


update : Msg -> Model -> Model
update msg model =
  case msg of
    ClickedTest ->
      { model | grid = model.testCase.after }

    ClickedLoad1 ->
      { model | testCase = testCase1, grid = testCase1.before }

    ClickedLoad2 ->
      { model | testCase = testCase2, grid = testCase2.before }

    ClickedLoad3 ->
      { model | testCase = testCase3, grid = testCase3.before }

    ClickedLoad4 ->
      { model | testCase = testCase4, grid = testCase4.before }

    ClickedLoad5 ->
      { model | testCase = testCase5, grid = testCase5.before }


-- VIEW


view : Model -> H.Html Msg
view { grid } =
  H.div []
    [ H.div [ HA.class "grid" ] <|
        List.map viewTile grid
    , H.p []
        [ H.button [ HE.onClick ClickedLoad1 ] [ H.text "Load Test Case 1" ]
        , H.button [ HE.onClick ClickedLoad2 ] [ H.text "Load Test Case 2" ]
        , H.button [ HE.onClick ClickedLoad3 ] [ H.text "Load Test Case 3" ]
        , H.button [ HE.onClick ClickedLoad4 ] [ H.text "Load Test Case 4" ]
        , H.button [ HE.onClick ClickedLoad5 ] [ H.text "Load Test Case 5" ]
        ]
    , H.p [] [ H.button [ HE.onClick ClickedTest ] [ H.text "Test" ] ]
    ]


viewTile : Tile -> H.Html msg
viewTile { kind, position, value, action } =
  let
    (r, c) =
      position
  in
  H.div
    [ HA.class "tile"
    , HA.class <| "tile--" ++ String.fromInt value
    , HA.class <|
        case kind of
          New ->
            "tile--new"

          Stale ->
            "tile--stale"

          Composite ->
            "tile--composite"
    , HA.class <|
        case action of
          Stay ->
            "tile-" ++ String.fromInt r ++ "-" ++ String.fromInt c

          MoveTo (mr, mc) ->
            "tile-" ++ String.fromInt mr ++ "-" ++ String.fromInt mc
    ]
    [ H.div [ HA.class "tile__inner" ] [] ]
