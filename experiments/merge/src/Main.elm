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
    , value : Int
    , position : Position
    , action : Action
    }


type Kind
    = New
    | Composite
    | Merged
    | Old


type alias Position =
    ( Int, Int )


type Action
    = Stay
    | MoveTo Position


testCase1 : TestCase
testCase1 =
    { before =
        [ Tile New 2 ( 1, 1 ) Stay
        , Tile New 2 ( 2, 1 ) Stay
        , Tile New 4 ( 2, 3 ) Stay
        , Tile New 2 ( 3, 1 ) Stay
        , Tile New 2 ( 3, 3 ) Stay
        , Tile New 2 ( 4, 4 ) Stay
        ]
    , after =
        [ Tile Old 2 ( 1, 1 ) <| MoveTo ( 1, 4 )
        , Tile Old 2 ( 2, 1 ) <| MoveTo ( 2, 3 )
        , Tile Old 4 ( 2, 3 ) <| MoveTo ( 2, 4 )
        , Tile Merged 2 ( 3, 1 ) <| MoveTo ( 3, 4 )
        , Tile Merged 2 ( 3, 3 ) <| MoveTo ( 3, 4 )
        , Tile Old 2 ( 4, 4 ) Stay
        , Tile Composite 4 ( 3, 4 ) Stay

        -- N.B. The action for New and Composite will always be Stay.
        -- It suggests we can reorganize the Tile type.
        ]
    }


testCase2 : TestCase
testCase2 =
    { before =
        [ Tile New 2 ( 1, 4 ) Stay
        , Tile New 4 ( 2, 2 ) Stay
        , Tile New 2 ( 2, 4 ) Stay
        , Tile New 2 ( 3, 2 ) Stay
        , Tile New 2 ( 3, 4 ) Stay
        , Tile New 2 ( 4, 1 ) Stay
        ]
    , after =
        [ Tile Old 2 ( 1, 4 ) <| MoveTo ( 1, 1 )
        , Tile Old 4 ( 2, 2 ) <| MoveTo ( 2, 1 )
        , Tile Old 2 ( 2, 4 ) <| MoveTo ( 2, 2 )
        , Tile Merged 2 ( 3, 2 ) <| MoveTo ( 3, 1 )
        , Tile Merged 2 ( 3, 4 ) <| MoveTo ( 3, 1 )
        , Tile Old 2 ( 4, 1 ) Stay
        , Tile Composite 4 ( 3, 1 ) Stay
        ]
    }


testCase3 : TestCase
testCase3 =
    { before =
        [ Tile New 2 ( 1, 1 ) Stay
        , Tile New 2 ( 1, 2 ) Stay
        , Tile New 2 ( 1, 3 ) Stay
        , Tile New 4 ( 3, 2 ) Stay
        , Tile New 2 ( 3, 3 ) Stay
        , Tile New 2 ( 4, 4 ) Stay
        ]
    , after =
        [ Tile Old 2 ( 1, 1 ) <| MoveTo ( 4, 1 )
        , Tile Old 2 ( 1, 2 ) <| MoveTo ( 3, 2 )
        , Tile Merged 2 ( 1, 3 ) <| MoveTo ( 4, 3 )
        , Tile Old 4 ( 3, 2 ) <| MoveTo ( 4, 2 )
        , Tile Merged 2 ( 3, 3 ) <| MoveTo ( 4, 3 )
        , Tile Old 2 ( 4, 4 ) Stay
        , Tile Composite 4 ( 4, 3 ) Stay
        ]
    }


testCase4 : TestCase
testCase4 =
    { before =
        [ Tile New 2 ( 1, 4 ) Stay
        , Tile New 4 ( 2, 2 ) Stay
        , Tile New 2 ( 2, 3 ) Stay
        , Tile New 2 ( 4, 1 ) Stay
        , Tile New 2 ( 4, 2 ) Stay
        , Tile New 2 ( 4, 3 ) Stay
        ]
    , after =
        [ Tile Old 2 ( 1, 4 ) Stay
        , Tile Old 4 ( 2, 2 ) <| MoveTo ( 1, 2 )
        , Tile Merged 2 ( 2, 3 ) <| MoveTo ( 1, 3 )
        , Tile Old 2 ( 4, 1 ) <| MoveTo ( 1, 1 )
        , Tile Old 2 ( 4, 2 ) <| MoveTo ( 2, 2 )
        , Tile Merged 2 ( 4, 3 ) <| MoveTo ( 1, 3 )
        , Tile Composite 4 ( 1, 3 ) Stay
        , Tile New 2 ( 4, 1 ) Stay
        , Tile New 4 ( 4, 4 ) Stay
        ]
    }


testCase5 : TestCase
testCase5 =
    { before =
        [ Tile New 2 ( 1, 1 ) Stay
        , Tile New 4 ( 1, 2 ) Stay
        , Tile New 2 ( 1, 3 ) Stay
        , Tile New 4 ( 1, 4 ) Stay
        , Tile New 2 ( 2, 1 ) Stay
        , Tile New 2 ( 2, 2 ) Stay
        , Tile New 2 ( 2, 3 ) Stay
        , Tile New 2 ( 2, 4 ) Stay
        , Tile New 4 ( 3, 1 ) Stay
        , Tile New 2 ( 3, 2 ) Stay
        , Tile New 2 ( 3, 3 ) Stay
        , Tile New 4 ( 3, 4 ) Stay
        , Tile New 2 ( 4, 1 ) Stay
        , Tile New 4 ( 4, 2 ) Stay
        , Tile New 2 ( 4, 3 ) Stay
        , Tile New 2 ( 4, 4 ) Stay
        ]
    , after =
        [ Tile Old 2 ( 1, 1 ) Stay
        , Tile Old 4 ( 1, 2 ) Stay
        , Tile Old 2 ( 1, 3 ) Stay
        , Tile Old 4 ( 1, 4 ) Stay
        , Tile Merged 2 ( 2, 1 ) <| MoveTo ( 2, 3 )
        , Tile Merged 2 ( 2, 2 ) <| MoveTo ( 2, 3 )
        , Tile Merged 2 ( 2, 3 ) <| MoveTo ( 2, 4 )
        , Tile Merged 2 ( 2, 4 ) Stay
        , Tile Old 4 ( 3, 1 ) <| MoveTo ( 3, 2 )
        , Tile Merged 2 ( 3, 2 ) <| MoveTo ( 3, 3 )
        , Tile Merged 2 ( 3, 3 ) Stay
        , Tile Old 4 ( 3, 4 ) Stay
        , Tile Old 2 ( 4, 1 ) <| MoveTo ( 4, 2 )
        , Tile Old 4 ( 4, 2 ) <| MoveTo ( 4, 3 )
        , Tile Merged 2 ( 4, 3 ) <| MoveTo ( 4, 4 )
        , Tile Merged 2 ( 4, 4 ) Stay
        , Tile Composite 4 ( 2, 3 ) Stay
        , Tile Composite 4 ( 2, 4 ) Stay
        , Tile Composite 4 ( 3, 3 ) Stay
        , Tile Composite 4 ( 4, 4 ) Stay
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
        [ H.div [ HA.class "grid" ] <| List.map viewGridTile grid
        , H.p []
            [ H.button [ HE.onClick ClickedLoad1 ] [ H.text "Load Test Case 1 (Move Right)" ]
            , H.text " "
            , H.button [ HE.onClick ClickedLoad2 ] [ H.text "Load Test Case 2 (Move Left)" ]
            , H.text " "
            , H.button [ HE.onClick ClickedLoad3 ] [ H.text "Load Test Case 3 (Move Down)" ]
            , H.text " "
            , H.button [ HE.onClick ClickedLoad4 ] [ H.text "Load Test Case 4 (Move Up + Add 2 New Tiles)" ]
            , H.text " "
            , H.button [ HE.onClick ClickedLoad5 ] [ H.text "Load Test Case 5 (Move Right)" ]
            ]
        , H.p [] [ H.button [ HE.onClick ClickedTest ] [ H.text "Test" ] ]
        ]


viewGridTile : Tile -> H.Html msg
viewGridTile { kind, value, position, action } =
    H.div
        [ HA.class "grid__tile"
        , HA.class <|
            case action of
                Stay ->
                    let
                        ( r, c ) =
                            position
                    in
                    "grid__tile--" ++ String.fromInt r ++ "-" ++ String.fromInt c

                MoveTo ( r, c ) ->
                    "grid__tile--" ++ String.fromInt r ++ "-" ++ String.fromInt c
        ]
        [ viewTile kind value ]


viewTile : Kind -> Int -> H.Html msg
viewTile kind value =
    let
        valueAsString =
            String.fromInt value
    in
    H.div
        [ HA.class "tile"
        , HA.class <| "tile--" ++ valueAsString
        , HA.class <|
            case kind of
                New ->
                    "tile--new"

                Composite ->
                    "tile--composite"

                Merged ->
                    "tile--merged"

                Old ->
                    "tile--old"
        ]
        [ H.div [ HA.class "tile__value" ] [ H.text valueAsString ] ]
