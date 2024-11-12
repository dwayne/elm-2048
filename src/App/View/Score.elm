module App.View.Score exposing
    ( Msg
    , State
    , ViewCurrentOptions
    , addPoints
    , init
    , update
    , viewBest
    , viewCurrent
    )

import App.Data.Points as Points exposing (Points)
import App.Lib.Html.Events as HE
import Html as H
import Html.Attributes as HA
import Html.Keyed as HK



-- STATE


type State
    = State
        { currentId : Int
        , deltas : List ( Int, Points )
        }


init : State
init =
    State
        { currentId = 0
        , deltas = []
        }


addPoints : Points -> State -> State
addPoints points (State state) =
    State
        { state
            | currentId = state.currentId + 1
            , deltas = state.deltas ++ [ ( state.currentId, points ) ]
        }



-- UPDATE


type Msg
    = AnimationEnded


update : Msg -> State -> State
update msg (State state) =
    case msg of
        AnimationEnded ->
            State
                { state
                    | deltas = List.tail state.deltas |> Maybe.withDefault []
                }



-- VIEW SHORTCUTS


type alias ViewCurrentOptions msg =
    { points : Points
    , state : State
    , onChange : Msg -> msg
    }


viewCurrent : ViewCurrentOptions msg -> H.Html msg
viewCurrent { points, state, onChange } =
    view
        { title = "Score"
        , points = points
        , maybeDynamic =
            Just
                { state = state
                , onChange = onChange
                }
        }


viewBest : Points -> H.Html msg
viewBest points =
    view
        { title = "Best"
        , points = points
        , maybeDynamic = Nothing
        }



-- VIEW


type alias ViewOptions msg =
    { title : String
    , points : Points
    , maybeDynamic : Maybe (Dynamic msg)
    }


type alias Dynamic msg =
    { state : State
    , onChange : Msg -> msg
    }


view : ViewOptions msg -> H.Html msg
view { title, points, maybeDynamic } =
    let
        ( pointsAsString, scoreNDigit ) =
            pointsToDetails points
    in
    H.div [ HA.class "score", scoreNDigit ]
        [ H.h2 [ HA.class "score__title" ] [ H.text title ]
        , let
            viewScoreValue =
                H.div [ HA.class "score__value" ] [ H.text pointsAsString ]
          in
          case maybeDynamic of
            Nothing ->
                H.div [ HA.class "score__total" ] [ viewScoreValue ]

            Just { state, onChange } ->
                let
                    viewScoreDeltas =
                        case state of
                            State { deltas } ->
                                List.map (viewScoreDelta <| onChange AnimationEnded) deltas
                in
                HK.node "div" [ HA.class "score__total" ] <|
                    ( "score__value", viewScoreValue )
                        :: viewScoreDeltas
        ]


viewScoreDelta : msg -> ( Int, Points ) -> ( String, H.Html msg )
viewScoreDelta msg ( id, points ) =
    ( String.fromInt id
    , H.div
        [ HA.class "score__delta"
        , HE.onAnimationEnd msg
        ]
        [ H.text <| "+" ++ Points.toString points ]
    )


pointsToDetails : Points -> ( String, H.Attribute msg )
pointsToDetails points =
    let
        pointsAsString =
            Points.toString points

        digits =
            String.length pointsAsString
    in
    ( pointsAsString
    , HA.classList
        [ ( "score--" ++ String.fromInt digits ++ "-digit"
          , digits > 3
          )
        ]
    )
