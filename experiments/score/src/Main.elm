module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Json.Decode as JD


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { id : Int
    , delta : List ( Int, String )
    }


init : Model
init =
    { id = 0
    , delta = []
    }



-- UPDATE


type Msg
    = ClickedPlus4
    | AnimationEnded


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPlus4 ->
            let
                newId =
                    model.id + 1
            in
            { model | id = newId, delta = model.delta ++ [ ( newId, "+4" ) ] }

        AnimationEnded ->
            { model | delta = Maybe.withDefault [] <| List.tail model.delta }



-- VIEW


view : Model -> H.Html Msg
view { delta } =
    H.div []
        [ H.h1 [] [ H.text "Scoring" ]
        , H.h2 [] [ H.text "Score" ]
        , H.div [ HA.class "score" ]
            [ H.div [ HA.class "score__title" ] [ H.text "Score" ]
            , HK.node "div" [ HA.class "score__total" ] <|
                [ ( "score__value"
                  , H.span [ HA.class "score__value" ] [ H.text "10000000" ]
                  )
                ]
                    ++ List.map viewScoreDelta delta
            ]
        , H.p [] [ H.button [ HE.onClick ClickedPlus4 ] [ H.text "+4" ] ]
        ]


viewScoreDelta : ( Int, String ) -> ( String, H.Html Msg )
viewScoreDelta ( id, text ) =
    ( String.fromInt id
    , H.span
        [ HA.class "score__delta"
        , onAnimationEnd AnimationEnded
        ]
        [ H.text text ]
    )


onAnimationEnd : msg -> H.Attribute msg
onAnimationEnd msg =
    HE.on "animationend" (JD.succeed msg)
