module App.View.Main exposing
    ( Msg
    , Options
    , State
    , UpdateOptions
    , init
    , update
    , view
    )

import App.Data.Grid as Grid
import App.Lib.Task as Task
import App.View.Footer as Footer
import App.View.Grid as Grid
import App.View.Header as Header
import App.View.Introduction as Introduction
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD



-- STATE


type State
    = State
        { maybeStartPoint : Maybe Point
        }


type alias Point =
    { x : Float
    , y : Float
    }


init : State
init =
    State
        { maybeStartPoint = Nothing
        }



-- UPDATE


type alias UpdateOptions msg =
    { onMove : Grid.Direction -> msg
    }


type Msg
    = TouchStarted Point
    | TouchEnded Point


update : UpdateOptions msg -> Msg -> State -> ( State, Cmd msg )
update options msg (State state) =
    case msg of
        TouchStarted startPoint ->
            ( State
                { maybeStartPoint = Just startPoint
                }
            , Cmd.none
            )

        TouchEnded endPoint ->
            case state.maybeStartPoint of
                Just startPoint ->
                    ( State
                        { maybeStartPoint = Nothing
                        }
                    , case directionOfMotion startPoint endPoint of
                        Just direction ->
                            Task.dispatch <| options.onMove direction

                        Nothing ->
                            Cmd.none
                    )

                Nothing ->
                    ( State state
                    , Cmd.none
                    )


directionOfMotion : Point -> Point -> Maybe Grid.Direction
directionOfMotion startPoint endPoint =
    let
        dx =
            endPoint.x - startPoint.x

        absDx =
            abs dx

        dy =
            endPoint.y - startPoint.y

        absDy =
            abs dy
    in
    if max absDx absDy > 10 then
        Just <|
            if absDx > absDy then
                if dx > 0 then
                    Grid.Right

                else
                    Grid.Left

            else if dy > 0 then
                Grid.Down

            else
                Grid.Up

    else
        Nothing



-- VIEW


type alias Options msg =
    { id : String
    , header : Header.Options msg
    , message : Grid.Message msg
    , gridState : Grid.State
    , onMove : Grid.Direction -> msg
    , onNewGame : msg
    , onChange : Msg -> msg
    }


view : Options msg -> H.Html msg
view { id, header, message, gridState, onMove, onNewGame, onChange } =
    H.main_
        [ HA.id id
        , HA.class "main"
        , HA.tabindex 0
        , HA.autofocus True
        , onKeyDown onMove onNewGame
        ]
        [ H.div [ HA.class "main__body" ]
            [ H.div [ HA.class "main__header" ] [ Header.view header ]
            , H.div [ HA.class "main__introduction" ] [ Introduction.view onNewGame ]
            , H.div
                (let
                    isPlaying =
                        message == Grid.NoMessage
                 in
                 (++) [ HA.class "main__grid" ] <|
                    if isPlaying then
                        [ HA.tabindex 0
                        , onTouchStart TouchStarted
                            |> HA.map onChange
                        , onTouchEnd TouchEnded
                            |> HA.map onChange
                        ]

                    else
                        []
                )
                [ Grid.view message gridState ]
            , H.div [ HA.class "main__footer" ] [ Footer.view ]
            ]
        ]


onKeyDown : (Grid.Direction -> msg) -> msg -> H.Attribute msg
onKeyDown onMove onNewGame =
    let
        keyDecoder =
            JD.field "key" JD.string
                |> JD.andThen
                    (\key ->
                        case ( key, String.toUpper key ) of
                            -- Arrow keys: Up, Right, Down, Left
                            ( "ArrowUp", _ ) ->
                                JD.succeed <| onMove Grid.Up

                            ( "ArrowRight", _ ) ->
                                JD.succeed <| onMove Grid.Right

                            ( "ArrowDown", _ ) ->
                                JD.succeed <| onMove Grid.Down

                            ( "ArrowLeft", _ ) ->
                                JD.succeed <| onMove Grid.Left

                            -- Vim: KLJH
                            ( _, "K" ) ->
                                JD.succeed <| onMove Grid.Up

                            ( _, "L" ) ->
                                JD.succeed <| onMove Grid.Right

                            ( _, "J" ) ->
                                JD.succeed <| onMove Grid.Down

                            ( _, "H" ) ->
                                JD.succeed <| onMove Grid.Left

                            -- WDSA
                            ( _, "W" ) ->
                                JD.succeed <| onMove Grid.Up

                            ( _, "D" ) ->
                                JD.succeed <| onMove Grid.Right

                            ( _, "S" ) ->
                                JD.succeed <| onMove Grid.Down

                            ( _, "A" ) ->
                                JD.succeed <| onMove Grid.Left

                            -- Restart
                            ( _, "R" ) ->
                                JD.succeed onNewGame

                            _ ->
                                JD.fail ""
                    )
    in
    keyDecoder
        |> JD.map (\msg -> ( msg, True ))
        |> HE.preventDefaultOn "keydown"


onTouchStart : (Point -> Msg) -> H.Attribute Msg
onTouchStart toMsg =
    let
        touchEventDecoder : JD.Decoder Point
        touchEventDecoder =
            targetTouchesLengthDecoder
                |> JD.andThen
                    (\n ->
                        if n == 1 then
                            JD.at [ "targetTouches", "0" ] pointDecoder

                        else
                            JD.fail ""
                    )
    in
    touchEventDecoder
        |> JD.map (\p -> ( toMsg p, True ))
        |> HE.preventDefaultOn "touchstart"


onTouchEnd : (Point -> Msg) -> H.Attribute Msg
onTouchEnd toMsg =
    let
        touchEventDecoder : JD.Decoder Point
        touchEventDecoder =
            targetTouchesLengthDecoder
                |> JD.andThen
                    (\n ->
                        if n == 0 then
                            JD.at [ "changedTouches", "0" ] pointDecoder

                        else
                            JD.fail ""
                    )
    in
    touchEventDecoder
        |> JD.map (\p -> ( toMsg p, True ))
        |> HE.preventDefaultOn "touchend"


targetTouchesLengthDecoder : JD.Decoder Int
targetTouchesLengthDecoder =
    JD.at [ "targetTouches", "length" ] JD.int


pointDecoder : JD.Decoder Point
pointDecoder =
    JD.map2 Point
        (JD.field "clientX" JD.float)
        (JD.field "clientY" JD.float)
