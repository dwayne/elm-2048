port module App.Port exposing (save)

import App.Data.Game as Game exposing (Game)
import Json.Encode as JE


save : Game -> Cmd msg
save =
    Game.encode >> sendMessage "save"


sendMessage : String -> JE.Value -> Cmd msg
sendMessage tag payload =
    send <|
        JE.object
            [ ( "tag", JE.string tag )
            , ( "payload", payload )
            ]


port send : JE.Value -> Cmd msg
