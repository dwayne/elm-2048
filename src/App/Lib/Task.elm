module App.Lib.Task exposing (dispatch)

import Task


dispatch : msg -> Cmd msg
dispatch =
    Task.perform identity << Task.succeed
