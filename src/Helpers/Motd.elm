module Helpers.Motd exposing (parseConsoleLog)

import Helpers.Helpers as Helpers
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Time
import Types.Server exposing (MotdSnapshot)


parseConsoleLog : String -> Maybe MotdSnapshot
parseConsoleLog consoleLog =
    consoleLog
        |> String.split "\n"
        |> List.map Helpers.stripTimeSinceBootFromLogLine
        |> List.filterMap
            (\line ->
                Decode.decodeString motdSnapshotDecoder line
                    |> Result.toMaybe
            )
        |> List.reverse
        |> List.head


motdSnapshotDecoder : Decode.Decoder MotdSnapshot
motdSnapshotDecoder =
    Decode.field "exoMotd" <|
        (Decode.succeed MotdSnapshot
            |> Pipeline.required "epoch" (Decode.int |> Decode.map Time.millisToPosix)
            |> Pipeline.required "text" Decode.string
        )
