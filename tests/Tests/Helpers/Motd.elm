module Tests.Helpers.Motd exposing (parseConsoleLogSuite)

import Expect
import Helpers.Motd as Motd
import Test exposing (Test, describe, test)
import Time


parseConsoleLogSuite : Test
parseConsoleLogSuite =
    describe "Helpers.Motd.parseConsoleLog"
        [ test "parses a valid structured MOTD line" <|
            \_ ->
                """noise
{"exoMotd":{"epoch":123000,"text":"Welcome to the instance"}}
more noise
"""
                    |> Motd.parseConsoleLog
                    |> Expect.equal
                        (Just
                            { epoch = Time.millisToPosix 123000
                            , text = "Welcome to the instance"
                            }
                        )
        , test "chooses the latest valid MOTD line" <|
            \_ ->
                """{"exoMotd":{"epoch":123000,"text":"old"}}
{"exoMotd":{"epoch":124000,"text":"new"}}
"""
                    |> Motd.parseConsoleLog
                    |> Maybe.map .text
                    |> Expect.equal (Just "new")
        , test "ignores malformed JSON and unrelated console output" <|
            \_ ->
                """[  10.123] {"status":"running","epoch":123000}
not json
{"exoMotd":{"epoch":"wrong","text":"bad"}}
"""
                    |> Motd.parseConsoleLog
                    |> Expect.equal Nothing
        , test "handles empty MOTD text" <|
            \_ ->
                """[  10.123] {"exoMotd":{"epoch":123000,"text":""}}"""
                    |> Motd.parseConsoleLog
                    |> Maybe.map .text
                    |> Expect.equal (Just "")
        ]
