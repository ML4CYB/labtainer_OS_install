module OpenStack.ServerMetadata exposing (requestCreateServerMetadata)

import Helpers.Error exposing (ErrorContext, ErrorLevel(..))
import Http
import Json.Encode as Encode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectStringWithErrorBody, openstackCredentialedRequest, resultToMsgErrorBody)
import Types.Types exposing (HttpRequestMethod(..), Msg(..), Project)


requestCreateServerMetadata : Project -> OSTypes.ServerUuid -> String -> String -> Cmd Msg
requestCreateServerMetadata project serverUuid key value =
    let
        requestBody =
            Encode.object
                [ ( "metadata", Encode.object [ ( key, Encode.string value ) ] )
                ]

        errorContext =
            ErrorContext
                ("create server metadata with key "
                    ++ key
                    ++ " and value "
                    ++ value
                    ++ " for server with UUID"
                    ++ serverUuid
                )
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        project
        Post
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/metadata")
        (Http.jsonBody requestBody)
        (expectStringWithErrorBody
            (resultToMsgErrorBody errorContext (\_ -> NoOp))
        )
