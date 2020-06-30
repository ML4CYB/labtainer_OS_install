module Rest.Cockpit exposing
    ( receiveCockpitLoginStatus
    , requestCockpitIfRequestable
    )

import Base64
import Helpers.Helpers as Helpers
import Http
import OpenStack.Types as OSTypes
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerOrigin(..)
        , ViewState(..)
        )



{- HTTP Requests -}


requestCockpitIfRequestable : Project -> Server -> Bool -> Cmd Msg
requestCockpitIfRequestable project server isElectron =
    {- Try to log into Cockpit IF:
       - server was launched from Exosphere
       - we have a floating IP address and exouser password
       - Cockpit status is not Ready
    -}
    let
        maybeCockpitUrl =
            Helpers.getServerCockpitUrl server

        maybeExouserPassword =
            Helpers.getServerExouserPassword server.osProps.details
    in
    case
        ( server.exoProps.serverOrigin
        , maybeCockpitUrl
        , maybeExouserPassword
        )
    of
        ( ServerFromExo exoOriginProps, Just cockpitUrl, Just password ) ->
            case exoOriginProps.cockpitStatus of
                Ready ->
                    Cmd.none

                _ ->
                    requestCockpitLogin project server.osProps.uuid password cockpitUrl isElectron

        _ ->
            -- Maybe in the future show an error here? Missing floating IP or password?
            Cmd.none


requestCockpitLogin : Project -> OSTypes.ServerUuid -> String -> String -> Bool -> Cmd Msg
requestCockpitLogin project serverUuid password cockpitUrl isElectron =
    let
        authHeaderValue =
            "Basic " ++ Base64.encode ("exouser:" ++ password)

        resultMsg project2 serverUuid2 result =
            ProjectMsg (Helpers.getProjectId project2) (ReceiveCockpitLoginStatus serverUuid2 result)

        requestParams =
            { method = "GET"
            , headers = [ Http.header "Authorization" authHeaderValue ]
            , url = cockpitUrl ++ "/cockpit/login"
            , body = Http.emptyBody
            , expect = Http.expectString (resultMsg project serverUuid)
            , timeout = Just 3000
            , tracker = Nothing
            }
    in
    -- Future todo handle errors with this API call, e.g. a timeout should not generate error to user but other errors should be handled differently
    -- Electron doesn't appear to respect the withCredentials option
    -- https://github.com/electron/electron/issues/9356
    -- CORS works differently between Electron and the browser
    if isElectron then
        Http.request requestParams

    else
        Http.riskyRequest requestParams



{- HTTP Response Handling -}


receiveCockpitLoginStatus : Model -> Project -> OSTypes.ServerUuid -> Result Http.Error String -> ( Model, Cmd Msg )
receiveCockpitLoginStatus model project serverUuid result =
    case Helpers.serverLookup project serverUuid of
        Nothing ->
            -- No server found, may have been deleted, nothing to do
            ( model, Cmd.none )

        Just server ->
            case server.exoProps.serverOrigin of
                ServerNotFromExo ->
                    ( model, Cmd.none )

                ServerFromExo serverFromExoProps ->
                    let
                        cockpitStatus =
                            case result of
                                -- TODO more error checking, e.g. handle case of invalid credentials rather than telling user "still not ready yet"
                                Err _ ->
                                    CheckedNotReady

                                Ok _ ->
                                    Ready

                        oldExoProps =
                            server.exoProps

                        newServerFromExoProps =
                            { serverFromExoProps | cockpitStatus = cockpitStatus }

                        newExoProps =
                            { oldExoProps | serverOrigin = ServerFromExo newServerFromExoProps }

                        newServer =
                            Server server.osProps newExoProps

                        newProject =
                            Helpers.projectUpdateServer project newServer

                        newModel =
                            Helpers.modelUpdateProject model newProject
                    in
                    ( newModel, Cmd.none )
