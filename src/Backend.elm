module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId)
import Math.Vector3 exposing (vec3)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { players = [] }
    , Cmd.none
    )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected _ clientId ->
            let
                -- Create new player with random-ish spawn position
                spawnX = toFloat (modBy 20 (String.length clientId)) - 10
                newPlayer = Player clientId (vec3 spawnX 1.7 6) 0
                updatedPlayers = ( clientId, newPlayer ) :: model.players
                existingPlayers = List.map Tuple.second (List.filter (\(id, _) -> id /= clientId) model.players)
            in
            ( { model | players = updatedPlayers }
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId (UpdatePlayers (List.map playerToSnapshot existingPlayers))
                , broadcastSnapshotsToAll updatedPlayers
                ]
            )

        ClientDisconnected _ clientId ->
            let
                updatedPlayers = List.filter (\(id, _) -> id /= clientId) model.players
            in
            ( { model | players = updatedPlayers }
            , broadcastSnapshotsToAll updatedPlayers
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        PlayerUpdate pos yaw ->
            let
                playerExists = List.any (\(id, _) -> id == clientId) model.players
                
                updatedPlayers =
                    if playerExists then
                        List.map (\(id, player) ->
                            if id == clientId then
                                ( id, { player | pos = position3ToVec3 pos, yaw = yaw } )
                            else
                                ( id, player )
                        ) model.players
                    else
                        -- Add player if not exists (first update from frontend)
                        ( clientId, Player clientId (position3ToVec3 pos) yaw ) :: model.players
            in
            ( { model | players = updatedPlayers }
            , broadcastSnapshotsToOthers clientId updatedPlayers
            )


broadcastSnapshotsToAll : List ( ClientId, Player ) -> Cmd BackendMsg
broadcastSnapshotsToAll players =
    let
        allPlayersList = List.map Tuple.second players
        
        sendToClient (clientId, _) =
            let
                othersOnly = List.filter (\p -> p.id /= clientId) allPlayersList
                    |> List.map playerToSnapshot
            in
            Lamdera.sendToFrontend clientId (UpdatePlayers othersOnly)
    in
    Cmd.batch (List.map sendToClient players)


broadcastSnapshotsToOthers : ClientId -> List ( ClientId, Player ) -> Cmd BackendMsg
broadcastSnapshotsToOthers senderId players =
    let
        allPlayersList = List.map Tuple.second players
        
        sendToClient (clientId, _) =
            if clientId /= senderId then
                let
                    othersOnly = List.filter (\p -> p.id /= clientId) allPlayersList
                        |> List.map playerToSnapshot
                in
                Lamdera.sendToFrontend clientId (UpdatePlayers othersOnly)
            else
                Cmd.none
    in
    Cmd.batch (List.map sendToClient players)


playerToSnapshot : Player -> PlayerSnapshot
playerToSnapshot p =
    { id = p.id
    , pos = vec3ToPosition3 p.pos
    , yaw = p.yaw
    }