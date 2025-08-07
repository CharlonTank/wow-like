module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import SeqDict exposing (SeqDict)
import Math.Vector3 as Vec3 exposing (Vec3)
import Set exposing (Set)
import Url exposing (Url)
-- Note: no Mat4/WebGL types are needed in this module


type alias FrontendModel =
    { key : Key
    , time : Float
    , playerPos : Vec3
    , playerYaw : Float
    , cameraDistance : Float
    , cameraHeight : Float
    , cameraMode : CameraMode
    , verticalVelocity : Float
    , isGrounded : Bool
    , selectedPlayerId : Maybe ClientId
    , moveFactor : Float
    , turnFactor : Float
    , otherTargets : SeqDict ClientId Player
    , cameraEye : Vec3
    , keys : Set String
    , mouseDown : Bool
    , lastMouseX : Float
    , otherPlayers : List Player
    , enemyViewDistance : Float
    , propViewDistance : Float
    , showNameplates : Bool
    , perfMode : Bool
    , stamina : Float
    }


type alias BackendModel =
    { players : List ( ClientId, Player )
    }


type alias Player =
    { id : ClientId
    , pos : Vec3
    , yaw : Float
    }


-- Network-safe position for sending over the wire
type alias Position3 =
    { x : Float, y : Float, z : Float }


-- Snapshot of a player that is safe to send between frontend and backend
type alias PlayerSnapshot =
    { id : ClientId
    , pos : Position3
    , yaw : Float
    }


vec3ToPosition3 : Vec3 -> Position3
vec3ToPosition3 v =
    { x = Vec3.getX v, y = Vec3.getY v, z = Vec3.getZ v }


position3ToVec3 : Position3 -> Vec3
position3ToVec3 p =
    Vec3.vec3 p.x p.y p.z


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Tick Float
    | KeyDown String
    | KeyUp String
    | MouseDown Float Float
    | MouseUp
    | MouseMove Float Float
    | RespawnClicked
    | Jump
    | CycleSelection
    | Clicked Float Float


type ToBackend
    = PlayerUpdate Position3 Float


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = UpdatePlayers (List PlayerSnapshot)


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


type CameraMode
    = FirstPerson
    | ThirdPerson