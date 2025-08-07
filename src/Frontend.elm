module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events as E
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Lamdera
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set
import Types exposing (..)
import Lamdera exposing (ClientId)
import Url
import WebGL


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ key =
    let
        initialPos = vec3 0 1.7 6
        initialYaw = 0
    in
    ( { key = key
      , time = 0
      , playerPos = initialPos
      , playerYaw = initialYaw
      , cameraDistance = 10
      , cameraHeight = 5
      , cameraMode = ThirdPerson
      , verticalVelocity = 0
      , isGrounded = True
      , selectedPlayerId = Nothing
      , moveFactor = 0
      , turnFactor = 0
      , keys = Set.empty
      , mouseDown = False
      , lastMouseX = 0
      , otherPlayers = []
      }
    , Lamdera.sendToBackend (PlayerUpdate (vec3ToPosition3 initialPos) initialYaw)
    )


subscriptions : Model -> Sub FrontendMsg
subscriptions _ =
    Sub.batch
        [ E.onAnimationFrameDelta Tick
        , E.onKeyDown (Decode.map KeyDown keyDecoder)
        , E.onKeyUp (Decode.map KeyUp keyDecoder)
        , E.onMouseMove
            (Decode.map2 MouseMove
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
            )
        , E.onMouseUp (Decode.succeed MouseUp)
        ]


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        UrlChanged _ ->
            ( model, Cmd.none )

        Tick dt ->
            let
                dtSeconds = dt / 1000
                baseSpeed = dt * 0.004
                sprintMultiplier = if Set.member "shift" model.keys then 2 else 1
                targetMove = (if (Set.member "w" model.keys || Set.member "s" model.keys || Set.member "a" model.keys || Set.member "d" model.keys) then 1 else 0) |> toFloat
                targetTurn = (if (Set.member "arrowright" model.keys || Set.member "arrowleft" model.keys) then 1 else 0) |> toFloat
                accel = 0.004 * dt
                deaccel = 0.006 * dt
                newMoveFactor =
                    if targetMove > 0 then
                        min 1 (model.moveFactor + accel)
                    else
                        max 0 (model.moveFactor - deaccel)
                newTurnFactor =
                    if targetTurn > 0 then
                        min 1 (model.turnFactor + accel)
                    else
                        max 0 (model.turnFactor - deaccel)
                speed = baseSpeed * toFloat sprintMultiplier * newMoveFactor
                turnSpeed = dt * 0.0025 * newTurnFactor

                forward =
                    (if Set.member "w" model.keys then 1 else 0)
                        - (if Set.member "s" model.keys then 1 else 0)

                strafe =
                    (if Set.member "d" model.keys then 1 else 0)
                        - (if Set.member "a" model.keys then 1 else 0)

                turn =
                    (if Set.member "ArrowRight" model.keys then 1 else 0)
                        - (if Set.member "ArrowLeft" model.keys then 1 else 0)

                yaw2 = model.playerYaw + (toFloat turn) * turnSpeed

                dir = vec3 (sin yaw2) 0 (cos yaw2)
                rightV = vec3 (sin (yaw2 - (pi/2))) 0 (cos (yaw2 - (pi/2)))

                -- Gravity and jumping (physics in seconds)
                gravity = -9.8 -- m/s^2
                jumpVelocity = 6.0 -- m/s, soft floaty jump
                vy0 = model.verticalVelocity + gravity * dtSeconds
                nextVy = if model.isGrounded && Set.member " " model.keys then jumpVelocity else vy0
                y0 = Vec3.getY model.playerPos
                yAfter = y0 + nextVy * dtSeconds
                nextY = max 0 yAfter
                groundedNext = nextY <= 0.0001

                unclampedPos =
                    model.playerPos
                        |> Vec3.add (Vec3.scale (toFloat forward * speed) dir)
                        |> Vec3.add (Vec3.scale (toFloat strafe * speed) rightV)
                        |> (\p -> vec3 (Vec3.getX p) nextY (Vec3.getZ p))

                -- World bounds clamp (match ground size 500 => half 250)
                clampCoord v =
                    clamp -245 245 v

                pos2 =
                    vec3 (clampCoord (Vec3.getX unclampedPos))
                         (Vec3.getY unclampedPos)
                         (clampCoord (Vec3.getZ unclampedPos))

                newModel =
                    { model
                        | time = model.time + dt
                        , playerPos = pos2
                        , playerYaw = yaw2
                        , verticalVelocity = if groundedNext then 0 else nextVy
                        , isGrounded = groundedNext
                        , moveFactor = newMoveFactor
                        , turnFactor = newTurnFactor
                    }
                
                -- Send update every 100ms if moving
                shouldSendUpdate = 
                    (forward /= 0 || strafe /= 0 || turn /= 0) 
                    && (toFloat (floor (model.time / 100)) /= toFloat (floor (newModel.time / 100)))
            in
            ( newModel
            , if shouldSendUpdate then
                Lamdera.sendToBackend (PlayerUpdate (vec3ToPosition3 pos2) yaw2)
              else
                Cmd.none
            )

        KeyDown key ->
            let
                lower = String.toLower key
                baseModel = { model | keys = Set.insert lower model.keys }
            in
            case lower of
                "tab" ->
                    let
                        sorted = List.sortBy .id model.otherPlayers
                        nextId =
                            case model.selectedPlayerId of
                                Nothing ->
                                    sorted |> List.head |> Maybe.map .id

                                Just current ->
                                    let
                                        idx =
                                            sorted
                                                |> List.indexedMap Tuple.pair
                                                |> List.filter (\(_, p) -> p.id == current)
                                                |> List.head
                                                |> Maybe.map Tuple.first

                                        afterCurrent =
                                            case idx of
                                                Just i -> List.drop (i + 1) sorted
                                                Nothing -> []
                                    in
                                    let
                                        candidate = afterCurrent |> List.head |> Maybe.map .id
                                    in
                                    case candidate of
                                        Just cid -> Just cid
                                        Nothing -> sorted |> List.head |> Maybe.map .id
                    in
                    ( { baseModel | selectedPlayerId = nextId }, Cmd.none )
                "c" ->
                    let
                        newMode =
                            case model.cameraMode of
                                FirstPerson ->
                                    ThirdPerson

                                ThirdPerson ->
                                    FirstPerson
                    in
                    ( { baseModel | cameraMode = newMode }, Cmd.none )

                "q" ->
                    ( { baseModel | cameraDistance = clamp 2 30 (model.cameraDistance - 1) }, Cmd.none )

                "e" ->
                    ( { baseModel | cameraDistance = clamp 2 30 (model.cameraDistance + 1) }, Cmd.none )

                _ ->
                    ( baseModel, Cmd.none )

        KeyUp key ->
            ( { model | keys = Set.remove (String.toLower key) model.keys }, Cmd.none )

        MouseDown x _ ->
            ( { model | mouseDown = True, lastMouseX = x }, Cmd.none )

        MouseUp ->
            ( { model | mouseDown = False }, Cmd.none )

        MouseMove x _ ->
            if model.mouseDown then
                let
                    deltaX = x - model.lastMouseX
                    newYaw = model.playerYaw - (deltaX * 0.01)
                in
                ( { model | playerYaw = newYaw, lastMouseX = x }
                , Lamdera.sendToBackend (PlayerUpdate (vec3ToPosition3 model.playerPos) newYaw)
                )
            else
                ( model, Cmd.none )

        RespawnClicked ->
            let
                spawnPos = vec3 0 1.7 6
            in
            ( { model | playerPos = spawnPos, verticalVelocity = 0, isGrounded = True }
            , Lamdera.sendToBackend (PlayerUpdate (vec3ToPosition3 spawnPos) model.playerYaw)
            )

        Jump ->
            -- handled via holding Space in Tick; keep a no-op branch for completeness
            ( model, Cmd.none )

        CycleSelection ->
            -- click cycles selection similar to Tab
            let
                sorted = List.sortBy .id model.otherPlayers
                nextId =
                    case model.selectedPlayerId of
                        Nothing ->
                            sorted |> List.head |> Maybe.map .id

                        Just current ->
                            let
                                idx =
                                    sorted
                                        |> List.indexedMap Tuple.pair
                                        |> List.filter (\(_, p) -> p.id == current)
                                        |> List.head
                                        |> Maybe.map Tuple.first

                                afterCurrent =
                                    case idx of
                                        Just i -> List.drop (i + 1) sorted
                                        Nothing -> []
                            in
                            case afterCurrent |> List.head |> Maybe.map .id of
                                Just cid -> Just cid
                                Nothing -> sorted |> List.head |> Maybe.map .id
            in
            ( { model | selectedPlayerId = nextId }, Cmd.none )

        Clicked _ _ ->
            let
                -- Simplified pick: choose nearest player by world XZ distance
                distanceOnScreen p =
                    let
                        dx = (Vec3.getX p.pos) - (Vec3.getX model.playerPos)
                        dz = (Vec3.getZ p.pos) - (Vec3.getZ model.playerPos)
                        approx = sqrt (dx * dx + dz * dz)
                    in
                    approx

                nearest =
                    model.otherPlayers
                        |> List.sortBy distanceOnScreen
                        |> List.head
                        |> Maybe.map .id
            in
            ( { model | selectedPlayerId = nearest }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UpdatePlayers players ->
            ( { model | otherPlayers = List.map snapshotToPlayer players }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "WoW-like Game"
    , body =
        [ Html.div
            [ Attr.style "width" "100vw"
            , Attr.style "height" "100vh"
            , Attr.style "overflow" "hidden"
            , Attr.style "margin" "0"
            , Attr.style "padding" "0"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "align-items" "center"
            , Attr.style "background-color" "#1a1a1a"
            ]
            [ viewWebGL model
            , viewControls
            , viewDebugInfo model
            ]
        ]
    }


viewWebGL : Model -> Html FrontendMsg
viewWebGL model =
    let
        width = 1280
        height = 720
        aspect = toFloat width / toFloat height

        perspective =
            Mat4.makePerspective 60 aspect 0.01 500

        (camDistance, camHeight) =
            case model.cameraMode of
                ThirdPerson ->
                    ( model.cameraDistance, model.cameraHeight )

                FirstPerson ->
                    ( 0.1, 1.7 )

        cameraOffset =
            vec3
                (sin (model.playerYaw + pi) * camDistance)
                camHeight
                (cos (model.playerYaw + pi) * camDistance)

        eye = Vec3.add model.playerPos cameraOffset
        target = Vec3.add model.playerPos (vec3 0 1.5 0)
        up = vec3 0 1 0

        camera = Mat4.makeLookAt eye target up

        uniforms =
            { perspective = perspective
            , camera = camera
            , model = Mat4.identity
            , time = model.time / 1000
            }

        ground =
            WebGL.entity vertexShader fragmentShader (groundMesh 500)
                { uniforms
                    | model =
                        Mat4.mul
                            (Mat4.makeTranslate (vec3 0 0 0))
                            (Mat4.makeRotate 0 (vec3 0 1 0))
                }

        player =
            WebGL.entity vertexShader fragmentShader playerMesh
                { uniforms
                    | model =
                        Mat4.mul
                            (Mat4.makeTranslate model.playerPos)
                            (Mat4.makeRotate model.playerYaw (vec3 0 1 0))
                }

        otherPlayerEntities =
            model.otherPlayers
                |> List.concatMap (\p ->
                    let
                        base = viewOtherPlayer uniforms model.selectedPlayerId p
                        ring = viewSelectionRing uniforms model.selectedPlayerId p
                    in
                    base :: ring
                )

        allEntities =
            case model.cameraMode of
                ThirdPerson ->
                    ground :: player :: otherPlayerEntities

                FirstPerson ->
                    ground :: otherPlayerEntities
    in
    WebGL.toHtml
        [ Attr.width width
        , Attr.height height
        , Attr.style "display" "block"
        , Attr.style "border-radius" "12px"
        , Attr.style "box-shadow" "0 16px 40px rgba(0,0,0,0.35)"
        , Attr.style "background" "linear-gradient(180deg, #bfe6ff 0%, #d6f0d5 60%, #cfe6bc 100%)"
        , Events.on "mousedown"
            (Decode.map2 MouseDown
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
            )
        , Events.on "click"
            (Decode.map2 Clicked
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
            )
        ]
        allEntities


viewOtherPlayer : Uniforms -> Maybe ClientId -> Player -> WebGL.Entity
viewOtherPlayer uniforms selectedId player =
    let
        isSelected =
            case selectedId of
                Just sid -> sid == player.id
                Nothing -> False

        modelMat =
            Mat4.mul
                (Mat4.makeTranslate player.pos)
                (Mat4.makeRotate player.yaw (vec3 0 1 0))

        highlight = if isSelected then 0.35 else 0.0
        uniforms2 = { uniforms | model = modelMat, time = uniforms.time + highlight }
    in
    WebGL.entity vertexShader fragmentShader otherPlayerMesh uniforms2


viewSelectionRing : Uniforms -> Maybe ClientId -> Player -> List WebGL.Entity
viewSelectionRing uniforms selectedId player =
    case selectedId of
        Just sid ->
            if sid == player.id then
                let
                    ringModel =
                        Mat4.makeTranslate (Vec3.add player.pos (vec3 0 0.05 0))
                    u = { uniforms | model = ringModel }
                in
                [ WebGL.entity vertexShader fragmentShader (selectionRingMesh 1.0) u ]
            else
                []

        Nothing ->
            []


snapshotToPlayer : PlayerSnapshot -> Player
snapshotToPlayer s =
    { id = s.id
    , pos = position3ToVec3 s.pos
    , yaw = s.yaw
    }


viewControls : Html FrontendMsg
viewControls =
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "bottom" "24px"
        , Attr.style "left" "50%"
        , Attr.style "transform" "translateX(-50%)"
        , Attr.style "color" "#172b39"
        , Attr.style "font-family" "monospace"
        , Attr.style "background" "rgba(255,255,255,0.85)"
        , Attr.style "padding" "16px 20px"
        , Attr.style "border-radius" "12px"
        , Attr.style "backdrop-filter" "blur(4px)"
        ]
        [ Html.div [] [ Html.text "WASD: Move (A/D strafe) | Shift: Sprint | Space: Jump | Arrows: Turn | Mouse: Look | C: Camera | Q/E: Zoom" ]
        , Html.button
            [ Events.onClick RespawnClicked
            , Attr.style "margin-top" "10px"
            , Attr.style "background" "#2f7d5d"
            , Attr.style "color" "white"
            , Attr.style "border" "none"
            , Attr.style "padding" "8px 12px"
            , Attr.style "border-radius" "8px"
            , Attr.style "cursor" "pointer"
            ]
            [ Html.text "Respawn" ]
        ]


viewDebugInfo : Model -> Html FrontendMsg
viewDebugInfo model =
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "20px"
        , Attr.style "left" "20px"
        , Attr.style "color" "white"
        , Attr.style "font-family" "monospace"
        , Attr.style "background" "rgba(0,0,0,0.7)"
        , Attr.style "padding" "10px"
        , Attr.style "border-radius" "5px"
        , Attr.style "font-size" "14px"
        ]
        [ Html.div [] [ Html.text ("Other players online: " ++ String.fromInt (List.length model.otherPlayers)) ]
        , Html.div [] [ Html.text ("My position: " ++ 
            "(" ++ String.fromFloat (Vec3.getX model.playerPos) ++ ", " ++ 
            String.fromFloat (Vec3.getY model.playerPos) ++ ", " ++ 
            String.fromFloat (Vec3.getZ model.playerPos) ++ ")") ]
        ]


type alias Uniforms =
    { perspective : Mat4
    , camera : Mat4
    , model : Mat4
    , time : Float
    }


groundMesh : Float -> WebGL.Mesh Vertex
groundMesh size =
    let
        s = size
        y = 0
        green = vec3 0.68 0.82 0.63
        brown = vec3 0.85 0.78 0.63

        v x z color = Vertex (vec3 x y z) color

        tileSize = 30
        tiles = floor (size / tileSize)
        
        makeTile i j =
            let
                x1 = toFloat i * tileSize - s/2
                z1 = toFloat j * tileSize - s/2
                x2 = x1 + tileSize
                z2 = z1 + tileSize
                color = if modBy 2 (i + j) == 0 then green else brown
            in
            [ ( v x1 z1 color, v x2 z1 color, v x2 z2 color )
            , ( v x2 z2 color, v x1 z2 color, v x1 z1 color )
            ]

        allTiles =
            List.concatMap (\i ->
                List.concatMap (\j -> makeTile i j)
                    (List.range 0 tiles)
            ) (List.range 0 tiles)
    in
    WebGL.triangles allTiles


playerMesh : WebGL.Mesh Vertex
playerMesh =
    let
        rft = vec3 0.4 2.0 0.4
        lft = vec3 -0.4 2.0 0.4
        lbt = vec3 -0.4 0 0.4
        rbt = vec3 0.4 0 0.4
        rbb = vec3 0.4 0 -0.4
        rfb = vec3 0.4 2.0 -0.4
        lfb = vec3 -0.4 2.0 -0.4
        lbb = vec3 -0.4 0 -0.4

        blue = vec3 0.35 0.55 0.95
        darkBlue = vec3 0.18 0.30 0.60
        
        v position color = Vertex position color

        face color a b c d =
            [ ( v a color, v b color, v c color )
            , ( v c color, v d color, v a color )
            ]
    in
    WebGL.triangles <|
        List.concat
            [ face blue rft rfb rbb rbt
            , face darkBlue rft rfb lfb lft
            , face blue rft lft lbt rbt
            , face darkBlue rfb lfb lbb rbb
            , face blue lft lfb lbb lbt
            , face darkBlue rbt rbb lbb lbt
            ]


otherPlayerMesh : WebGL.Mesh Vertex
otherPlayerMesh =
    let
        rft = vec3 0.4 2.0 0.4
        lft = vec3 -0.4 2.0 0.4
        lbt = vec3 -0.4 0 0.4
        rbt = vec3 0.4 0 0.4
        rbb = vec3 0.4 0 -0.4
        rfb = vec3 0.4 2.0 -0.4
        lfb = vec3 -0.4 2.0 -0.4
        lbb = vec3 -0.4 0 -0.4

        red = vec3 0.95 0.45 0.35
        darkRed = vec3 0.60 0.25 0.20
        
        v position color = Vertex position color

        face color a b c d =
            [ ( v a color, v b color, v c color )
            , ( v c color, v d color, v a color )
            ]
    in
    WebGL.triangles <|
        List.concat
            [ face red rft rfb rbb rbt
            , face darkRed rft rfb lfb lft
            , face red rft lft lbt rbt
            , face darkRed rfb lfb lbb rbb
            , face red lft lfb lbb lbt
            , face darkRed rbt rbb lbb lbt
            ]


selectionRingMesh : Float -> WebGL.Mesh Vertex
selectionRingMesh radius =
    let
        y = 0
        ringColor = vec3 0.95 0.9 0.4
        segments = 24
        r1 = radius
        r2 = radius + 0.1
        step = 2 * pi / toFloat segments
        v x z color = Vertex (vec3 x y z) color

        quad i =
            let
                a = toFloat i * step
                b = toFloat (i + 1) * step
                x1 = cos a * r1
                z1 = sin a * r1
                x2 = cos b * r1
                z2 = sin b * r1
                x3 = cos b * r2
                z3 = sin b * r2
                x4 = cos a * r2
                z4 = sin a * r2
            in
            [ ( v x1 z1 ringColor, v x2 z2 ringColor, v x3 z3 ringColor )
            , ( v x3 z3 ringColor, v x4 z4 ringColor, v x1 z1 ringColor )
            ]
    in
    WebGL.triangles (List.concatMap quad (List.range 0 (segments - 1)))


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3, vheight : Float }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 model;
        uniform float time;
        varying vec3 vcolor;
        varying float vheight;
        
        void main () {
            gl_Position = perspective * camera * model * vec4(position, 1.0);
            // Pulse color slightly for selected entities using time offset
            float pulse = 0.05 * sin(time * 4.0);
            vcolor = color + vec3(pulse, pulse, pulse);
            vheight = position.y;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3, vheight : Float }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform float time;
        varying vec3 vcolor;
        varying float vheight;
        
        void main () {
            vec3 fogColor = vec3(0.82, 0.90, 0.98);
            float fogAmount = gl_FragCoord.z / gl_FragCoord.w / 100.0;
            fogAmount = clamp(fogAmount, 0.0, 0.45);

            // Miyazaki-like soft warm tint based on height
            float warm = smoothstep(0.0, 2.0, vheight);
            vec3 warmTint = mix(vec3(1.0, 0.95, 0.9), vec3(1.0, 1.0, 1.0), warm);
            vec3 base = vcolor * warmTint;
            
            vec3 finalColor = mix(base, fogColor, fogAmount);
            gl_FragColor = vec4(finalColor, 1.0);
        }
    |]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.map canonicalizeKey <|
        Decode.oneOf
            [ Decode.field "key" Decode.string
            , Decode.field "code" Decode.string
            ]


canonicalizeKey : String -> String
canonicalizeKey raw =
    let
        lower = String.toLower raw
    in
    case lower of
        -- Physical keys mapping (from event.code)
        "keyw" -> "w"
        "keys" -> "s"
        "keya" -> "a"
        "keyd" -> "d"
        "keyq" -> "q"
        "keye" -> "e"
        "shiftleft" -> "shift"
        "shiftright" -> "shift"

        -- Already-processed event.key values
        "shift" -> "shift"
        other -> other