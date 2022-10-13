module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import Math.Matrix4 as Mat4
import Math.Vector3 as Vector3 exposing (Vec3, add, getY, toRecord, vec3)
import Set exposing (Set)
import Skybox
import Task
import Terrain
import Time
import Types exposing (Person, ProgramFlags)
import Utils exposing (formatFloat)
import WebGL exposing (..)


eyeLevel : Float
eyeLevel =
    1.8


type alias Model =
    { skybox : Skybox.Model
    , terrain : Terrain.Model
    , heightmap : Array Float
    , person : Person
    , keys : Set String
    , mouse : ( Float, Float )
    , screen : ( Int, Int )
    }


type alias KeyCode =
    Int


type alias ScreenDimensions =
    ( Int, Int )


type Action
    = NoOp
    | SkyboxAction Skybox.Action
    | WindowResize Int Int
    | TerrainAction Terrain.Action
    | KeyUp String
    | KeyDown String
    | MouseMove ( Float, Float )
    | Frame Float


init : { terrainHeightMap : Array Float } -> ( Model, Cmd Action )
init flags =
    ( { skybox = Skybox.init
      , terrain = Terrain.init
      , person = defaultPerson
      , screen = ( 0, 0 )
      , heightmap = flags.terrainHeightMap
      , mouse = ( 0, 0 )
      , keys = Set.empty
      }
    , Cmd.batch
        [ Task.attempt
            (Result.map SkyboxAction >> Result.withDefault NoOp)
            Skybox.loadTextures
        , Task.attempt
            (Result.map TerrainAction >> Result.withDefault NoOp)
            Terrain.loadTextures
        ]
    )


defaultPerson : Person
defaultPerson =
    { position = vec3 196.05 eyeLevel 218.0
    , velocity = vec3 0.0 0 5.0
    , rotation = 6.18
    , lookVert = 0.3
    }


mouseLook : Float -> Int -> Int -> Person -> Person
mouseLook mouseY w h person =
    { person | lookVert = -(pi / 2.0) + (mouseY / toFloat h) * pi }



-- TODO - replace this with fmod


fixRot : Float -> Float
fixRot rot =
    if rot < 0 then
        rot + (pi * 2.0)

    else if rot > (pi * 2.0) then
        rot - (pi * 2.0)

    else
        rot


walk : Float -> Set String -> Person -> Person
walk delta keys person =
    let
        rotSpeed =
            delta / 100.0

        rot =
            (if Set.member "ArrowLeft" keys then
                -rotSpeed

             else
                0
            )
                + (if Set.member "ArrowRight" keys then
                    rotSpeed

                   else
                    0
                  )

        forward =
            (if Set.member "ArrowUp" keys then
                delta

             else
                0
            )
                + (if Set.member "ArrowDown" keys then
                    -delta

                   else
                    0
                  )

        p =
            toRecord person.position

        rotVal =
            person.rotation + (pi / 2.0)

        vx =
            -(cos rotVal * (forward / 3.0))

        vz =
            -(sin rotVal * (forward / 3.0))
    in
    { person
        | velocity = vec3 vx (getY person.velocity) vz
        , rotation = (person.rotation + (rot * rotSpeed)) |> fixRot
    }


updateFrame : Float -> Model -> Model
updateFrame delta model =
    let
        ( w, h ) =
            model.screen

        ( _, y ) =
            model.mouse

        person =
            model.person
                |> mouseLook y w h
                |> walk delta model.keys
                |> gravity delta
                |> physics model delta
    in
    { model | person = person }


update : Action -> Model -> ( Model, Cmd msg )
update action model =
    case action of
        SkyboxAction a ->
            let
                ( m, cmd ) =
                    Skybox.update a model.skybox
            in
            ( { model | skybox = m }, Cmd.none )

        TerrainAction a ->
            let
                ( m, cmd ) =
                    Terrain.update a model.terrain
            in
            ( { model | terrain = m }, Cmd.none )

        WindowResize width height ->
            ( { model
                | screen = ( width, height )
                , mouse =
                    ( toFloat width / 2, toFloat height / 2 )
              }
            , Cmd.none
            )

        KeyDown key ->
            ( { model | keys = Set.insert key model.keys }, Cmd.none )

        KeyUp key ->
            ( { model | keys = Set.remove key model.keys }, Cmd.none )

        Frame delta ->
            ( updateFrame delta model, Cmd.none )

        MouseMove pos ->
            ( { model | mouse = pos }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


gravity : Float -> Person -> Person
gravity dt person =
    let
        p =
            toRecord person.position
    in
    let
        v =
            toRecord person.velocity
    in
    { person
        | velocity = vec3 v.x (v.y - 0.0025 * dt) v.z
    }


toTuple : { x : Float, y : Float, z : Float } -> ( Float, Float, Float )
toTuple { x, y, z } =
    ( x, y, z )


physics : Model -> Float -> Person -> Person
physics model dt person =
    let
        position =
            add person.position <| Vector3.scale (dt / 500.0) person.velocity

        p =
            toRecord position

        ty =
            eyeLevel + Terrain.getTerrainHeight (toTuple (Vector3.toRecord position)) model.heightmap
    in
    { person
        | position =
            if p.y < ty then
                vec3 p.x ty p.z

            else
                position
    }


fov : Float
fov =
    45


near : Float
near =
    0.5


far : Float
far =
    512.0


perspective : Int -> Int -> Mat4.Mat4
perspective w h =
    Mat4.makePerspective fov (toFloat w / toFloat h) near far


glElement model =
    let
        ( w, h ) =
            model.screen

        perspectiveMatrix =
            perspective w h

        skybox =
            Skybox.makeSkybox perspectiveMatrix model.person model.skybox

        terrain =
            Terrain.view perspectiveMatrix model.person model.terrain
    in
    WebGL.toHtml
        [ Html.Attributes.width w
        , Html.Attributes.height h
        ]
        (skybox ++ terrain)


debugReadout : Model -> Html.Html a
debugReadout model =
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        ]
        [ text <|
            "screen="
                ++ coordsToString model.screen
                ++ "; keys=["
                ++ String.join ", " (Set.toList model.keys)
                ++ "]; camera="
                ++ personToString model.person
                ++ "; terrain="
                ++ String.fromInt (List.length model.terrain)
        ]


coordsToString : ( Int, Int ) -> String
coordsToString ( x, y ) =
    "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


vec3ToString : { x : Float, y : Float, z : Float } -> String
vec3ToString { x, y, z } =
    "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ", " ++ String.fromFloat z ++ ")"


personToString { velocity, position, rotation, lookVert } =
    "{ position = ("
        ++ vec3ToString (Vector3.toRecord position)
        ++ ")"
        ++ ", velocity = ("
        ++ vec3ToString (Vector3.toRecord velocity)
        ++ "), rotation = "
        ++ String.fromFloat rotation
        ++ ", lookVert = "
        ++ String.fromFloat lookVert
        ++ ") }"


view : Model -> Browser.Document Action
view model =
    { title = "Terrain in Elm"
    , body =
        [ div [ Mouse.onMove (.clientPos >> MouseMove) ] [ glElement model ]
        ]
    }



--, debugReadout model ]


main : Program ProgramFlags Model Action
main =
    Browser.document
        { init = init
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onResize WindowResize
                    , Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
                    , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
                    , Browser.Events.onAnimationFrameDelta Frame
                    ]
        , update = update
        }
