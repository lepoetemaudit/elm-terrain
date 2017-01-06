import Array exposing (Array)
import Window
import Html exposing (div, text)
import Html.Attributes exposing (style)

import WebGL exposing (..)
import Math.Vector3 as Vector3 exposing (toTuple, vec3, toRecord, add, getY)
import Math.Matrix4 as Mat4
import Keyboard
import String
import Task
import Set exposing (Set)
import AnimationFrame
import Time exposing (Time)

import Terrain
import Skybox
import Types exposing (Person, ProgramFlags)
import Utils exposing (formatFloat)

eyeLevel : Float
eyeLevel = 3.6

type alias Model =
  { skybox : Skybox.Model
  , terrain : Terrain.Model
  , heightmap : Array Float
  , person : Person
  , keys : Set Keyboard.KeyCode
  , screen : (Int, Int) }

type alias ScreenDimensions = ( Int, Int )

type Action 
  = NoOp
  | SkyboxAction Skybox.Action
  | WindowResize Window.Size
  | TerrainAction Terrain.Action
  | KeyUp Keyboard.KeyCode
  | KeyDown Keyboard.KeyCode
  | Frame Time
  
init flags =
  ( { skybox = Skybox.init
    , terrain = Terrain.init
    , person = defaultPerson
    , screen = (0, 0)
    , heightmap = flags.terrainHeightMap
    , keys = Set.empty }
  , Cmd.batch 
      [ Task.attempt 
          ((Result.map SkyboxAction) >> (Result.withDefault NoOp))
          Skybox.loadTextures  
      , Task.attempt 
          ((Result.map TerrainAction) >> (Result.withDefault NoOp))
          Terrain.loadTextures  
      , Task.perform WindowResize Window.size
      ]
  )

defaultPerson : Person
defaultPerson =
  { position = vec3 108.05 eyeLevel 44.00
  , velocity = vec3 0.0 0 5.0
  , rotation = pi
  , lookVert = 0.0
  }

mouseLook : Int -> Int -> Int -> Person -> Person
mouseLook mouseY w h person =
  { person | lookVert = -(pi/2.0) + ((toFloat mouseY) / (toFloat h)) * pi  }

-- TODO - replace this with fmod
fixRot : Float -> Float
fixRot rot =
  if rot < 0 then
    rot + (pi * 2.0)
  else if rot > (pi * 2.0) then
    rot - (pi * 2.0)
  else
    rot


walk : Float -> { x : Int, y : Int } -> Person -> Person
walk delta directions person =
  let
    p = toRecord person.position
    rot = toFloat directions.x
    rotSpeed = delta / 1000.0
    rotVal = person.rotation + (pi / 2.0)
    forward = toFloat directions.y
    vx = -(cos(rotVal) * forward * 4.0)
    vz = -(sin(rotVal) * forward * 4.0)
  in
    { person 
      | velocity = vec3 vx (getY person.velocity) vz
      , rotation = (person.rotation + (rot * rotSpeed)) |> fixRot
    }

updateFrame : Float -> Model -> Model
updateFrame delta model =
  let person = model.person |> gravity delta |> physics model delta in
    { model | person = person }

update : Action -> Model -> ( Model, Cmd msg )
update action model =  
  case action of
    SkyboxAction a -> 
      let (m, cmd) = Skybox.update a model.skybox
      in 
        { model | skybox = m } ! [cmd]

    TerrainAction a ->
      let (m, cmd) = Terrain.update a model.terrain
      in
        { model | terrain = m} ! [cmd]

    WindowResize s -> { model | screen = (s.width, s.height) } ! []

    KeyDown key -> { model | keys = model.keys } ! []

    KeyUp key -> { model | keys = model.keys } ! []

    Frame delta -> updateFrame delta model ! []

    NoOp -> model ! []
    -- Keyboard keys -> nofx { model | keys = keys }



gravity : Float -> Person -> Person
gravity dt person =
  let
    p = toRecord person.position
  in
    let
      v = toRecord person.velocity
    in
      { person |
          velocity = vec3 v.x (v.y - 0.25 * dt) v.z
      }

physics : Model -> Float -> Person -> Person
physics model dt person =
  let
    position =
      add person.position <| Vector3.scale (dt / 500.0) person.velocity
    p = toRecord position
    ty = eyeLevel + Terrain.getTerrainHeight (toTuple position) model.heightmap
    
  in
    { person |
        position = if p.y < ty then vec3 p.x ty p.z else position
    }

{--
cameraOutput : Person -> Float -> Element
cameraOutput person th =
  let
    (x, y, z) = toTuple person.position
    pos =
      List.map (toString >> formatFloat) [x, y, z, (person.rotation / (pi * 2.0)) * 360.0, th]
      |> String.join ", "
  in
    leftAligned <| Text.monospace <| Text.fromString ("Pos: " ++ pos)
--}

fov : Float
fov = 45

near : Float
near = 0.2

far : Float
far = 280.0

perspective : Int -> Int -> Mat4.Mat4
perspective w h =
  Mat4.makePerspective fov (toFloat w / toFloat h) near far

glElement model =
  let
    (w, h) = model.screen
    perspectiveMatrix = perspective w h
    skybox = Skybox.makeSkybox perspectiveMatrix model.person model.skybox
    terrain = Terrain.view perspectiveMatrix model.person model.terrain 
  in
    WebGL.toHtml [ Html.Attributes.width w
                 , Html.Attributes.height h ] 
                 (skybox ++ terrain)

debugReadout : Model -> Html.Html a
debugReadout model =
  div
    [ style
        [ ("position", "absolute")
        , ("top", "0")
        , ("left", "0") ] ]

    [ text <| "screen=" ++ (toString model.screen) ++
      "; keys=" ++ (toString model.keys) ++
      "; camera=" ++ (toString model.person) ++
      "; terrain=" ++ (toString (List.length model.terrain))]

view : Model -> Html.Html msg
view model =
  div [] [ glElement model ]

main : Program ProgramFlags Model Action
main =
    Html.programWithFlags
        { init = init
        , view = view
        , subscriptions = (\_ -> Sub.batch [ Window.resizes WindowResize 
                                           , Keyboard.downs KeyDown
                                           , Keyboard.ups KeyUp
                                           , AnimationFrame.diffs Frame ] )
        , update = update
        }