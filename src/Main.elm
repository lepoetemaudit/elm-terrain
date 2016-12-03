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

import Terrain
import Skybox
import Types exposing (Person, ProgramFlags)
import Utils exposing (formatFloat)

eyeLevel : Float
eyeLevel = 30.6

type alias Model =
  { skybox : Skybox.Model
  , terrain : Terrain.Model
  , person : Person
  , keys : {x: Int, y: Int}
  , screen : (Int, Int) }

type alias ScreenDimensions = ( Int, Int )

type Action 
  = NoOp
  | SkyboxAction Skybox.Action
  | WindowResize Window.Size
  | TerrainAction Terrain.Action
  --| SkyboxAction Skybox.Action
  --| Keyboard {x: Int, y: Int}
  --| NoOp
  
init hmap =
  ( { skybox = Skybox.init
    , terrain = Terrain.init
    , person = defaultPerson
    , screen = (0, 0)
    , keys = { x = 0, y = 0 } }
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
  { position = vec3 128.01 eyeLevel 48.00
  , velocity = vec3 0 0 0
  , rotation = pi
  , lookVert = 0.0
  }

type alias Inputs =
    ( {x:Int, y:Int}, Float, Array Float, (Int, (Int, Int)) )

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
    {--
    Tick delta dim -> nofx { model | screen = dim
                                   , person = walk delta model.keys model.person
                                  |> physics delta } --}

    {--
    SkyboxAction act ->
      let (m, fx) = Skybox.update act model.skybox
      in
        ( { model | skybox = m}, Effects.map SkyboxAction fx )

    TerrainAction act ->
      let (m, fx) = Terrain.update act model.terrain
      in
        ( { model | terrain = m}, Effects.map TerrainAction fx ) --}


    -- Keyboard keys -> nofx { model | keys = keys }

    NoOp -> model ! []


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

physics : Float -> Person -> Person
physics dt person =
  let
    position =
      add person.position <| Vector3.scale (dt / 500.0) person.velocity
    p = toRecord position
    --ty = eyeLevel + Terrain.getTerrainHeight (toTuple position) th
    ty = 1.0
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
near = 0.10

far : Float
far = 256.0

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
        , subscriptions = (\_ -> Window.resizes WindowResize)
        , update = update
        }