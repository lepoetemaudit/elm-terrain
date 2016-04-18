import StartApp exposing (start)
import Effects exposing (Effects)
import Array exposing (Array)
import Window
import Html exposing (div, text)
import Html.Attributes exposing (style)

import WebGL exposing (..)
import Math.Vector3 as Vector3 exposing (toTuple, vec3, toRecord, add, getY)
import Math.Matrix4 as Mat4
import Keyboard
import Graphics.Element exposing (..)
import Task
import Text
import String
import Time exposing (fps)

import Terrain
import Skybox
import Types exposing (Person)
import Utils exposing (formatFloat)

eyeLevel : Float
eyeLevel = 1.6

type alias Model =
  { skybox : Skybox.Model
  , person : Person
  , keys : {x: Int, y: Int}
  , screen : (Int, Int) }

type alias ScreenDimensions = ( Int, Int )

type Action
  = TerrainAction Terrain.Action
  | SkyboxAction Skybox.Action
  | Tick Time.Time ScreenDimensions
  | Keyboard {x: Int, y: Int}
  | NoOp


tickWithDimensions : Signal Action
tickWithDimensions =
  Signal.map2 Tick (fps 60) Window.dimensions

initEffects : Effects Action
initEffects =
  List.map (Effects.map SkyboxAction) Skybox.initEffects
  |> Effects.batch

init : (Model, Effects Action)
init =
  ( { skybox = Skybox.init
    , person = defaultPerson
    , screen = (0, 0)
    , keys = { x = 0, y = 0 } }
  , initEffects )

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
      { person |
          velocity = vec3 vx (getY person.velocity) vz,
          rotation = (person.rotation + (rot * rotSpeed)) |> fixRot
      }

nofx : Model -> ( Model, Effects a)
nofx a = (a , Effects.none)

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Tick delta dim -> nofx { model | screen = dim
                                   , person = walk delta model.keys model.person
                                   |> physics delta }


    SkyboxAction act ->
      let (m, fx) = Skybox.update act model.skybox
      in
        ( { model | skybox = m}, Effects.map SkyboxAction fx )

    -- TODO - delegate to terrain
    TerrainAction _ -> nofx model

    Keyboard keys -> nofx { model | keys = keys }

    NoOp -> nofx model


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
      person.position `add` Vector3.scale dt person.velocity
    p = toRecord position
    --ty = eyeLevel + Terrain.getTerrainHeight (toTuple position) th
    ty = 1.0
  in
    { person |
        position = if p.y < ty then vec3 p.x ty p.z else position
    }


port terrainHeightMap : Signal (Array Float)


cameraOutput : Person -> Float -> Element
cameraOutput person th =
  let
    (x, y, z) = toTuple person.position
    pos =
      List.map (toString >> formatFloat) [x, y, z, (person.rotation / (pi * 2.0)) * 360.0, th]
      |> String.join ", "
  in
    leftAligned <| Text.monospace <| Text.fromString ("Pos: " ++ pos)

fov : Float
fov = 45

near : Float
near = 0.10

far : Float
far = 256.0

perspective : Int -> Int -> Mat4.Mat4
perspective w h =
  Mat4.makePerspective fov (toFloat w / toFloat h) near far

glElement : Model -> Element
glElement model =
  let
    (w, h) = model.screen
    perspectiveMatrix = perspective w h
    skybox = Skybox.makeSkybox perspectiveMatrix model.person model.skybox
  in
    webgl model.screen skybox

debugReadout : Model -> Html.Html
debugReadout model =
  div
    [ style
        [ ("position", "absolute")
        , ("top", "0")
        , ("left", "0") ] ]

    [ text <| "screen=" ++ (toString model.screen) ++
      "; keys=" ++ (toString model.keys) ++
      "; camera=" ++ (toString model.person)]

view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [] [ Html.fromElement <| glElement model
         , debugReadout model ]

keyboard : Signal Action
keyboard =
  Signal.merge Keyboard.wasd Keyboard.arrows
  |> Signal.map Keyboard

app : StartApp.App Model
app =
  start { init = init
        , update = update
        , view = view
        , inputs = [ tickWithDimensions, keyboard ] }

main : Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks