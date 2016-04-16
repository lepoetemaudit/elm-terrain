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
import Mouse
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
  , screen : (Int, Int) }

type alias ScreenDimensions = ( Int, Int )

type Action
  = TerrainAction Terrain.Action
  | SkyboxAction Skybox.Action
  | Tick Time.Time ScreenDimensions
  | NoOp


tickWithDimensions : Signal Action
tickWithDimensions =
  Signal.map2 Tick (fps 30) Window.dimensions

initEffects : Effects Action
initEffects =
  List.map (Effects.map SkyboxAction) Skybox.initEffects
  |> Effects.batch

init : (Model, Effects Action)
init =
  ( { skybox = Skybox.init
    , screen = (0, 0) }
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


walk : { x : Int, y : Int } -> Person -> Person
walk directions person =
  let
    p = toRecord person.position
    rot = toFloat directions.x
    rotVal = person.rotation + (pi / 2.0)
    forward = toFloat directions.y
    vx = -(cos(rotVal) * forward * 4.0)
    vz = -(sin(rotVal) * forward * 4.0)
    in
      { person |
          velocity = vec3 vx (getY person.velocity) vz,
          rotation = (person.rotation + (rot / 40.0)) |> fixRot
      }

-- UPDATE

{--
update : Inputs -> Person -> Person
update (directions, dt, th, (mouseY, (w, h))) person =
  person
    |> walk directions
    |> mouseLook mouseY w h
    |> gravity dt
    |> physics dt th
--}

nofx : Model -> ( Model, Effects a)
nofx a = (a , Effects.none)

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Tick delta dim -> nofx { model | screen = dim }

    SkyboxAction act ->
      let (m, fx) = Skybox.update act model.skybox
      in
        ( { model | skybox = m}, Effects.map SkyboxAction fx )

    -- TODO - delegate to terrain
    TerrainAction _ -> nofx model

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

physics : Float -> Array Float -> Person -> Person
physics dt th person =
  let
    position =
      person.position `add` Vector3.scale dt person.velocity
    p = toRecord position
    ty = eyeLevel + Terrain.getTerrainHeight (toTuple position) th
  in
    { person |
        position = if p.y < ty then vec3 p.x ty p.z else position
    }


port terrainHeightMap : Signal (Array Float)


inputs : Signal Inputs
inputs =
  let
    dt = Signal.map (\t -> t/500) (fps 60)
  in
    Signal.map4
      (,,,)
      (Signal.merge Keyboard.wasd Keyboard.arrows)
      dt
      terrainHeightMap
      (Signal.map2 (,) Mouse.y Window.dimensions)
      |> Signal.sampleOn dt


{--
view : (Int,Int) -> List Renderable -> Person -> Float -> Element
view (w,h) entities person th =
  layers
    [ webgl (w,h) entities
    , container w 100 (midLeftAt (absolute 10) (relative 0.1))
                      (cameraOutput person th)
    ]

--}





cameraOutput : Person -> Float -> Element
cameraOutput person th =
  let
    (x, y, z) = toTuple person.position
    pos =
      List.map (toString >> formatFloat) [x, y, z, (person.rotation / (pi * 2.0)) * 360.0, th]
      |> String.join ", "
  in
    leftAligned <| Text.monospace <| Text.fromString ("Pos: " ++ pos)


{--
gl : Signal Element
gl =
  let
    skyTexMb = Skybox.textures
    persp = (Signal.map2 Terrain.perspective Window.dimensions person)
    skypersp = (Signal.map2 Skybox.perspective Window.dimensions person)
    entities =
      Signal.map2
        (++)
        (Signal.map2 Skybox.makeSkybox skypersp skyTexMb.signal)
        (Signal.map3 Terrain.view terrainTexMb.signal persp person)

    terrain =
      Signal.map2 Terrain.getTerrainHeight
        (Signal.map (.position >> toTuple) person)
        terrainHeightMap
  in
    Signal.map4 view Window.dimensions entities person terrain
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

glElement : Model -> Element
glElement model =
  let
    (w, h) = model.screen
    perspectiveMatrix = perspective w h
    skybox = Skybox.makeSkybox perspectiveMatrix model.skybox
  in
    webgl model.screen skybox

debugReadout : Model -> Html.Html
debugReadout model =
  div
    [ style
        [ ("position", "absolute")
        , ("top", "0")
        , ("left", "0") ] ]

    [ text <| toString model.screen]

view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [] [ Html.fromElement <| glElement model
         , debugReadout model ]


app : StartApp.App Model
app =
  start { init = init
        , update = update
        , view = view
        , inputs = [ tickWithDimensions ] }

main : Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks