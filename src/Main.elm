import Array exposing (Array)
import Window

import WebGL exposing (..)
import Math.Vector3 as Vector3 exposing (toTuple, vec3, toRecord, add, getY)
import Keyboard
import Graphics.Element exposing (..)
import Text
import Mouse
import String
import Time exposing (fps)
import Task exposing (Task)

import Terrain
import Skybox
import Types exposing (Person)
import Utils exposing (formatFloat)

eyeLevel : Float
eyeLevel = 1.6

defaultPerson : Person
defaultPerson =
  { position = vec3 128.01 eyeLevel 48.00
  , velocity = vec3 0 0 0
  , rotation = pi
  , lookVert = 0.0
  }


type alias Inputs =
    ( Bool, {x:Int, y:Int}, Float, Array Float, (Int, (Int, Int)) )

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

update : Inputs -> Person -> Person
update (isJumping, directions, dt, th, (mouseY, (w, h))) person =
  person
    |> walk directions
    |> mouseLook mouseY w h
    |> gravity dt
    |> physics dt th


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


person : Signal Person
person =
  Signal.foldp update defaultPerson inputs


terrainTexMb : Signal.Mailbox (List Texture)
terrainTexMb = Terrain.textures

port fetchTextures : Task WebGL.Error ()
port fetchTextures =
  Task.sequence
    [ loadTextureWithFilter Linear "texture/grass.jpg"
    , loadTextureWithFilter Linear "texture/soil.jpg"
    , loadTextureWithFilter Linear "texture/tundra.jpg"
    , loadTextureWithFilter Linear "texture/attributemap.png"
    ]
    `Task.andThen` (\tex -> Signal.send terrainTexMb.address tex)

port skyboxTextures : Task WebGL.Error()
port skyboxTextures = Skybox.getTextures

port terrainHeightMap : Signal (Array Float)


inputs : Signal Inputs
inputs =
  let
    dt = Signal.map (\t -> t/500) (fps 60)
  in
    Signal.map5
      (,,,,)
      Keyboard.space
      (Signal.merge Keyboard.wasd Keyboard.arrows)
      dt
      terrainHeightMap
      (Signal.map2 (,) Mouse.y Window.dimensions)
      |> Signal.sampleOn dt

view : (Int,Int) -> List Renderable -> Person -> Float -> Element
view (w,h) entities person th =
  layers
    [ webgl (w,h) entities
    , container w 100 (midLeftAt (absolute 10) (relative 0.1))
                      (cameraOutput person th)
    ]



cameraOutput : Person -> Float -> Element
cameraOutput person th =
  let
    (x, y, z) = toTuple person.position
    pos =
      List.map (toString >> formatFloat) [x, y, z, (person.rotation / (pi * 2.0)) * 360.0, th]
      |> String.join ", "
  in
    leftAligned <| Text.monospace <| Text.fromString ("Pos: " ++ pos)

main : Signal Element
main =
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
