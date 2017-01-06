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
import Mouse

import Terrain
import Skybox
import Types exposing (Person, ProgramFlags)
import Utils exposing (formatFloat)

eyeLevel : Float
eyeLevel = 2.6

type alias Model =
  { skybox : Skybox.Model
  , terrain : Terrain.Model
  , heightmap : Array Float
  , person : Person
  , keys : Set Keyboard.KeyCode
  , mouse : Mouse.Position
  , screen : (Int, Int) }

type alias ScreenDimensions = ( Int, Int )

type Action 
  = NoOp
  | SkyboxAction Skybox.Action
  | WindowResize Window.Size
  | TerrainAction Terrain.Action
  | KeyUp Keyboard.KeyCode
  | KeyDown Keyboard.KeyCode
  | MouseMove Mouse.Position
  | Frame Time
  
init flags =
  ( { skybox = Skybox.init
    , terrain = Terrain.init
    , person = defaultPerson
    , screen = (0, 0)
    , heightmap = flags.terrainHeightMap
    , mouse = { x = 0, y = 0}
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


walk : Float -> Set Keyboard.KeyCode -> Person -> Person
walk delta keys person =
  let
    rotSpeed = delta / 100.0
    rot = (if (Set.member 65 keys) then -rotSpeed else 0) +
          (if (Set.member 68 keys) then rotSpeed else 0)
    forward = (if (Set.member 87 keys) then delta else 0) +
              (if (Set.member 83 keys) then -delta else 0)    
    p = toRecord person.position    
    rotVal = person.rotation + (pi / 2.0)
    vx = -(cos(rotVal) * (forward / 2.0))
    vz = -(sin(rotVal) * (forward / 2.0))
  in
    { person 
      | velocity = vec3 vx (getY person.velocity) vz
      , rotation = (person.rotation + (rot * rotSpeed)) |> fixRot
    }

updateFrame : Float -> Model -> Model
updateFrame delta model =
  let 
      (w, h) = model.screen
      person = model.person 
               |> mouseLook model.mouse.y w h
               |> walk delta model.keys 
               |> gravity delta 
               |> physics model delta in
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

    KeyDown key -> { model | keys = Set.insert key model.keys } ! []

    KeyUp key -> { model | keys = Set.remove key model.keys } ! []

    Frame delta -> updateFrame delta model ! []

    MouseMove pos -> { model | mouse = pos } ! []

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

fov : Float
fov = 45

near : Float
near = 1.0

far : Float
far = 250.0

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
         -- , debugReadout model ]

main : Program ProgramFlags Model Action
main =
    Html.programWithFlags
        { init = init
        , view = view
        , subscriptions = (\_ -> Sub.batch [ Window.resizes WindowResize 
                                           , Keyboard.downs KeyDown
                                           , Keyboard.ups KeyUp
                                           , AnimationFrame.diffs Frame
                                           , Mouse.moves MouseMove ] )
        , update = update
        }