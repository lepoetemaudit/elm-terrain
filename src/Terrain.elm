import Graphics.Element exposing (..)
import Keyboard
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Task exposing (Task)
import Text
import Time exposing (..)
import WebGL exposing (..)
import Window
import Array exposing (Array)
import String


-- MODEL

type alias Person =
    { position : Vec3
    , velocity : Vec3
    , rotation : Float
    }


type alias Inputs =
    ( Bool, {x:Int, y:Int}, Float, Array Float )

sectorSize : number
sectorSize = 32

fmod : Float -> Int -> Float
fmod f n =
  let integer = floor f
  in  toFloat (integer % n) + f - toFloat integer

eyeLevel : Float
eyeLevel = 1.6


defaultPerson : Person
defaultPerson =
  { position = vec3 64.01 eyeLevel 64.01
  , velocity = vec3 0 0 0
  , rotation = pi
  }


-- UPDATE

update : Inputs -> Person -> Person
update (isJumping, directions, dt, th) person =
  person
    |> walk directions
    |> jump isJumping
    |> gravity dt
    |> physics dt th



fixRot : Float -> Float
fixRot rot =
  if rot < 0 then
    rot + (pi * 2.0)
  else if rot > (pi * 2.0) then
    rot - (pi * 2.0)
  else
    rot

walk : { x:Int, y:Int } -> Person -> Person
walk directions person =
  let
    p = toRecord person.position
    rot = toFloat directions.x
    rotVal = person.rotation + (pi / 2.0)
    forward = toFloat directions.y
    vx = -(cos(rotVal) * forward * 5.0)
    vz = -(sin(rotVal) * forward * 5.0)
    in
      { person |
          velocity = vec3 vx (getY person.velocity) vz,
          rotation = (person.rotation + (rot / 50.0)) |> fixRot
      }

dist x y =
  sqrt (abs (x*x)) + (abs (y*y))

getHeight hmap (x, z) =
  Array.get (x + ((512 - z) * 512)) hmap
  |> Maybe.withDefault 5.0

getTerrainHeight : (Float, Float, Float) -> Array Float -> Float
getTerrainHeight (x, _, z) hmap =
  let
    (x1, z1) = (floor x, floor z)
    (x2, z2) = (x1 + 1, z1 + 1)
    gh = (getHeight hmap)
    (h11, h21, h12, h22) = ( gh (x1, z1)
                           , gh (x2, z1)
                           , gh (x2, z2)
                           , gh (x1, z2) )
    in
      (h11 * ((toFloat x2) - x) * ((toFloat z2) - z) +
       h21 * (x - (toFloat x1)) * ((toFloat z2) - z) +
       h12 * ((toFloat x2) - x) * (z - (toFloat z1)) +
       h22 * (x - (toFloat x1)) * (z - (toFloat z1))
      )

jump : Bool -> Person -> Person
jump isJumping person =
  if not isJumping || getY person.position > eyeLevel then
    person
  else
    let
      (vx,_,vz) = toTuple person.velocity
    in
      { person |
          velocity = vec3 vx 5 vz
      }


physics : Float -> Array Float -> Person -> Person
physics dt th person =
  let
    position =
      person.position `add` V3.scale dt person.velocity
    p = toRecord position
    ty = eyeLevel + getTerrainHeight (toTuple position) th
  in
    { person |
        position = if p.y < ty then vec3 p.x ty p.z else position
    }


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


type alias Sector =
  { texturePos : (Float, Float)
  , position : (Float, Float)
  }

getSectorPos : Float -> Float -> ( Float, Float )
getSectorPos x z =
  (x - (fmod x sectorSize), z - (fmod z sectorSize))

makeSector rowNum person fov colNum =
  let
    (pX, _, pZ) = toTuple person.position
    length = (toFloat rowNum) * sectorSize
    -- TODO - this trig stinks. Fix it.
    rot = person.rotation - (pi / 2.0) - fov

    cross = rot + (fov * 3.0)

    sX = (cos rot) * length
    sZ = (sin rot) * length

    x = sX + ((cos cross) * sectorSize * (toFloat colNum))
    z = sZ + ((sin cross) * sectorSize * (toFloat colNum))

    texturePos = getSectorPos (x + pX) (z + pZ)
    userSector = getSectorPos pX pZ
    sectorPos = (fst texturePos - fst userSector, snd texturePos - snd userSector)

  in
    { texturePos = texturePos
     , position = sectorPos
    }



getSectorRow : Int -> Person -> Float -> List Sector
getSectorRow rowNum person fov =
  let
      length = (toFloat rowNum) * sectorSize
      width = (cos fov) * length
      cols = [-1..((width / sectorSize) * 2.0 |> round)+1]
  in
      List.map (makeSector rowNum person fov) cols



getSectors : Person -> List Sector
getSectors person =
  let
    (x, _, z) = toTuple person.position
    -- Get initial sector position
    (sx, sy) = (x - (fmod x sectorSize), z - (fmod z sectorSize))
    rows = [0..7]
  in
    List.concatMap (\r -> getSectorRow r person (degrees 45.0)) rows


-- SIGNALS
world : List Texture -> Mat4 -> Person -> List Renderable
world textures perspective person =
  case textures of
    [tex, tex2, hmap] ->
      List.map
        (\sector ->
          (render vertexShader
                  fragmentShader
                  sectorBlock  { stone=tex
                               , soil=tex2
                               , heightmap=hmap
                               , texPos=Math.Vector2.vec2 (fst sector.texturePos) (snd sector.texturePos)
                               , sectorPos=Math.Vector2.vec2 (fst sector.position) (snd sector.position)
                               , perspective=perspective }))
        (getSectors person)

    _ -> []

person : Signal Person
person =
  Signal.foldp update defaultPerson inputs

getTexPos : { a | position : Vec3 } -> Vec2
getTexPos person =
  let
    (x, _, z) = toTuple person.position
  in
    Math.Vector2.vec2 ((toFloat (floor x)) + 0.0)
                      ((toFloat (floor z)) + 0.0)

main : Signal Element
main =
  let
    entities =
      Signal.map3 world
        textures.signal
        (Signal.map2 perspective Window.dimensions person)
        person

    terrain =
      Signal.map2 getTerrainHeight
        (Signal.map (.position >> toTuple) person)
        terrainHeightMap
  in
    Signal.map4 view Window.dimensions entities person terrain


textures : Signal.Mailbox (List Texture)
textures = Signal.mailbox []

port fetchTextures : Task WebGL.Error ()
port fetchTextures =
  Task.sequence
    [ loadTextureWithFilter Linear "texture/tundra.jpg"
    , loadTextureWithFilter Linear "texture/grass.jpg"
    , loadTextureWithFilter Linear "texture/heightmap.png"
    ]
    `Task.andThen` (\tex -> Signal.send textures.address tex)


port terrainHeightMap : Signal (Array Float)

inputs : Signal Inputs
inputs =
  let
    dt = Signal.map (\t -> t/500) (fps 60)
  in
    Signal.map4 (,,,) Keyboard.space (Signal.merge Keyboard.wasd Keyboard.arrows) dt terrainHeightMap
      |> Signal.sampleOn dt


-- VIEW

perspective : (Int,Int) -> Person -> Mat4
perspective (w,h) person =
  let
    (x, y, z) = Math.Vector3.negate person.position |> toTuple
    camera = vec3 ((fmod x sectorSize) - sectorSize) y ((fmod z sectorSize) - sectorSize)
  in
    (makePerspective 45 (toFloat w / toFloat h) 0.01 250)
    `mul` makeRotate person.rotation (vec3 0 1 0.0)
    `mul` makeTranslate camera


view : (Int,Int) -> List Renderable -> Person -> Float -> Element
view (w,h) entities person th =
  layers
    [ webgl (w,h) entities
    --, container w 100 position message
    , container w 100 position (cameraOutput person th)
    ]


position : Position
position =
  midLeftAt (absolute 10) (relative 0.1)


formatFloat : String -> String
formatFloat val =
  String.foldl (\el (str, dist) ->
    if dist == -1 then
      case el of
        '.' -> ((str ++ "."), 0)
        _ -> ((str ++ (String.fromChar el)), -1)
    else if dist < 2 then
      ((str ++ (String.fromChar el)), (dist+1))
    else (str, dist)
    ) ("", -1) val
    |> fst

cameraOutput : Person -> Float -> Element
cameraOutput person th =
  let
    (x, y, z) = toTuple person.position
    pos =
      List.map (toString >> formatFloat) [x, y, z, (person.rotation / (pi * 2.0)) * 360.0, th]
      |> String.join ", "
  in
    leftAligned <| Text.monospace <| Text.fromString ("Pos: " ++ pos)

-- Define the mesh for a terrain slice

type alias Vertex =
    { position : Vec3
    , coord : Vec3
    }


makeTile : Int -> Int -> List (Vertex, Vertex, Vertex)
makeTile sectorSize pos =
  let
    x = (pos % sectorSize  |> toFloat)
    y = (pos // sectorSize |> toFloat)
    topLeft     = Vertex (vec3 x     0 (y+1)) (vec3 0 1 0)
    topRight    = Vertex (vec3 (x+1) 0 (y+1)) (vec3 1 1 0)
    bottomLeft  = Vertex (vec3 x     0  y)    (vec3 0 0 0)
    bottomRight = Vertex (vec3 (x+1) 0  y)    (vec3 1 0 0)
  in
    [ (topLeft,topRight,bottomLeft)
    , (bottomLeft,topRight,bottomRight)
    ]

sectorBlock : Drawable Vertex
sectorBlock = Triangle (List.concatMap (makeTile sectorSize) [0..(sectorSize*sectorSize)-1])
-- Shaders

vertexShader : Shader { position:Vec3, coord:Vec3 } { u | perspective:Mat4, heightmap:Texture, texPos:Vec2, sectorPos: Vec2 } { vcoord:Vec2, blend: Float, dist: Float, normal: Vec3 }
vertexShader = [glsl|

precision mediump float;

attribute vec3 position;
attribute vec3 coord;

uniform mat4 perspective;
uniform vec2 sectorPos;
varying vec2 vcoord;
uniform vec2 texPos;
varying float blend;
varying float dist;
varying vec3 normal;
uniform sampler2D heightmap;

float height(vec2 pos) {
    vec4 texel = texture2DLod(heightmap, pos, 0.0);
    return texel.x * 16.0;
}

void main () {
  vec2 texelPos = (position.xz + texPos) / 512.0;
  float vHeight = height(texelPos);
  vec2 pos = position.xz + sectorPos;
  vec4 outputPos = perspective * vec4(pos.x, vHeight, pos.y, 1.0);
  vcoord = coord.xy;
  //blend = vHeight;
  blend = vHeight / 8.0;
  gl_Position = outputPos;
  dist = outputPos.z;

  /* Calculate normal */
  vec3 off = vec3(1.0 / 512.0, 1.0 / 512.0, 0.0);
  float hL = height(texelPos.xy - off.xz);
  float hR = height(texelPos.xy + off.xz);
  float hD = height(texelPos.xy - off.zy);
  float hU = height(texelPos.xy + off.zy);

  vec3 n;

  n.x = hL - hR;
  n.y = hD - hU;
  n.z = 2.0;
  n = normalize(n);

  normal = n;
}

|]


fragmentShader : Shader {} { u | stone:Texture, soil:Texture} { vcoord:Vec2, blend: Float, dist: Float, normal: Vec3 }
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D stone;
uniform sampler2D soil;
varying vec2 vcoord;
varying vec3 normal;
varying float blend;
varying float dist;

void main () {
  vec4 a = texture2D(stone, vcoord);
  vec4 b = texture2D(soil, vcoord);
  // Get the base colour from the textures
  vec4 colVal = mix(b, a, blend);
  // Apply the horizon blend
  vec4 horizon = vec4(0.8, 1.0, 1.0, 1.0);

  vec4 darken = vec4(0.0, 0.0, 0.0, 1.0);
  colVal = mix(colVal, darken, normal.x);

  if (dist > 160.0) {
    discard;
  } else if (dist > 128.0) {
    gl_FragColor = mix(colVal, horizon, (dist - 128.0) / 32.0);
  } else {
    gl_FragColor = colVal;
  }
}

|]
