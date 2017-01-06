module Terrain exposing (..)

import Array exposing (Array)
import Task

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import WebGL exposing (Shader, triangles, entity, entityWith, Entity)
import WebGL.Settings as Settings
import WebGL.Texture as Texture exposing (..)
import WebGL.Settings.DepthTest as DepthTest
import Tuple exposing (first, second)

import Types exposing (Person)


type Action = TexturesLoaded (List Texture)
type alias Model = List Texture

init : Model
init = []

update a model =
  case a of
    TexturesLoaded texes ->
      texes ! []

sectorSize : number
sectorSize = 32

fmod : Float -> Int -> Float
fmod f n =
  let integer = floor f
  in  toFloat (integer % n) + f - toFloat integer



getHeight : Array Float -> ( Int, Int ) -> Float
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
                           , gh (x1, z2)
                           , gh (x2, z2) )
  in

      (h11 * ((toFloat x2) - x) * ((toFloat z2) - z) +
       h21 * (x - (toFloat x1)) * ((toFloat z2) - z) +
       h12 * ((toFloat x2) - x) * (z - (toFloat z1)) +
       h22 * (x - (toFloat x1)) * (z - (toFloat z1))
      )


type alias Sector =
  { texturePos : (Float, Float)
  , position : (Float, Float)
  }

getSectorPos : Float -> Float -> ( Float, Float )
getSectorPos x z =
  (x - (fmod x sectorSize), z - (fmod z sectorSize))

makeSector : Int -> Person -> Float -> Int -> Maybe Sector
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
    sectorPos = (first texturePos - first userSector, second texturePos - second userSector)

    (tx, ty) = texturePos

  in
    if tx >= -1.0 && ty >= -1.0 && tx <= 512 && ty <= 512 then
      Just { texturePos = texturePos
       , position = sectorPos
      }
    else
      Nothing



getSectorRow : Int -> Person -> Float -> List Sector
getSectorRow rowNum person fov =
  let
      length = (toFloat rowNum) * sectorSize
      width = (cos fov) * length
      cols = List.range -1 <| ((width / sectorSize) * 2.1 |> round)+1
  in
      List.filterMap (makeSector rowNum person fov) cols


getSectors : Person -> List Sector
getSectors person =
  let
    (x, _, z) = toTuple person.position
    -- Get initial sector position
    (sx, sy) = (x - (fmod x sectorSize), z - (fmod z sectorSize))
    rows = List.range 0 12
  in
    List.concatMap (\r -> getSectorRow r person (degrees 45.0)) rows

view : Mat4 -> Person -> Model -> List Entity
view perspective camera model =
  case model of
    [tex, tex2, tex3, hmap] ->
      List.map
        (\sector ->
          (entityWith
                  [ Settings.cullFace Settings.back
                  , DepthTest.less { write = True
                                   , near = 0
                                   , far = 1
                  }]
                  vertexShader
                  fragmentShader
                  sectorBlock  { grass=tex
                               , drygrass=tex2
                               , cliff=tex3
                               , heightmap=hmap
                               , texPos=Math.Vector2.vec2 (first sector.texturePos) (second sector.texturePos)
                               , sectorPos=Math.Vector2.vec2 (first sector.position) (second sector.position)
                               , perspective=perspective
                               , model=modelMatrix camera}))
        (getSectors camera)

    _ -> []


getTexPos : { a | position : Vec3 } -> Vec2
getTexPos person =
  let
    (x, _, z) = toTuple person.position
  in
    Math.Vector2.vec2 ((toFloat (floor x)) + 0.0)
                      ((toFloat (floor z)) + 0.0)

-- VIEW

modelMatrix : Person -> Mat4
modelMatrix person =
  let
    (x, y, z) = Math.Vector3.negate person.position |> toTuple
    camera = vec3 ((fmod x sectorSize) - sectorSize) y ((fmod z sectorSize) - sectorSize)
  in
    makeTranslate camera
    |> mul (makeRotate person.rotation (vec3 0 1 0.0))
    |> mul (makeRotate person.lookVert (vec3 1 0 0.0))


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

sectorBlock = triangles (List.concatMap (makeTile sectorSize) <| List.range 0 <| (sectorSize*sectorSize)-1)

-- Required external resources

textureNames : List String
textureNames = ["highland.jpg", "highland.jpg", "highland.jpg", "heightmap.png"]

loadTextures =
  List.map
    (\t -> Texture.loadWith 
              { defaultOptions
                | magnify = linear
                , minify = linearMipmapNearest } <| "texture/" ++ t)
    textureNames
  |> Task.sequence
  |> Task.map TexturesLoaded
  |> Debug.log "texture loading: "
  
-- Shaders
vertexShader : Shader { position:Vec3, coord:Vec3 }
                      { u | perspective:Mat4
                      , model:Mat4
                      , heightmap:Texture
                      , texPos:Vec2
                      , sectorPos: Vec2 }
                      { vcoord:Vec2, dist: Float
                      , normal: Vec3
                      , hmapPos:Vec2 }
vertexShader = [glsl|

precision mediump float;

attribute vec3 position;
attribute vec3 coord;

uniform mat4 perspective;
uniform mat4 model;
uniform vec2 sectorPos;
varying vec2 vcoord;
uniform vec2 texPos;
varying float dist;
varying vec3 normal;
varying vec2 hmapPos;
uniform sampler2D heightmap;

float height(vec2 pos) {
    vec4 texel = texture2D(heightmap, pos);
    return texel.r * 64.0;
}

void main () {
  vec2 texelPos = (position.xz + texPos) / 512.0;

  float vHeight = height(texelPos);
  vec2 pos = position.xz + sectorPos;
  vec4 outputPos = perspective * model * vec4(pos.x, vHeight, pos.y, 1.0);
  vcoord = coord.xy;
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

  hmapPos = texelPos;
}

|]


fragmentShader : Shader {} { u | grass:Texture, drygrass:Texture, cliff:Texture, heightmap:Texture} { vcoord:Vec2, hmapPos: Vec2, dist: Float, normal: Vec3 }
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D grass;
uniform sampler2D drygrass;
uniform sampler2D cliff;
uniform sampler2D heightmap;
varying vec2 vcoord;
varying vec3 normal;
varying vec2 hmapPos;
varying float dist;

void main () {
  //vec4 colVal = vec4(texture2D(heightmap, hmapPos).gba, 1.0);

  //vec4 base = vec4(0.0, 0.0, 0.0, 1.0);
  //vec4 colVal = mix(base, texture2D(grass, vcoord), col.g);
  //colVal = mix(colVal, texture2D(drygrass, vcoord), col.b);
  //colVal = mix(colVal, texture2D(cliff, vcoord), col.a);

  // Get the base colour from the textures
  // Apply the horizon blend
  vec4 colVal = texture2D(grass, hmapPos);
  vec4 horizon = vec4(0.7, 0.7, 0.9, 1.0);

  vec3 surfaceToLight = normalize(vec3(-0.2, -0.1, 0.1));

  float lightValue = 0.1 + dot(normal, surfaceToLight);


  colVal = colVal * vec4(lightValue, lightValue, lightValue, 1.0);

  gl_FragColor = colVal;
 
}

|]
