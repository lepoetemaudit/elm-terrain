module Skybox where

import Task

import WebGL exposing (..)
import Effects exposing (Effects)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (vec3, Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4, mul)

import Types exposing (Person)

type Action = TexturesLoaded (Maybe (List Texture))

textures : List String
textures = ["lf", "up", "ft", "bk", "rt", "dn"]

{--
loadTextures : Effects.Effects Action
loadTextures =
  List.map
    (\t -> loadTextureWithFilter Linear <| "texture/" ++ t ++ ".jpg")
    textureNames
  |> Task.sequence
  |> Task.toMaybe
  |> Task.map TexturesLoaded
  |> Effects.task --}


getTextures : Effects.Effects Action
getTextures =
  (List.map (\t -> loadTextureWithFilter
                         Linear
                         ("texture/miramar_" ++ t ++ ".jpeg"))
     textures)
  |> Task.sequence
  |> Task.toMaybe
  |> Task.map TexturesLoaded
  |> Effects.task





cube : List (Drawable Vertex)
cube =
  let
    rft = vec3  1  1  1   -- right, front, top
    lft = vec3 -1  1  1   -- left,  front, top
    lbt = vec3 -1 -1  1
    rbt = vec3  1 -1  1
    rbb = vec3  1 -1 -1
    rfb = vec3  1  1 -1
    lfb = vec3 -1  1 -1
    lbb = vec3 -1 -1 -1

  in List.map Triangle
      [ face rft rfb rbb rbt   -- right
      , face rfb rft lft lfb   -- front (ignore)
      , face lft rft rbt lbt   -- top (actual front)
      , face rfb lfb lbb rbb   -- bottom
      , face lfb lft lbt lbb   -- left (actual right)
      , face rbt rbb lbb lbt   -- back
      ]


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (Vertex, Vertex, Vertex)
face a b c d =
  let
    vertex position coord =
        Vertex position coord
  in
    [ (vertex a (vec2 0 0), vertex b (vec2 1.0 0), vertex c (vec2 1.0 1.0))
    , (vertex c (vec2 1.0 1.0), vertex d (vec2 0 1.0), vertex a (vec2 0 0))
    ]


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }

type alias Model = List Texture

init : Model
init = []

initEffects : List (Effects Action)
initEffects = [getTextures]

makeSkybox : Mat4 -> Model -> List Renderable
makeSkybox perspective textures =
  List.map2
    (\tex tris ->
      (renderWithConfig
              []
              vertexShader
              fragmentShader
              tris
              { facetex = tex, perspective = perspective } ) )
    textures cube

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    TexturesLoaded (Just textures) -> (textures, Effects.none)
    -- TODO - capture higher up and throw warning
    TexturesLoaded Nothing -> (model, Effects.none)


-- TODO - this should return a model matrix, not a perspective one
perspective : (Int,Int) -> Person -> Mat4
perspective (w,h) person =
  (Mat4.makePerspective 45 (toFloat w / toFloat h) 0.10 255)
  `mul` Mat4.makeRotate person.lookVert (vec3 1 0 0.0)
  `mul` Mat4.makeRotate person.rotation (vec3 0 1 0.0)


-- Shaders

vertexShader : Shader { attr | position:Vec3, coord: Vec2 }
                      {u | facetex: Texture, perspective: Mat4} { texcoord: Vec2}
vertexShader = [glsl|
precision mediump float;

attribute vec3 position;
attribute vec2 coord;
varying vec2 texcoord;
uniform mat4 perspective;
void main () {
    texcoord = vec2(coord.x, 1.0-coord.y);
    gl_Position = perspective * vec4(position * 145.0, 1.0);
}
|]


fragmentShader : Shader {} {u | facetex: Texture} { texcoord: Vec2 }
fragmentShader = [glsl|
precision mediump float;
uniform sampler2D facetex;
varying vec2 texcoord;

void main () {
    gl_FragColor = texture2D(facetex, texcoord);
}
|]
