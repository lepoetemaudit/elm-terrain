module Skybox exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4, mul)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Task
import Types exposing (Person)
import WebGL exposing (Entity, Shader, entity, entityWith)
import WebGL.Settings as Settings
import WebGL.Texture exposing (..)


type Action
    = TexturesLoaded (List Texture)


textureNames : List String
textureNames =
    [ "lf", "up", "ft", "bk", "rt", "dn" ]


loadTextures : Task.Task Error Action
loadTextures =
    List.map
        (\t ->
            loadWith
                { defaultOptions
                    | horizontalWrap = clampToEdge
                    , verticalWrap = clampToEdge
                }
                ("texture/miramar_" ++ t ++ ".jpeg")
        )
        textureNames
        |> Task.sequence
        |> Task.map TexturesLoaded
        |> Debug.log "skybox textures"


cube =
    let
        rft =
            vec3 1 1 1

        -- right, front, top
        lft =
            vec3 -1 1 1

        -- left,  front, top
        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
    List.map WebGL.triangles
        [ face rft rfb rbb rbt -- right
        , face rfb rft lft lfb -- front (ignore)
        , face lft rft rbt lbt -- top (actual front)
        , face rfb lfb lbb rbb -- bottom
        , face lfb lft lbt lbb -- left (actual right)
        , face rbt rbb lbb lbt -- back
        ]


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face a b c d =
    let
        vertex position coord =
            Vertex position coord
    in
    [ ( vertex a (vec2 0 0), vertex b (vec2 1.0 0), vertex c (vec2 1.0 1.0) )
    , ( vertex c (vec2 1.0 1.0), vertex d (vec2 0 1.0), vertex a (vec2 0 0) )
    ]


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


type alias Model =
    List Texture


init : Model
init =
    []


update : Action -> a -> ( List Texture, Cmd msg )
update msg model =
    case msg of
        TexturesLoaded tex ->
            ( tex, Cmd.none )


makeSkybox : Mat4 -> Person -> Model -> List Entity
makeSkybox perspective person textures =
    List.map2
        (\tex tris ->
            entityWith [ Settings.cullFace Settings.back ]
                vertexShader
                fragmentShader
                tris
                { facetex = tex
                , perspective = perspective
                , model = modelMatrix person
                }
        )
        textures
        cube


modelMatrix : Person -> Mat4
modelMatrix person =
    -- ORDERING ??
    Mat4.makeRotate person.rotation (vec3 0 1 0.0)
        |> mul (Mat4.makeRotate person.lookVert (vec3 1 0 0.0))



-- Shaders


vertexShader : Shader { attr | position : Vec3, coord : Vec2 } { u | facetex : Texture, perspective : Mat4, model : Mat4 } { texcoord : Vec2 }
vertexShader =
    [glsl|
precision mediump float;

attribute vec3 position;
attribute vec2 coord;
varying vec2 texcoord;
uniform mat4 perspective;
uniform mat4 model;
void main () {
    texcoord = vec2(coord.x, 1.0-coord.y);
    gl_Position = perspective * model * vec4(position * 100.0, 1.0);
}
|]


fragmentShader : Shader {} { u | facetex : Texture } { texcoord : Vec2 }
fragmentShader =
    [glsl|
precision mediump float;
uniform sampler2D facetex;
varying vec2 texcoord;

void main () {
    gl_FragColor = texture2D(facetex, texcoord);
}
|]
