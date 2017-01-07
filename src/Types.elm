module Types exposing (..)

import Math.Vector3 exposing (Vec3)
import Array exposing (Array)

type alias ProgramFlags =
    { terrainHeightMap : Array Float }

type alias Person =
    { position : Vec3
    , velocity : Vec3
    , rotation : Float
    , lookVert : Float
    }